"""Owner SSH lock channel. No exchange keys or website connection tokens travel here."""

import selectors
import os
import time
import shlex
import subprocess
import uuid


class RemoteLock:
    def __init__(self, owner, fingerprint):
        command = [owner['executable'],
            '--home',owner['home'],'hold-account-lock','--fingerprint',fingerprint]
        if owner['ssh'].split('@')[0]!=owner['user']:
            command = ['sudo','-n','-H','-u',owner['user'],*command]
        self.buffer = b''
        self.process = subprocess.Popen(['ssh','-T','-o','BatchMode=yes','-o','ConnectTimeout=5',
            '-o','ServerAliveInterval=5','-o','ServerAliveCountMax=1',owner['ssh'],shlex.join(command)],
            stdin=subprocess.PIPE,stdout=subprocess.PIPE,stderr=subprocess.DEVNULL)
        try:
            if self.read(10)!=b'READY\n':
                raise RuntimeError('Owner account lock is unavailable')
        except BaseException:
            self.close()
            raise

    def read(self, timeout):
        deadline = time.monotonic()+timeout
        with selectors.DefaultSelector() as selector:
            selector.register(self.process.stdout,selectors.EVENT_READ)
            while b'\n' not in self.buffer:
                remaining = deadline-time.monotonic()
                if remaining<=0 or not selector.select(remaining):
                    raise RuntimeError('Owner account lock channel timed out')
                chunk = os.read(self.process.stdout.fileno(),1024)
                if not chunk:
                    return b''
                self.buffer += chunk
                if len(self.buffer)>1024:
                    raise RuntimeError('Invalid owner lock response')
        line,self.buffer = self.buffer.split(b'\n',1)
        return line+b'\n'

    def check(self):
        if self.process.poll() is not None:
            raise RuntimeError('Owner account lock channel disconnected')
        nonce = uuid.uuid4().hex.encode()
        try:
            self.process.stdin.write(b'PING '+nonce+b'\n')
            self.process.stdin.flush()
            if self.read(3)!=b'PONG '+nonce+b'\n':
                raise RuntimeError('Owner account lock verification failed')
        except (OSError,ValueError,RuntimeError):
            self.close()
            raise RuntimeError('Owner account lock channel disconnected') from None

    def close(self):
        if self.process.stdin:
            try:
                self.process.stdin.close()
            except OSError:
                pass
        try:
            self.process.wait(timeout=2)
        except subprocess.TimeoutExpired:
            self.process.kill()
            self.process.wait()
        if self.process.stdout:
            self.process.stdout.close()
