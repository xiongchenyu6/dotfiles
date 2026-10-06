"""One account executor per OS user, independent of journal location."""

import fcntl
import hashlib
import json
import os
from pathlib import Path


def acquire_local(config, directory=None):
    if config['mode']!='live':
        return None
    directory = Path(directory) if directory else Path.home()/'.local/state/starslab-runner/account-locks'
    directory.mkdir(parents=True,exist_ok=True,mode=0o700)
    if directory.is_symlink() or directory.stat().st_uid!=os.getuid() or directory.stat().st_mode & 0o077:
        raise ValueError('Account lock directory must be private and owner-controlled')
    identity = json.dumps([config['venue'],config['account_uid'],config['spot_account_id']]).encode()
    path = directory/(hashlib.sha256(identity).hexdigest()+'.lock')
    fd = os.open(path,os.O_CREAT|os.O_RDWR|os.O_NOFOLLOW,0o600)
    handle = os.fdopen(fd,'a')
    try:
        if os.fstat(fd).st_uid!=os.getuid():
            raise ValueError('Account lock belongs to another owner')
        os.fchmod(fd,0o600)
        fcntl.flock(handle,fcntl.LOCK_EX|fcntl.LOCK_NB)
        return handle
    except BaseException:
        handle.close()
        raise RuntimeError('Another executor owns this account on this machine') from None


def fingerprint(config):
    return hashlib.sha256(json.dumps([config['venue'],config['account_uid'],config['spot_account_id']]).encode()).hexdigest()


def acquire(config, directory=None):
    from .config import machine_identity
    owner = config.get('account_lock_owner')
    if config['mode']!='live' or directory is not None or not owner or owner['machine_id']==machine_identity():
        return acquire_local(config,directory)
    from .remote_lock import RemoteLock
    return RemoteLock(owner,fingerprint(config))


def check(store):
    lock = getattr(store,'account_lock',None)
    if hasattr(lock,'check'):
        lock.check()
