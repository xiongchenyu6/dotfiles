"""Explicit local setup and schema upgrades; never submit orders or credit funding."""

from contextlib import contextmanager
import copy
import fcntl
import os
from pathlib import Path
import pwd
import socket
import sys

from .config import DEFAULT, machine_identity, private_json, read_private_json, validate


def local_owner(home, executable=None):
    user = pwd.getpwuid(os.getuid()).pw_name
    host = socket.gethostname()
    return dict(machine_id=machine_identity(), host=host, ssh=f'{user}@{host}',
                user=user, home=str(Path(home).resolve()),
                executable=str(Path(executable or DEFAULT['account_lock_owner']['executable']).absolute()))


def private_home(home):
    home = Path(home).absolute()
    home.mkdir(parents=True, exist_ok=True, mode=0o700)
    if home.is_symlink() or home.stat().st_uid != os.getuid() or home.stat().st_mode & 0o077:
        raise ValueError('Runner home must be private and owned by the current OS user')
    return home


@contextmanager
def stopped(home, config=None):
    """Hold journal locks throughout a config mutation, without opening SQLite."""
    handles = []
    account = None
    try:
        for mode in ('dry_run', 'live'):
            path = home / f'htx-{mode}.sqlite.lock'
            fd = os.open(path, os.O_CREAT | os.O_RDWR | os.O_NOFOLLOW, 0o600)
            handle = os.fdopen(fd, 'a')
            handles.append(handle)
            if os.fstat(fd).st_uid != os.getuid():
                raise ValueError('Journal lock belongs to another OS user')
            os.fchmod(fd, 0o600)
            try:
                fcntl.flock(handle, fcntl.LOCK_EX | fcntl.LOCK_NB)
            except BlockingIOError:
                raise RuntimeError('Stop the original executor before changing configuration') from None
        if config and config['mode'] == 'live':
            from .account_lock import acquire
            account = acquire(config)
        yield
    finally:
        if account:
            account.close()
        for handle in handles:
            handle.close()


def initialize(home, executable=None):
    home = private_home(home)
    with stopped(home):
        path = home / 'config.json'
        if path.exists():
            return validate(read_private_json(path))
        config = copy.deepcopy(DEFAULT)
        config['account_lock_owner'] = local_owner(home, executable)
        private_json(path, validate(config))
        return config


def upgrade(home, *, original_stopped=False, original_owner=None):
    """Migrate legacy configs only on an explicitly identified original executor.

    original_owner must contain the original machine ID, actual OS user and state
    directory, supplied by the operator; copied live configs cannot be adopted.
    """
    home = private_home(home)
    if not original_stopped:
        raise ValueError('Explicit confirmation that the original executor is stopped is required')
    data = read_private_json(home / 'config.json')
    if 'account_lock_owner' in data:
        validate(data)
        owner = data['account_lock_owner']
    else:
        if set(data) != set(DEFAULT) - {'account_lock_owner'}:
            raise ValueError('Unsupported legacy configuration schema')
        if original_owner is None:
            raise ValueError('Provide the original executor ownership; do not adopt copied configurations')
        owner = dict(original_owner)
        data = dict(data, account_lock_owner=owner)
        validate(data)
    if (owner['machine_id'] != machine_identity() or
            owner['user'] != pwd.getpwuid(os.getuid()).pw_name or
            Path(owner['home']).resolve() != home.resolve()):
        raise ValueError('Upgrade must run as the original executor user on its original machine and home')
    with stopped(home, data):
        private_json(home / 'config.json', data)
    return data


def guided_setup(home, *, live=False, configure_live=None, executable=None):
    config = initialize(home, executable)
    if live:
        if config['mode'] == 'live':
            raise ValueError('Live configuration already exists; use status to inspect it')
        if configure_live is None:
            raise ValueError('Live setup requires the local credential and account verification workflow')
        owner = config['account_lock_owner']
        if owner['machine_id'] != machine_identity() or owner['user'] != pwd.getpwuid(os.getuid()).pw_name:
            raise ValueError('Live setup must run on the designated owner machine as its OS user')
        with stopped(Path(home), config):
            configure_live(Path(home))
    return config
