"""Serialize executable replacement against running owner CLI processes."""

from contextlib import contextmanager
import fcntl
import os
from pathlib import Path


@contextmanager
def installation_guard(directory=None, exclusive=False):
    directory=Path(directory) if directory is not None else Path.home()/'.local/state/starslab-runner'
    directory.mkdir(parents=True,exist_ok=True,mode=0o700)
    if directory.is_symlink() or directory.stat().st_uid!=os.getuid():
        raise RuntimeError('Installation lock directory must belong to this OS user')
    directory.chmod(0o700)
    fd=os.open(directory/'installation.lock',os.O_CREAT|os.O_RDWR|os.O_NOFOLLOW,0o600)
    handle=os.fdopen(fd,'a')
    try:
        os.fchmod(fd,0o600)
        try:
            fcntl.flock(handle,(fcntl.LOCK_EX if exclusive else fcntl.LOCK_SH)|fcntl.LOCK_NB)
        except BlockingIOError:
            raise RuntimeError('Runner installation is busy; stop execution before upgrading') from None
        yield
    finally:
        handle.close()


def main(argv=None):
    from contextlib import ExitStack
    import sys
    with ExitStack() as stack:
        try:
            stack.enter_context(installation_guard())
        except RuntimeError:
            print('Runner installation is busy; retry after the upgrade completes.',file=sys.stderr)
            return 1
        from .cli import main as command
        return command(argv)
