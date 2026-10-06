"""Offline diagnosis and consistent journal backups; never submit orders."""

from datetime import datetime, timezone
import hashlib
import json
from pathlib import Path
import sqlite3

from .config import credentials, read_private_json, validate


def diagnose(home):
    checks = []
    def check(name, action, remedy):
        try:
            detail = action()
            checks.append({'check':name,'status':'ok','detail':detail})
        except Exception:
            # Never include exception messages: they may contain credentials.
            checks.append({'check':name,'status':'attention','remedy':remedy})
    config = None
    def configuration():
        nonlocal config
        config = validate(read_private_json(home/'config.json'))
        return 'Local configuration validated'
    check('configuration',configuration,'Run init or correct the private config.json file.')
    if config is None:
        return checks
    if config['mode']=='live':
        check('credentials',lambda: 'Private credentials present' if credentials(home/config['credentials_file']) else None,
              'Provide a valid credential file with permission 600; do not paste keys into chat.')
    path = home/f"{config['venue']}-{config['mode']}.sqlite"
    def ledger():
        with sqlite3.connect(path.as_uri()+'?mode=ro',uri=True) as db:
            if db.execute('PRAGMA integrity_check').fetchone()[0]!='ok':
                raise ValueError()
            expected = json.dumps([config['venue'],config['mode'],config['account_uid'],config['spot_account_id']])
            identity = db.execute("SELECT value FROM metadata WHERE key='account'").fetchone()
            if identity is None or identity[0]!=expected:
                raise ValueError()
            pending = db.execute("SELECT count(*) FROM orders WHERE status='pending'").fetchone()[0]
            if pending:
                checks.append({'check':'pending_orders','status':'attention',
                    'remedy':'Keep new orders paused. Reconcile existing client IDs with HTX; never delete or resubmit intents.',
                    'count':pending})
            return 'Journal integrity and account identity verified'
    check('journal',ledger,'Initialize the journal or restore a verified backup while execution is stopped.')
    def heartbeat():
        data = read_private_json(home/'status.json')
        age = (datetime.now(timezone.utc)-datetime.fromisoformat(data['observed_at'])).total_seconds()
        if not 0<=age<=300 or data.get('status')!='healthy':
            raise ValueError()
        return 'Healthy report observed within five minutes'
    check('heartbeat',heartbeat,'Check the owner runner service and its logs. A stale report alone does not prove execution stopped.')
    return checks


def backup(source, destination):
    """SQLite backup includes committed WAL records even while execution is active."""
    source, destination = Path(source).resolve(), Path(destination).resolve()
    if destination.exists() or destination.with_suffix(destination.suffix+'.sha256').exists():
        raise ValueError('Backup destination already exists')
    destination.parent.mkdir(parents=True,exist_ok=True,mode=0o700)
    destination.touch(mode=0o600,exist_ok=False)
    try:
        with sqlite3.connect(source.as_uri()+'?mode=ro',uri=True) as original:
            with sqlite3.connect(destination) as target:
                original.backup(target)
                if target.execute('PRAGMA integrity_check').fetchone()[0]!='ok':
                    raise ValueError('Invalid backup')
        digest = hashlib.sha256(destination.read_bytes()).hexdigest()
        manifest = destination.with_suffix(destination.suffix+'.sha256')
        with manifest.open('x') as handle:
            manifest.chmod(0o600)
            handle.write(digest+'\n')
    except BaseException:
        destination.unlink(missing_ok=True)
        raise
    return digest


def verify_backup(path):
    path = Path(path).resolve()
    expected = path.with_suffix(path.suffix+'.sha256').read_text().strip()
    if hashlib.sha256(path.read_bytes()).hexdigest()!=expected:
        raise ValueError('Backup checksum mismatch')
    with sqlite3.connect(path.as_uri()+'?mode=ro',uri=True) as db:
        if db.execute('PRAGMA integrity_check').fetchone()[0]!='ok':
            raise ValueError('Backup integrity failed')
        identity = db.execute("SELECT value FROM metadata WHERE key='account'").fetchone()
        if not identity:
            raise ValueError('Backup has no account identity')
        counts = {name:db.execute(f'SELECT count(*) FROM {name}').fetchone()[0]
                  for name in ('orders','funding')}
    return {'integrity':'ok',**counts}


def restore(path, home, config, original_stopped=False):
    """Restore into an empty account directory; leave service activation to owner."""
    if not original_stopped:
        raise ValueError('Confirm the original executor is stopped on every host')
    path, home = Path(path).resolve(), Path(home).resolve()
    verify_backup(path)
    expected = json.dumps([config['venue'],config['mode'],config['account_uid'],config['spot_account_id']])
    with sqlite3.connect(path.as_uri()+'?mode=ro',uri=True) as source:
        identity = source.execute("SELECT value FROM metadata WHERE key='account'").fetchone()
        if not identity or identity[0]!=expected:
            raise ValueError('Backup account differs from destination configuration')
        target = home/f"{config['venue']}-{config['mode']}.sqlite"
        if any(Path(str(target)+suffix).exists() for suffix in ('','-wal','-shm')):
            raise ValueError('Destination journal already exists; never overwrite execution state')
        from .setup import stopped,private_home
        home=private_home(home)
        with stopped(home,config):
            # Stale lock files are normal after installation; held locks refuse
            # restore. Exclusive creation still prevents replacing any database.
            target.touch(mode=0o600,exist_ok=False)
            with sqlite3.connect(target) as restored:
                source.backup(restored)
                from .journal import initialize_schema
                initialize_schema(restored)
                if restored.execute('PRAGMA integrity_check').fetchone()[0]!='ok':
                    raise ValueError('Restored journal integrity failed')
                identity=restored.execute("SELECT value FROM metadata WHERE key='account'").fetchone()
                if not identity or identity[0]!=expected:
                    raise ValueError('Restored account identity differs')
    return verify_backup(path)
