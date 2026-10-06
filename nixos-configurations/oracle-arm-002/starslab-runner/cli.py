"""Local configuration, funding and execution. No hosted trading control."""

import argparse
import copy
from datetime import datetime, timezone
import getpass
import json
import math
import os
from pathlib import Path
import sys
import time

from .config import DEFAULT, credentials, private_json, read_private_json, validate
from .engine import tick
from .exchange import HTX
from .journal import Journal
from .reporting import report, rpc, upload
from .signals import validate_snapshot, timestamp


def load(home):
    return validate(read_private_json(home/'config.json'))


def exchange(home, config):
    import ccxt
    options = {'enableRateLimit':True,'timeout':20000}
    if config['mode']=='live':
        secret = credentials(home/config['credentials_file'])
        options.update(apiKey=secret['HTX_API_KEY'],secret=secret['HTX_API_SECRET'])
    if config['proxy_url']:
        key = 'socksProxy' if config['proxy_url'].startswith('socks') else 'httpsProxy'
        options[key] = config['proxy_url']
    ex = ccxt.htx(options)
    ex.load_markets()
    return HTX(ex,config['mode'],config['account_uid'],config['spot_account_id'])


def journal(home, config):
    from .account_lock import acquire
    lock = acquire(config)
    try:
        store = Journal(home/f"{config['venue']}-{config['mode']}.sqlite")
    except BaseException:
        if lock is not None:
            lock.close()
        raise
    store.account_lock = lock
    try:
        store.bind(json.dumps([config['venue'],config['mode'],config['account_uid'],config['spot_account_id']]))
        return store
    except BaseException:
        store.close()
        raise


def configure_live(home):
    config = load(home)
    if config['mode']=='live':
        raise ValueError('Live mode is already configured')
    print('Use a dedicated HTX spot account with Read and Trade permissions, without withdrawal permission.')
    key = getpass.getpass('HTX access key (stored only on this machine): ')
    secret = getpass.getpass('HTX secret key (stored only on this machine): ')
    if not key or not secret or '\n' in key or '\n' in secret:
        raise ValueError('Invalid credentials')
    import ccxt
    options = {'apiKey':key,'secret':secret,'enableRateLimit':True,'timeout':20000}
    if config['proxy_url']:
        options['socksProxy' if config['proxy_url'].startswith('socks') else 'httpsProxy'] = config['proxy_url']
    ex = ccxt.htx(options)
    identity = ex.spot_private_get_v2_user_uid()
    accounts = ex.spot_private_get_v1_account_accounts()
    if identity.get('code')!=200 or accounts.get('status')!='ok':
        raise ValueError('Unable to verify HTX account')
    spot = [str(row['id']) for row in accounts.get('data',[]) if row['type']=='spot' and row['state']=='working']
    if len(spot)!=1:
        raise ValueError('A single working spot account is required')
    print(f"Verified HTX UID {identity['data']}, spot account {spot[0]}.")
    print(f"Monthly confirmed funding limits: trend {config['monthly_trend_usdt']:g}, BTC DCA {config['monthly_dca_usdt']:g} USDT.")
    if input('Type ENABLE LIVE to authorize spot orders from this machine: ')!='ENABLE LIVE':
        raise ValueError('Live authorization was not provided')
    path = home/config['credentials_file']
    path.parent.mkdir(parents=True,exist_ok=True,mode=0o700)
    fd = os.open(path,os.O_WRONLY|os.O_CREAT|os.O_TRUNC,0o600)
    with os.fdopen(fd,'w') as handle:
        os.fchmod(handle.fileno(),0o600)
        handle.write('HTX_API_KEY='+key+'\nHTX_API_SECRET='+secret+'\n')
        handle.flush()
        os.fsync(handle.fileno())
    config.update(mode='live',allow_live=True,account_uid=str(identity['data']),spot_account_id=spot[0])
    private_json(home/'config.json',validate(config))
    print('Live configuration saved locally. No orders were submitted. Confirm funding before running.')


def run(home, config, once=False):
    store = journal(home,config)
    try:
        venue = exchange(home,config)
        while True:
            status, prices = 'paused', {}
            decisions = []
            stage = 'reconciliation'
            try:
                # Recovery precedes feed fetching, so a service outage cannot hide
                # an already-submitted order from the local journal.
                if not venue.reconcile(store):
                    status = 'pending'
                stage = 'signal_feed'
                snapshot = rpc(config['api_base'],'runner_signals',{})
                held = {row['asset'] for row in store.holdings() if row['quantity']>1e-12}
                prices = validate_snapshot(snapshot,set(config['assets']) | held)['prices']
                stage = 'execution'
                status = tick(store,venue,config,snapshot,decisions=decisions)
            except Exception as cause:
                # Exchange exception messages can include signed URLs and keys.
                decisions.append({'strategy':'account','asset':None,'reason':stage+'_failed','error_class':type(cause).__name__})
                print(f'Execution paused ({type(cause).__name__}); pending intents are preserved.',flush=True)
            private_json(home/'decisions.json',{'observed_at':datetime.now(timezone.utc).isoformat(),
                'status':status,'decisions':decisions})
            if prices:
                price_as_of = min(timestamp(row['last_ts']) for row in snapshot['assets'] if row['asset'] in prices).isoformat()
                display = report(store,config,prices,status,decisions,price_as_of)
                private_json(home/'status.json',display)
                if config['display_file']:
                    try:
                        upload(home/config['display_file'],display)
                    except Exception as cause:
                        print(f'Report upload unavailable ({type(cause).__name__}); local execution is independent.',flush=True)
            print(f"{datetime.now(timezone.utc).isoformat()} {config['venue']}/{config['mode']} {status}, pending={len(store.pending())}",flush=True)
            if once:
                return 0 if status=='healthy' else 1
            time.sleep(60)
    finally:
        store.close()


def main(argv=None):
    parser = argparse.ArgumentParser(description='Account-owner execution and private display reporting')
    parser.add_argument('--home',type=Path,default=Path.home()/'.config/starslab-runner')
    commands = parser.add_subparsers(dest='command',required=True)
    commands.add_parser('init')
    commands.add_parser('configure-live')
    attach = commands.add_parser('connect-display')
    attach.add_argument('file',type=Path)
    fund = commands.add_parser('fund')
    fund.add_argument('--month',default=datetime.now(timezone.utc).strftime('%Y-%m-01'))
    fund.add_argument('--trend',type=float)
    fund.add_argument('--dca',type=float)
    execute = commands.add_parser('run')
    execute.add_argument('--once',action='store_true')
    commands.add_parser('status')
    commands.add_parser('doctor')
    commands.add_parser('decisions')
    commands.add_parser('history')
    carry = commands.add_parser('carry-dca')
    carry.add_argument('--reference',required=True)
    carry.add_argument('--from-month',required=True)
    carry.add_argument('--amount',type=float,required=True)
    deposit = commands.add_parser('deposit')
    deposit.add_argument('--reference',required=True)
    deposit.add_argument('--trend',type=float,required=True)
    deposit.add_argument('--dca',type=float,required=True)
    adjust = commands.add_parser('cash-flow')
    adjust.add_argument('--reference',required=True)
    adjust.add_argument('--trend',type=float,required=True)
    adjust.add_argument('--dca',type=float,required=True)
    save = commands.add_parser('backup')
    save.add_argument('destination',type=Path)
    verify = commands.add_parser('verify-backup')
    verify.add_argument('file',type=Path)
    recover = commands.add_parser('restore')
    recover.add_argument('file',type=Path)
    recover.add_argument('--confirm-original-stopped',action='store_true')
    args = parser.parse_args(argv)
    home = args.home.expanduser().resolve()
    try:
        if args.command=='init':
            if (home/'config.json').exists():
                validate(read_private_json(home/'config.json'))
                print('Existing local configuration verified; preserved.')
            else:
                private_json(home/'config.json',copy.deepcopy(DEFAULT))
                print(f'Simulation configuration created at {home}/config.json. No live orders are enabled.')
            return 0
        if args.command=='decisions':
            print(json.dumps(read_private_json(home/'decisions.json'),indent=2))
            return 0
        if args.command=='doctor':
            from .recovery import diagnose
            checks = diagnose(home)
            print(json.dumps(checks,indent=2))
            return int(any(row['status']!='ok' for row in checks))
        if args.command=='verify-backup':
            from .recovery import verify_backup
            print(json.dumps(verify_backup(args.file)))
            return 0
        config = load(home)
        if args.command=='history':
            import sqlite3
            from types import SimpleNamespace
            from .history import history
            path = home/f"{config['venue']}-{config['mode']}.sqlite"
            with sqlite3.connect(path.as_uri()+'?mode=ro',uri=True) as db:
                db.row_factory = sqlite3.Row
                print(json.dumps(history(SimpleNamespace(db=db)),indent=2,allow_nan=False))
            return 0
        if args.command=='restore':
            from .recovery import restore
            restore(args.file,home,config,args.confirm_original_stopped)
            print('Journal restored. No service was started or order submitted. Reconcile exchange state before resuming.')
            return 0
        if args.command=='backup':
            from .recovery import backup
            backup(home/f"{config['venue']}-{config['mode']}.sqlite",args.destination)
            print('Consistent journal backup created with checksum. Credentials and configuration require separate private backup.')
            return 0
        if args.command=='configure-live':
            configure_live(home)
            return 0
        if args.command=='connect-display':
            # Browsers may save downloads as 644: tighten before reading and copying.
            args.file.chmod(0o600)
            data = read_private_json(args.file)
            import re
            if not isinstance(data,dict) or set(data)!= {'api_base','upload_token'} or not isinstance(
                data['upload_token'],str) or not re.fullmatch(r'[a-f0-9]{64}',data['upload_token']):
                raise ValueError('Invalid display file')
            from urllib.parse import urlparse
            if urlparse(data['api_base']).scheme!='https':
                raise ValueError('Reporting requires HTTPS')
            private_json(home/'display.json',data)
            config['display_file']='display.json'
            private_json(home/'config.json',config)
            print('Upload-only display connection saved locally. Execution settings are unchanged.')
            return 0
        if args.command=='run':
            return run(home,config,args.once)
        if args.command=='status' and (home/'status.json').exists():
            snapshot = read_private_json(home/'status.json')
            if snapshot.get('environment')==config['mode'] and snapshot.get('venue')==config['venue']:
                snapshot['snapshot_age_seconds'] = max(0,(datetime.now(timezone.utc)-datetime.fromisoformat(snapshot['observed_at'])).total_seconds())
                print(json.dumps(snapshot,indent=2,allow_nan=False))
                return 0
        store = journal(home,config)
        try:
            if args.command=='carry-dca':
                if config['mode']=='live':
                    venue = exchange(home,config)
                    if not venue.reconcile(store):
                        raise ValueError('Reconcile pending orders before carrying allocation')
                    venue.check(store)
                store.carry_dca(args.reference,args.from_month,args.amount)
                print('Unused DCA allocation carried to the current month; cash and contributions are unchanged.')
            elif args.command=='deposit':
                month = datetime.now(timezone.utc).strftime('%Y-%m-01')
                prior = store.db.execute('SELECT 1 FROM cash_flows WHERE reference=?',(args.reference,)).fetchone()
                if config['mode']=='live' and not prior:
                    venue = exchange(home,config)
                    if not venue.reconcile(store):
                        raise ValueError('Reconcile pending orders before confirming a deposit')
                    venue.check(store)
                    if float(venue.balance()['free'].get('USDT') or 0)+1e-8<store.cash()+args.trend+args.dca:
                        raise ValueError('Confirmed additional deposit is absent')
                store.deposit(args.reference,month,args.trend,args.dca,
                    config['monthly_trend_usdt'],config['monthly_dca_usdt'])
                print('Additional funding confirmed locally; no transfer or order submitted.')
            elif args.command=='cash-flow':
                month = datetime.now(timezone.utc).strftime('%Y-%m-01')
                prior = store.db.execute('SELECT 1 FROM cash_flows WHERE reference=?',(args.reference,)).fetchone()
                if config['mode']=='live' and not prior:
                    venue = exchange(home,config)
                    if not venue.reconcile(store):
                        raise ValueError('Reconcile pending orders before adjusting cash')
                    expected_cash = store.cash()+args.trend+args.dca
                    if not math.isfinite(expected_cash) or not math.isclose(
                        float(venue.balance()['total'].get('USDT') or 0),expected_cash,rel_tol=0,abs_tol=.01):
                        raise ValueError('Exchange cash does not match the confirmed movement')
                    class AdjustedAccount:
                        def cash(self): return expected_cash
                        def holdings(self): return store.holdings()
                    venue.check(AdjustedAccount())
                store.cash_flow(args.reference,month,args.trend,args.dca)
                print('Cash movement confirmed locally. No exchange transfer or order was submitted.')
            elif args.command=='fund':
                if args.month!=datetime.now(timezone.utc).strftime('%Y-%m-01'):
                    raise ValueError('Only the current UTC month can be funded')
                trend = config['monthly_trend_usdt'] if args.trend is None else args.trend
                dca = config['monthly_dca_usdt'] if args.dca is None else args.dca
                if not 0<=trend<=config['monthly_trend_usdt'] or not 0<=dca<=config['monthly_dca_usdt']:
                    raise ValueError('Funding exceeds local monthly limits')
                prior = store.db.execute('SELECT trend,dca FROM funding WHERE month=?',(args.month,)).fetchone()
                if not prior:
                    added = store.db.execute('SELECT coalesce(sum(trend_delta),0),coalesce(sum(dca_delta),0) FROM cash_flows WHERE month=? AND cash_delta>0',(args.month,)).fetchone()
                    if trend+added[0]>config['monthly_trend_usdt']+1e-8 or dca+added[1]>config['monthly_dca_usdt']+1e-8:
                        raise ValueError('Combined deposits exceed monthly funding caps')
                if config['mode']=='live' and not prior:
                    venue = exchange(home,config)
                    if not venue.reconcile(store):
                        raise ValueError('Reconcile the pending order before confirming funding')
                    venue.check(store)
                    if float(venue.balance()['free'].get('USDT') or 0)+1e-8<store.cash()+trend+dca:
                        raise ValueError('Confirmed additional funding is not present in the spot wallet')
                store.fund(args.month,trend,dca)
                print('Funding confirmed locally; no transfer or order was submitted.')
            else:
                result = {'venue':config['venue'],'mode':config['mode'],'cash_usdt':store.cash(),
                    'pending_orders':len(store.pending()),'positions':store.holdings()}
                print(json.dumps(result,indent=2,allow_nan=False))
        finally:
            store.close()
        return 0
    except Exception as cause:
        print(f'Runner stopped safely ({type(cause).__name__}). Check local settings, permissions and account state.',file=sys.stderr)
        return 1


if __name__=='__main__':
    raise SystemExit(main())
