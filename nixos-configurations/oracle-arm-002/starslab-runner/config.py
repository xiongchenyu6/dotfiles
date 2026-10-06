"""Private local configuration. Platform reports never contain these settings."""

import json
import socket
import getpass
import hashlib
import math
from pathlib import Path
import re
from urllib.parse import urlparse

def machine_identity():
    path = Path('/etc/machine-id')
    value = path.read_text().strip() if path.exists() else socket.gethostname()
    return hashlib.sha256(value.encode()).hexdigest()


ASSETS = ['BTC','ETH','SOL','XRP','DOGE','ADA','AVAX','SUI','NEAR','UNI','ZEC','PEPE','WLD']
DEFAULT = {'venue':'htx','mode':'dry_run','allow_live':False,'assets':ASSETS,
    'trend':True,'dca':True,'monthly_trend_usdt':100,'monthly_dca_usdt':100,
    'order_usdt':20,'api_base':'https://api.panda.qzz.io',
    'account_uid':None,'spot_account_id':None,'credentials_file':'credentials.env',
    'display_file':None,'proxy_url':None,
    'account_lock_owner':{'machine_id':machine_identity(),'host':socket.gethostname(),'ssh':getpass.getuser()+'@'+socket.gethostname(),
        'user':getpass.getuser(),'home':str(Path.home()/'.config/starslab-runner'),
        'executable':'/run/current-system/sw/bin/starslab-runner' if Path('/run/current-system/sw/bin/starslab-runner').exists() else str(Path.home()/'.local/bin/starslab-runner')}}


def private_json(path, data):
    path = Path(path)
    path.parent.mkdir(parents=True,exist_ok=True,mode=0o700)
    temporary = path.with_suffix(path.suffix+'.tmp')
    import os
    descriptor = os.open(temporary,os.O_WRONLY|os.O_CREAT|os.O_TRUNC,0o600)
    os.fchmod(descriptor,0o600)
    with os.fdopen(descriptor,'w') as handle:
        handle.write(json.dumps(data,indent=2,allow_nan=False)+'\n')
        handle.flush()
        os.fsync(handle.fileno())
    temporary.replace(path)


def read_private_json(path):
    path = Path(path)
    if path.stat().st_mode & 0o077:
        raise ValueError('Configuration file must have private permissions (chmod 600)')
    return json.loads(path.read_text())


def validate(data):
    if not isinstance(data,dict) or set(data)!=set(DEFAULT):
        raise ValueError('Invalid local configuration fields')
    if data['venue']!='htx' or data['mode'] not in ('dry_run','live'):
        raise ValueError('Only HTX spot is supported by this runner')
    for key in ['allow_live','trend','dca']:
        if type(data[key]) is not bool:
            raise ValueError('Invalid local enable setting')
    if data['mode']=='live' and (not data['allow_live'] or
        not all(isinstance(data[key],str) and re.fullmatch(r'[0-9]+',data[key])
                for key in ['account_uid','spot_account_id'])):
        raise ValueError('Live mode requires explicit local authorization and account identity')
    owner = data['account_lock_owner']
    if not isinstance(owner,dict) or set(owner)!={'machine_id','host','ssh','user','home','executable'}:
        raise ValueError('Invalid account lock owner')
    if not isinstance(owner['machine_id'],str) or not re.fullmatch(r'[a-f0-9]{64}',owner['machine_id']):
        raise ValueError('Invalid lock owner machine identity')
    for key in ('host','ssh','user'):
        if not isinstance(owner[key],str) or not re.fullmatch(r'[A-Za-z0-9_][A-Za-z0-9_.@-]{0,200}',owner[key]):
            raise ValueError('Invalid owner SSH setting')
    if any(not isinstance(owner[key],str) or not Path(owner[key]).is_absolute() for key in ('home','executable')):
        raise ValueError('Owner state directory must be absolute')
    assets = data['assets']
    if not isinstance(assets,list) or not assets or len(assets)!=len(set(assets)) or not set(assets).issubset(ASSETS):
        raise ValueError('Invalid locally selected universe')
    if data['dca'] and 'BTC' not in assets:
        raise ValueError('BTC must be selected for DCA')
    for key in ['monthly_trend_usdt','monthly_dca_usdt','order_usdt']:
        value = data[key]
        if type(value) not in (int,float) or not math.isfinite(value) or not 0<value<=1000000:
            raise ValueError('Invalid local spending limit')
    if data['order_usdt']>data['monthly_trend_usdt']:
        raise ValueError('Per-order limit exceeds monthly trend allocation')
    base = urlparse(data['api_base'])
    if base.scheme!='https' or not base.hostname or base.username or base.password or base.query or base.fragment:
        raise ValueError('Signal URL must use HTTPS without embedded credentials')
    for key in ['credentials_file','display_file']:
        if key=='display_file' and data[key] is None:
            continue
        if not isinstance(data[key],str) or not data[key]:
            raise ValueError('Invalid local configuration path')
    if data['proxy_url'] is not None:
        proxy = urlparse(data['proxy_url'])
        if proxy.scheme not in ('http','https','socks5','socks5h') or not proxy.hostname:
            raise ValueError('Invalid locally configured proxy')
    return data


def credentials(path):
    path = Path(path)
    if path.stat().st_mode & 0o077:
        raise ValueError('Exchange credentials must have private permissions (chmod 600)')
    values = {}
    for line in path.read_text().splitlines():
        if not line.strip() or line.startswith('#'):
            continue
        key, separator, value = line.partition('=')
        if not separator or key not in ('HTX_API_KEY','HTX_API_SECRET') or key in values or not value.strip():
            raise ValueError('Invalid exchange credential file')
        values[key] = value.strip()
    if set(values)!= {'HTX_API_KEY','HTX_API_SECRET'}:
        raise ValueError('Missing local exchange credentials')
    return values
