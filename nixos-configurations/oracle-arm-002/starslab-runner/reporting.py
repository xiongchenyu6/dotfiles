"""Upload normalized display data only; no raw exchange response or credentials."""

from datetime import datetime, timezone
import json
import urllib.request
import ssl
from urllib.parse import urlparse
from .config import read_private_json
from .fills import fee_value
import re


def rpc(base, function, body):
    parsed = urlparse(base)
    if parsed.scheme!='https' or not parsed.hostname or parsed.username or parsed.password:
        raise ValueError('Reporting and signal endpoints must use HTTPS')
    request = urllib.request.Request(base.rstrip('/')+'/rpc/'+function,
        data=json.dumps(body,allow_nan=False).encode(),headers={'Content-Type':'application/json',
            'User-Agent':'StarslabRunner/0.1 (+https://github.com/stars-labs/quant)'},method='POST')
    # Do not follow redirects carrying a reporting token to another origin.
    class NoRedirect(urllib.request.HTTPRedirectHandler):
        def redirect_request(self, req, fp, code, msg, headers, newurl):
            return None
    import certifi
    context = ssl.create_default_context(cafile=certifi.where())
    with urllib.request.build_opener(NoRedirect,urllib.request.HTTPSHandler(context=context)).open(request,timeout=20) as response:
        raw = response.read(262145)
        if len(raw)>262144:
            raise ValueError('Oversized service response')
        return json.loads(raw)


def report(store, config, prices, status):
    positions = []
    equity = store.cash()
    for row in store.holdings():
        qty = row['quantity']
        if qty<=1e-12:
            continue
        price = prices[row['asset']]
        equity += qty*price
        realized = row['proceeds']-(row['bought']-qty)*row['cost']/row['bought']
        positions.append({'strategy':row['kind'],'asset':row['asset'],'quantity':qty,
            'price_usdt':price,'cost_usdt':row['cost'],'realized_pnl_usdt':realized})
    fills, total_fee = [], 0.0
    for row in store.db.execute("SELECT * FROM orders WHERE status='done' ORDER BY finished_at DESC"):
        fee, rate = fee_value(row)
        total_fee += fee
        if row['amount'] and len(fills)<50:
            fills.append({'client_id':row['client_id'],'strategy':row['kind'],'asset':row['asset'],
                'side':row['side'],'quantity':row['amount'],'quote_usdt':row['cost'],
                'fee_usdt':fee,'fee_rate':rate,'finished_at':row['finished_at']})
    funded = store.db.execute('SELECT coalesce(sum(trend+dca),0) FROM funding').fetchone()[0]
    now = datetime.now(timezone.utc)
    return {'version':1,'sequence':store.next_sequence(),'observed_at':now.isoformat(),
        'status':status,'venue':config['venue'],'environment':config['mode'],
        'cash_usdt':max(0,store.cash()),'equity_usdt':max(0,equity),'funded_usdt':funded,
        'trend_available_usdt':store.budget('trend',now.strftime('%Y-%m-01')),
        'dca_available_usdt':store.budget('dca',now.strftime('%Y-%m-01')),
        'fees_usdt':total_fee,'positions':positions,'fills':fills}


def upload(path, data):
    settings = read_private_json(path)
    if not isinstance(settings,dict) or set(settings)!= {'api_base','upload_token'} or not isinstance(
        settings['upload_token'],str) or not re.fullmatch(r'[a-f0-9]{64}',settings['upload_token']):
        raise ValueError('Invalid display configuration')
    return rpc(settings['api_base'],'upload_runner_report',
        {'upload_token':settings['upload_token'],'report':data})
