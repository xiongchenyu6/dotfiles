"""Download public closed Binance hourly bars for research, without credentials."""
import argparse
from concurrent.futures import ThreadPoolExecutor
from datetime import datetime,timezone
import json
from pathlib import Path
import time
import requests
import urllib.parse
import sys
sys.path.insert(0,str(Path(__file__).resolve().parents[1]/'strategies'))
import strategy_record as sr


def main():
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument('directory',type=Path)
    p.add_argument('--start',default='2023-12-15T00:00:00+00:00')
    p.add_argument('--end',required=True)
    p.add_argument('--assets',nargs='+',default=list(sr.ASSETS))
    a=p.parse_args()
    start,end=(datetime.fromisoformat(v.replace('Z','+00:00')) for v in (a.start,a.end))
    if any(v.tzinfo is None for v in (start,end)) or not start<end or end>datetime.now(timezone.utc):
        p.error('Require ordered UTC boundaries ending in the past')
    if any(not s.isalnum() or not s.isupper() for s in a.assets): p.error('Invalid symbols')
    a.directory.mkdir(parents=True,exist_ok=True)
    def download(asset):
        path=a.directory/f'{asset}.json'
        if path.exists():
            cached=json.loads(path.read_text())
            if cached and cached[0][0]==int(start.timestamp()*1000) and cached[-1][0]==int(end.timestamp()*1000)-3600000:
                print(asset,len(cached),'cached',flush=True)
                return
        session=requests.Session()
        cursor=int(start.timestamp()*1000);stop=int(end.timestamp()*1000);rows=[]
        while cursor<stop:
            params=urllib.parse.urlencode(dict(symbol=asset+'USDT',interval='1h',startTime=cursor,endTime=stop-1,limit=1000))
            for attempt in range(4):
                try:
                    r=session.get('https://data-api.binance.vision/api/v3/klines?'+params,timeout=30)
                    r.raise_for_status()
                    batch=r.json()
                    if not isinstance(batch,list): raise ValueError('Invalid public OHLC response')
                    break
                except Exception:
                    if attempt==3: raise
                    time.sleep(2**attempt)
            if not batch: break
            rows.extend([int(k[0]),*[float(v) for v in k[1:5]]] for k in batch if int(k[6])<stop)
            nxt=int(batch[-1][0])+3600000
            if nxt<=cursor: raise ValueError('Nonadvancing public data')
            cursor=nxt;time.sleep(.08)
        session.close()
        tmp=path.with_suffix('.json.tmp');tmp.write_text(json.dumps(rows));tmp.replace(path)
        print(asset,len(rows),flush=True)
    with ThreadPoolExecutor(max_workers=3) as pool:
        list(pool.map(download,a.assets))
    (a.directory/'source.json').write_text(json.dumps({'source':'Binance public hourly OHLC',
        'start':start.isoformat(),'end_exclusive':end.isoformat(),'assets':a.assets,
        'downloaded_at':datetime.now(timezone.utc).isoformat()},indent=2)+'\n')
    return 0

if __name__=='__main__': raise SystemExit(main())
