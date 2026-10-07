"""Frozen prospective hourly OHLC evaluation. No exchange accounts or order APIs."""
import argparse
from datetime import datetime,timezone,timedelta
import hashlib
import json
from pathlib import Path
import subprocess
import sys
import fcntl
import pandas as pd
sys.path.insert(0,str(Path(__file__).resolve().parents[1]/'strategies'))
from trend_replay import Rule,replay


def digest(value):
    return hashlib.sha256(json.dumps(value,sort_keys=True,separators=(',',':')).encode()).hexdigest()


def write(path,value):
    tmp=path.with_suffix('.tmp');tmp.write_text(json.dumps(value,indent=2)+'\n');tmp.replace(path)


def evaluate(home,plan,now,download):
    if plan['implementation_sha256']!=hashlib.sha256(Path(replay.__code__.co_filename).read_bytes()).hexdigest():
        raise ValueError('Registered replay implementation changed')
    home=Path(home);home.mkdir(parents=True,exist_ok=True)
    frozen=home/'plan.json'
    if frozen.exists():
        if json.loads(frozen.read_text())!=plan: raise ValueError('Registered shadow plan changed')
    else:
        if datetime.fromisoformat(plan['start'])<=now:
            raise ValueError('First registration must precede the shadow start')
        write(frozen,plan)
    start=datetime.fromisoformat(plan['start'])
    end=now.astimezone(timezone.utc).replace(minute=0,second=0,microsecond=0)
    if end<=start:
        result={'status':'waiting','start':plan['start'],'plan_hash':digest(plan),
                'observed_at':now.isoformat(),'results':[]}
        write(home/'latest.json',result);return result
    checkpoints=home/'checkpoints';checkpoints.mkdir(exist_ok=True)
    path=checkpoints/(end.strftime('%Y%m%dT%H')+'.json')
    if path.exists():
        result=json.loads(path.read_text())
        if result['plan_hash']!=digest(plan): raise ValueError('Checkpoint plan changed')
        write(home/'latest.json',result);return result
    previous=json.loads((home/'latest.json').read_text()) if (home/'latest.json').exists() else {}
    warm=start-timedelta(hours=max(r['entry'] for r in plan['rules'])+1)
    download(home/'bars',warm,end,plan['assets'])
    frames={};hashes={}
    for asset in plan['assets']:
        rows=json.loads((home/'bars'/f'{asset}.json').read_text())
        if previous.get('bar_hashes'):
            cutoff=int(datetime.fromisoformat(previous['end_exclusive']).timestamp()*1000)
            if digest([r for r in rows if r[0]<cutoff])!=previous['bar_hashes'][asset]:
                raise ValueError('Previously observed market data changed')
        hashes[asset]=digest(rows)
        frame=pd.DataFrame(rows,columns=['time','open','high','low','close'])
        frame.index=pd.to_datetime(frame.pop('time'),unit='ms',utc=True);frames[asset]=frame
    results=[replay(frames,Rule(**rule),start,end,fee=c['fee'],slippage=c['slippage'],
                    monthly=plan['monthly'],order_cap=plan['order_cap'],minimum=plan['minimum'])
             for rule in plan['rules'] for c in plan['costs']]
    result={'status':'observing','start':plan['start'],'end_exclusive':end.isoformat(),
            'observed_at':now.isoformat(),'plan_hash':digest(plan),'bar_hashes':hashes,
            'evaluation':'prospective hourly OHLC replay; simulated fills, no live orders',
            'results':results}
    write(path,result);write(home/'latest.json',result);return result


def main():
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument('--home',type=Path,required=True);p.add_argument('--plan',type=Path,required=True)
    a=p.parse_args();a.home.mkdir(parents=True,exist_ok=True)
    with (a.home/'shadow.lock').open('a') as lock:
        fcntl.flock(lock,fcntl.LOCK_EX|fcntl.LOCK_NB)
        def download(directory,start,end,assets):
            subprocess.run([sys.executable,str(Path(__file__).with_name('download_research_bars.py')),
                str(directory),'--start',start.isoformat(),'--end',end.isoformat(),'--assets',*assets],check=True)
        r=evaluate(a.home,json.loads(a.plan.read_text()),datetime.now(timezone.utc),download)
        print(r['status'],r.get('end_exclusive',r['start']),r['plan_hash'],flush=True)
    return 0

if __name__=='__main__': raise SystemExit(main())
