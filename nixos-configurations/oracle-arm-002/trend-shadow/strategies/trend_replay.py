"""Research-only, cash-constrained spot replay. Never imports an exchange adapter."""
from dataclasses import dataclass
import math
import pandas as pd


@dataclass(frozen=True)
class Rule:
    name: str
    entry: int = 168
    exit: int = 72
    buffer: float = 0


def replay(frames, rule, start, end, *, fee=.002, slippage=.0005,
           monthly=100., order_cap=20., minimum=5.):
    """Signals at prior close; fills at next open; base-buy/quote-sell fees.

    Contributions are simulated assumptions, never credits to a live journal.
    Unitisation prices contributions at the current open before trading.
    Drawdown uses hourly close NAV, including unrealised positions and cash.
    """
    if not frames or not isinstance(rule.entry,int) or not isinstance(rule.exit,int):
        raise ValueError('Missing frames or invalid lookbacks')
    if rule.entry<=0 or rule.exit<=0 or rule.exit>rule.entry:
        raise ValueError('Invalid lookbacks')
    if any(not math.isfinite(x) or x<0 for x in (fee,slippage,monthly,order_cap,minimum,rule.buffer)):
        raise ValueError('Invalid replay parameter')
    if fee>=1 or slippage>=1 or monthly<=0 or order_cap<=0 or minimum<=0:
        raise ValueError('Invalid replay parameter')
    start,end=pd.Timestamp(start),pd.Timestamp(end)
    if (start.tzinfo is None or end.tzinfo is None or start>=end
            or start!=start.floor('h') or end!=end.floor('h')):
        raise ValueError('Require ordered timezone-aware boundaries')
    index=pd.date_range(start,end,freq='h',inclusive='left')
    if not len(index): raise ValueError('Empty replay window')
    prepared={}
    for asset,frame in frames.items():
        d=frame.copy()
        if not isinstance(d.index,pd.DatetimeIndex) or d.index.tz is None:
            raise ValueError('Require timezone-aware bars')
        if d.index.has_duplicates or not d.index.is_monotonic_increasing:
            raise ValueError('Duplicate or unordered bars')
        values=d[['open','high','low','close']]
        if not values.map(lambda x: math.isfinite(float(x)) and float(x)>0).all().all():
            raise ValueError('Invalid OHLC prices')
        if ((d.high<d[['open','close']].max(axis=1)) | (d.low>d[['open','close']].min(axis=1)) | (d.low>d.high)).any():
            raise ValueError('Inconsistent OHLC bars')
        # Include the previous close in the signal's own bar, but exclude it
        # from its channel. All signal columns are shifted before execution.
        p=pd.DataFrame({'open':d.open,'close':d.close,
            'signal_close':d.close.shift(1),
            'entry':d.high.shift(2).rolling(rule.entry).max(),
            'exit':d.low.shift(2).rolling(rule.exit).min()})
        warm=pd.date_range(start-pd.Timedelta(hours=rule.entry+1),end,freq='h',inclusive='left')
        if not warm.isin(d.index).all():
            raise ValueError(f'Missing hourly bars or warmup for {asset}')
        p=p.reindex(index)
        if p.isna().any().any(): raise ValueError(f'Incomplete data for {asset}')
        prepared[asset]=p.to_numpy()
    cash=monthly;funded=monthly;units=monthly;peak=1.;drawdown=0.
    held={};target={};processed=set();fills=[];fee_total=0.;last_month=index[0].strftime('%Y-%m')
    history=[];target_counter=0
    for i,ts in enumerate(index):
        opens={a:p[i,0] for a,p in prepared.items()}
        before=cash+sum(v['quantity']*opens[v['asset']] for v in held.values())
        nav=before/units
        if ts.strftime('%Y-%m')!=last_month:
            units+=monthly/nav;cash+=monthly;funded+=monthly
            last_month=ts.strftime('%Y-%m')
        for a,p in prepared.items():
            _,_,previous,entry,exit=p[i]
            if a in target:
                if previous<exit: del target[a]
            elif previous>entry*(1+rule.buffer):
                target_counter+=1;target[a]=target_counter
        for position in list(held):
            h=held[position]
            a=h['asset']
            if target.get(a)!=h['target']:
                price=opens[a]*(1-slippage);gross=h['quantity']*price
                if gross<minimum: continue
                proceeds=gross*(1-fee);cash+=proceeds;fee_total+=gross*fee
                fills.append({'asset':a,'side':'sell','time':ts.isoformat(),
                              'price':price,'cash':proceeds,'pnl':proceeds-h['cost']})
                del held[position]
        for a in prepared:
            tid=target.get(a)
            if tid is None or tid in processed: continue
            allocation=min(order_cap,cash)
            # Match the live adapter's conservative 0.3% fee reservation.
            cost=allocation/1.003
            if cost<minimum: continue
            price=opens[a]*(1+slippage);gross_qty=cost/price
            quantity=gross_qty*(1-fee);cash-=cost;fee_total+=cost*fee
            held[tid]={'asset':a,'target':tid,'quantity':quantity,'cost':cost};processed.add(tid)
            fills.append({'asset':a,'side':'buy','time':ts.isoformat(),'price':price,'cash':-cost})
        equity=cash+sum(v['quantity']*prepared[v['asset']][i,1] for v in held.values())
        nav=equity/units;peak=max(peak,nav);drawdown=min(drawdown,nav/peak-1)
        if cash < -1e-8: raise AssertionError('Replay spent unavailable cash')
        if ts.hour==23 or i==len(index)-1:
            history.append({'time':(ts+pd.Timedelta(hours=1)).isoformat(),'nav':nav,'equity':equity,'cash':cash,'funded':funded})
    liquidation=cash+sum(v['quantity']*prepared[v['asset']][-1,1]*(1-slippage)*(1-fee) for v in held.values())
    sells=[f for f in fills if f['side']=='sell']
    return {'rule':rule.name,'entry_lookback':rule.entry,'exit_lookback':rule.exit,'buffer':rule.buffer,
            'start':index[0].isoformat(),'end_exclusive':end.isoformat(),'assets':list(prepared),
            'fee_per_side':fee,'slippage_per_side':slippage,'monthly_usdt':monthly,'order_cap_usdt':order_cap,
            'funded_usdt':funded,'equity_usdt':equity,'cash_usdt':cash,'pnl_usdt':equity-funded,
            'time_weighted_return':nav-1,'hourly_close_max_drawdown':drawdown,
            'liquidation_equity_usdt':liquidation,'paid_fees_usdt':fee_total,
            'buys':len(fills)-len(sells),'sells':len(sells),'open_positions':len(held),
            'closed_win_rate':sum(f['pnl']>0 for f in sells)/len(sells) if sells else None,
            'history':history,'fills':fills,
            'limitations':['Historical Binance OHLC, not HTX historical spreads or fills',
                'Uniform 5 USDT minimum; fractional quantities, no exchange rounding or dust',
                'Monthly deposits are simulated, not confirmed live funding; DCA excluded',
                'Hourly close drawdown misses intrahour excursions',
                'Fixed current universe has historical availability/survivorship limitations',
                'Retrospective diagnostics do not constitute prospective validation']}
