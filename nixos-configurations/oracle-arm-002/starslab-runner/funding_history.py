"""Owner-confirmed funding and allocation history; unknown legacy times remain unknown."""


def funding_history(store, limit=100):
    if type(limit) is not int or not 1<=limit<=100:
        raise ValueError('Invalid funding history limit')
    rows=[]
    for row in store.db.execute('SELECT month,trend,dca FROM funding'):
        stamp=store.db.execute('SELECT value FROM metadata WHERE key=?',('funded-at:'+row['month'],)).fetchone()
        rows.append({'reference':'fund:'+row['month'],'month':row['month'],'kind':'deposit',
            'confirmed_at':stamp[0] if stamp else None,'trend_delta_usdt':row['trend'],
            'dca_delta_usdt':row['dca'],'cash_delta_usdt':row['trend']+row['dca']})
    for row in store.db.execute('SELECT * FROM cash_flows'):
        kind='deposit' if row['cash_delta']>0 else 'withdrawal' if row['cash_delta']<0 else 'carry' if row['trend_delta']+row['dca_delta'] else 'allocation'
        rows.append({'reference':row['reference'],'month':row['month'],'kind':kind,
            'confirmed_at':row['created_at'],'trend_delta_usdt':row['trend_delta'],
            'dca_delta_usdt':row['dca_delta'],'cash_delta_usdt':row['cash_delta']})
    rows.sort(key=lambda row:(row['confirmed_at'] is not None,row['confirmed_at'] or row['month'],row['reference']),reverse=True)
    return rows[:limit]
