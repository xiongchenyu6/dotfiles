"""Local durable intents; this module has no network or platform DB access."""

from contextlib import contextmanager
from datetime import date, datetime, timezone
import math
import fcntl
import re
from pathlib import Path
import sqlite3


def finite(value):
    number = float(value)
    if not math.isfinite(number):
        raise ValueError("Non-finite amount")
    return number


def initialize_schema(db):
    db.executescript(SCHEMA)


SCHEMA = """
            CREATE TABLE IF NOT EXISTS funding (
                month TEXT PRIMARY KEY, trend REAL NOT NULL, dca REAL NOT NULL
            );
            CREATE TABLE IF NOT EXISTS orders (
                client_id TEXT PRIMARY KEY, action TEXT NOT NULL UNIQUE,
                position TEXT NOT NULL, kind TEXT NOT NULL,
                asset TEXT NOT NULL, side TEXT NOT NULL, requested REAL NOT NULL,
                status TEXT NOT NULL DEFAULT 'pending', exchange_id TEXT,
                quoted_taker_rate REAL, quoted_basic_rate REAL,
                asset_delta REAL, cash_delta REAL, amount REAL, cost REAL,
                created_at TEXT NOT NULL, finished_at TEXT
            );
            CREATE TABLE IF NOT EXISTS equity_snapshots (
                sequence INTEGER PRIMARY KEY, observed_at TEXT NOT NULL,
                price_as_of TEXT NOT NULL, equity REAL NOT NULL,
                cash REAL NOT NULL, net_funding REAL NOT NULL, fees REAL NOT NULL
            );
            CREATE TABLE IF NOT EXISTS cash_flows (
                reference TEXT PRIMARY KEY, month TEXT NOT NULL,
                trend_delta REAL NOT NULL, dca_delta REAL NOT NULL,
                cash_delta REAL NOT NULL, created_at TEXT NOT NULL
            );
            CREATE TABLE IF NOT EXISTS metadata (key TEXT PRIMARY KEY, value TEXT NOT NULL);
"""


class Journal:
    def __init__(self, path):
        path = Path(path)
        self.path = path.resolve()
        path.parent.mkdir(parents=True, exist_ok=True, mode=0o700)
        self.lock = path.with_suffix(path.suffix + '.lock').open('a')
        path.with_suffix(path.suffix + '.lock').chmod(0o600)
        try:
            fcntl.flock(self.lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
        except BlockingIOError:
            self.lock.close()
            raise RuntimeError("Another executor owns this journal") from None
        self.db = sqlite3.connect(path, isolation_level=None)
        path.chmod(0o600)
        self.db.row_factory = sqlite3.Row
        self.db.execute("PRAGMA journal_mode=WAL")
        self.db.execute("PRAGMA synchronous=FULL")
        initialize_schema(self.db)

    @contextmanager
    def transaction(self):
        self.db.execute("BEGIN IMMEDIATE")
        try:
            yield
            self.db.execute("COMMIT")
        except BaseException:
            self.db.execute("ROLLBACK")
            raise

    def close(self):
        self.db.close()
        self.lock.close()
        account_lock = getattr(self,'account_lock',None)
        if account_lock is not None:
            account_lock.close()

    def bind(self, identity):
        with self.transaction():
            prior = self.db.execute("SELECT value FROM metadata WHERE key='account'").fetchone()
            if prior and prior[0] != identity:
                raise ValueError('This journal belongs to a different account or mode')
            self.db.execute("INSERT OR IGNORE INTO metadata VALUES ('account',?)", (identity,))

    def next_sequence(self):
        with self.transaction():
            row = self.db.execute("SELECT value FROM metadata WHERE key='sequence'").fetchone()
            value = int(row[0]) + 1 if row else 1
            self.db.execute("INSERT INTO metadata VALUES ('sequence',?) ON CONFLICT(key) DO UPDATE SET value=excluded.value", (str(value),))
            return value

    def exists(self, action):
        return self.db.execute('SELECT 1 FROM orders WHERE action=?', (action,)).fetchone() is not None

    def fund(self, month, trend, dca, confirmed_at=None):
        parsed = date.fromisoformat(month)
        if parsed.day != 1 or parsed.isoformat() != month:
            raise ValueError("Funding month must start on day one")
        trend, dca = finite(trend), finite(dca)
        if min(trend, dca) < 0 or trend + dca <= 0:
            raise ValueError("Funding must be positive")
        with self.transaction():
            prior = self.db.execute("SELECT trend,dca FROM funding WHERE month=?", (month,)).fetchone()
            if prior:
                if tuple(prior) != (trend, dca):
                    raise ValueError("Funding already confirmed with different amounts")
                return
            self.db.execute("INSERT INTO funding VALUES (?,?,?)", (month, trend, dca))
            if confirmed_at is not None:
                stamp=datetime.fromisoformat(confirmed_at)
                if stamp.tzinfo is None:
                    raise ValueError('Confirmation time needs a timezone')
                self.db.execute("INSERT INTO metadata VALUES (?,?)", ('funded-at:'+month,stamp.astimezone(timezone.utc).isoformat()))

    def reserve(self, client_id, action, position, kind, asset, side, requested,
                quoted_taker_rate=None, quoted_basic_rate=None):
        requested = finite(requested)
        if kind not in ("trend", "dca") or side not in ("buy", "sell") or requested <= 0:
            raise ValueError("Invalid order intent")
        if not isinstance(asset, str) or not re.fullmatch(r'[A-Z0-9]{2,20}', asset):
            raise ValueError("Invalid asset")
        if kind == 'dca' and (asset != 'BTC' or side != 'buy'):
            raise ValueError("DCA only buys BTC")
        if not all(isinstance(v, str) and v and len(v) <= 128
                   for v in (client_id, action, position, asset)):
            raise ValueError("Invalid order identity")
        if (quoted_taker_rate is None) != (quoted_basic_rate is None) or any(
            not 0<=finite(v)<=.003 for v in (quoted_taker_rate,quoted_basic_rate) if v is not None):
            raise ValueError('Invalid quoted account fee')
        with self.transaction():
            if self.db.execute("SELECT 1 FROM orders WHERE action=?", (action,)).fetchone():
                return False
            if self.db.execute("SELECT 1 FROM orders WHERE status='pending'").fetchone():
                raise RuntimeError("Reconcile the pending order before submitting another")
            prior = self.db.execute("SELECT kind,asset FROM orders WHERE position=? LIMIT 1", (position,)).fetchone()
            if prior and tuple(prior) != (kind, asset):
                raise ValueError("Position identity mismatch")
            if side == 'buy':
                month = datetime.now(timezone.utc).strftime('%Y-%m-01')
                if requested > min(self.cash(), self.budget(kind, month)) + 1e-8:
                    raise ValueError("Order exceeds confirmed funding")
            else:
                held = self.db.execute("SELECT coalesce(sum(asset_delta),0) FROM orders WHERE position=? AND status='done'", (position,)).fetchone()[0]
                if requested > held + 1e-12:
                    raise ValueError("Sale exceeds owned position")
            self.db.execute("""INSERT INTO orders
                (client_id,action,position,kind,asset,side,requested,created_at,quoted_taker_rate,quoted_basic_rate)
                VALUES (?,?,?,?,?,?,?,?,?,?)""", (client_id, action, position, kind,
                asset, side, requested, datetime.now(timezone.utc).isoformat(),quoted_taker_rate,quoted_basic_rate))
            return True

    def pending(self):
        return [dict(r) for r in self.db.execute(
            "SELECT * FROM orders WHERE status='pending' ORDER BY created_at")]

    def set_exchange_id(self, client_id, exchange_id):
        if not isinstance(exchange_id, str) or not exchange_id:
            raise ValueError("Invalid exchange order ID")
        with self.transaction():
            row = self.db.execute("SELECT exchange_id FROM orders WHERE client_id=?", (client_id,)).fetchone()
            if row is None or row[0] not in (None, exchange_id):
                raise ValueError("Exchange order identity mismatch")
            self.db.execute("UPDATE orders SET exchange_id=? WHERE client_id=?", (exchange_id, client_id))

    def finish(self, client_id, asset_delta, cash_delta, amount, cost):
        qty, cash, amount, cost = map(finite, (asset_delta, cash_delta, amount, cost))
        with self.transaction():
            row = self.db.execute("SELECT * FROM orders WHERE client_id=?", (client_id,)).fetchone()
            if row is None:
                raise ValueError("Unknown order intent")
            values = (qty, cash, amount, cost)
            if row['status'] == 'done':
                if tuple(row[k] for k in ('asset_delta', 'cash_delta', 'amount', 'cost')) != values:
                    raise ValueError("Conflicting terminal fill")
                return
            if any(values):
                if amount <= 0 or cost <= 0:
                    raise ValueError("Invalid gross fill")
                buy = row['side'] == 'buy'
                base_fee = amount-qty if buy else -qty-amount
                quote_fee = -cash-cost if buy else cost-cash
                if base_fee < -max(1e-12, amount*1e-10) or quote_fee < -max(1e-8, cost*1e-10):
                    raise ValueError("Fill movements disagree")
                if (max(0, base_fee)*cost/amount + max(0, quote_fee))/cost > .003+1e-8:
                    raise ValueError("Fee exceeds safety ceiling")
                spent = -cash if buy else -qty
                if spent > row['requested']+1e-8:
                    raise ValueError("Fill exceeds reserved amount")
                if not buy:
                    held = self.db.execute("SELECT coalesce(sum(asset_delta),0) FROM orders WHERE position=? AND status='done'", (row['position'],)).fetchone()[0]
                    if -qty > held + 1e-12:
                        raise ValueError("Sale fee exceeds owned position")
                elif -cash > self.cash() + 1e-8:
                    raise ValueError("Fill exceeds confirmed account cash")
            self.db.execute("""UPDATE orders SET status='done',asset_delta=?,cash_delta=?,
                amount=?,cost=?,finished_at=? WHERE client_id=?""",
                (*values, datetime.now(timezone.utc).isoformat(), client_id))

    def cash(self):
        credit = self.db.execute("SELECT coalesce(sum(trend+dca),0) FROM funding").fetchone()[0]
        delta = self.db.execute("SELECT coalesce(sum(cash_delta),0) FROM orders WHERE status='done'").fetchone()[0]
        flow = self.db.execute("SELECT coalesce(sum(cash_delta),0) FROM cash_flows").fetchone()[0]
        return credit + delta + flow

    def holdings(self):
        return [dict(r) for r in self.db.execute("""SELECT position,kind,asset,
            sum(asset_delta) AS quantity,
            sum(CASE WHEN side='buy' THEN asset_delta ELSE 0 END) AS bought,
            -sum(CASE WHEN side='buy' THEN cash_delta ELSE 0 END) AS cost,
            sum(CASE WHEN side='sell' THEN cash_delta ELSE 0 END) AS proceeds
            FROM orders WHERE status='done' GROUP BY position,kind,asset""")]

    def budget(self, kind, month):
        if kind not in ('trend', 'dca') or date.fromisoformat(month).day != 1:
            raise ValueError("Invalid budget")
        if kind == 'trend':
            credit = self.db.execute("SELECT coalesce(sum(trend),0) FROM funding WHERE month<=?", (month,)).fetchone()[0]
            delta = self.db.execute("SELECT coalesce(sum(cash_delta),0) FROM orders WHERE kind='trend' AND status='done'").fetchone()[0]
        else:
            credit = self.db.execute("SELECT coalesce(sum(dca),0) FROM funding WHERE month=?", (month,)).fetchone()[0]
            delta = self.db.execute("SELECT coalesce(sum(cash_delta),0) FROM orders WHERE kind='dca' AND status='done' AND substr(created_at,1,7)=?", (month[:7],)).fetchone()[0]
        if kind=='trend':
            adjustment = self.db.execute('SELECT coalesce(sum(trend_delta),0) FROM cash_flows WHERE month<=?',(month,)).fetchone()[0]
        else:
            adjustment = self.db.execute('SELECT coalesce(sum(dca_delta),0) FROM cash_flows WHERE month=?',(month,)).fetchone()[0]
        return max(0.0, credit + delta + adjustment)

    def cash_flow(self, reference, month, trend_delta, dca_delta):
        """Record an owner-confirmed withdrawal or allocation transfer, never an order."""
        if not isinstance(reference,str) or not re.fullmatch(r'[A-Za-z0-9:_-]{1,128}',reference):
            raise ValueError('Invalid cash-flow reference')
        if month!=datetime.now(timezone.utc).strftime('%Y-%m-01'):
            raise ValueError('Only the current month can be adjusted')
        trend_delta, dca_delta = finite(trend_delta), finite(dca_delta)
        total = trend_delta+dca_delta
        if total>1e-8 or (trend_delta==0 and dca_delta==0):
            raise ValueError('Only withdrawals or allocation transfers are supported')
        with self.transaction():
            prior = self.db.execute('SELECT month,trend_delta,dca_delta FROM cash_flows WHERE reference=?',(reference,)).fetchone()
            if prior:
                if tuple(prior)!=(month,trend_delta,dca_delta):
                    raise ValueError('Cash-flow reference already has different amounts')
                return
            if self.pending():
                raise ValueError('Reconcile pending orders before changing cash allocation')
            if -trend_delta>self.budget('trend',month)+1e-8 or -dca_delta>self.budget('dca',month)+1e-8:
                raise ValueError('Adjustment exceeds available strategy allocation')
            if -total>self.cash()+1e-8:
                raise ValueError('Withdrawal exceeds tracked cash')
            self.db.execute('INSERT INTO cash_flows VALUES (?,?,?,?,?,?)',
                (reference,month,trend_delta,dca_delta,total,datetime.now(timezone.utc).isoformat()))

    def net_funding(self):
        funding = self.db.execute('SELECT coalesce(sum(trend+dca),0) FROM funding').fetchone()[0]
        movements = self.db.execute('SELECT coalesce(sum(cash_delta),0) FROM cash_flows').fetchone()[0]
        return funding+movements

    def deposit(self, reference, month, trend, dca, trend_limit, dca_limit):
        """Confirm incremental funding within the owner's monthly deposit caps."""
        if not isinstance(reference,str) or not re.fullmatch(r'[A-Za-z0-9:_-]{1,128}',reference):
            raise ValueError('Invalid deposit reference')
        if month!=datetime.now(timezone.utc).strftime('%Y-%m-01'):
            raise ValueError('Only the current month can receive funding')
        trend,dca,trend_limit,dca_limit = map(finite,(trend,dca,trend_limit,dca_limit))
        if min(trend,dca,trend_limit,dca_limit)<0 or trend+dca<=0:
            raise ValueError('Invalid confirmed deposit')
        with self.transaction():
            prior = self.db.execute('SELECT month,trend_delta,dca_delta FROM cash_flows WHERE reference=?',(reference,)).fetchone()
            if prior:
                if tuple(prior)!=(month,trend,dca):
                    raise ValueError('Deposit reference already has different amounts')
                return
            if self.pending():
                raise ValueError('Reconcile pending orders before confirming a deposit')
            funded = self.db.execute('SELECT coalesce(sum(trend),0),coalesce(sum(dca),0) FROM funding WHERE month=?',(month,)).fetchone()
            added = self.db.execute('SELECT coalesce(sum(trend_delta),0),coalesce(sum(dca_delta),0) FROM cash_flows WHERE month=? AND cash_delta>0',(month,)).fetchone()
            if funded[0]+added[0]+trend>trend_limit+1e-8 or funded[1]+added[1]+dca>dca_limit+1e-8:
                raise ValueError('Deposit exceeds monthly funding caps')
            self.db.execute('INSERT INTO cash_flows VALUES (?,?,?,?,?,?)',
                (reference,month,trend,dca,trend+dca,datetime.now(timezone.utc).isoformat()))

    def carry_dca(self, reference, source_month, amount):
        """Explicitly move unused prior-month DCA budget to the current month."""
        current = datetime.now(timezone.utc).strftime('%Y-%m-01')
        if not isinstance(reference,str) or not re.fullmatch(r'[A-Za-z0-9:_-]{1,120}',reference):
            raise ValueError('Invalid carry reference')
        parsed = date.fromisoformat(source_month)
        if parsed.day!=1 or parsed.isoformat()!=source_month or source_month>=current:
            raise ValueError('Carry source must be an earlier UTC month')
        amount = finite(amount)
        if amount<=0:
            raise ValueError('Carry amount must be positive')
        outgoing, incoming = reference+':out', reference+':in'
        expected = [(outgoing,source_month,0,-amount,0),(incoming,current,0,amount,0)]
        with self.transaction():
            prior = [tuple(r) for r in self.db.execute('SELECT reference,month,trend_delta,dca_delta,cash_delta FROM cash_flows WHERE reference IN (?,?) ORDER BY reference DESC',(outgoing,incoming))]
            if prior:
                if prior!=expected:
                    raise ValueError('Carry reference already has different amounts')
                return
            if self.pending():
                raise ValueError('Reconcile pending orders before carrying allocation')
            if amount>min(self.budget('dca',source_month),self.cash())+1e-8:
                raise ValueError('Carry exceeds unused DCA allocation or tracked cash')
            stamp = datetime.now(timezone.utc).isoformat()
            self.db.executemany('INSERT INTO cash_flows VALUES (?,?,?,?,?,?)',
                [(*row,stamp) for row in expected])
