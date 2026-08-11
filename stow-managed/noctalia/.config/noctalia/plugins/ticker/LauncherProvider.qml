import QtQuick
import Quickshell
import Quickshell.Io
import qs.Commons

Item {
  id: root

  property var pluginApi: null

  // Provider metadata
  property string name: "Price Ticker"
  property var launcher: null
  property bool handleSearch: false
  property string supportedLayouts: "list"
  property bool supportsAutoPaste: false
  property bool ignoreDensity: false

  // 默认关注列表;查别的直接 >cc <名字> / >st <代码>
  property var cryptoWatchlist: [
    { id: "bitcoin", symbol: "BTC" },
    { id: "ethereum", symbol: "ETH" },
    { id: "solana", symbol: "SOL" },
    { id: "ripple", symbol: "XRP" },
    { id: "dogecoin", symbol: "DOGE" }
  ]
  property var stockWatchlist: ["AAPL", "TSLA", "NVDA", "MSFT", "GOOGL"]

  property string mode: ""            // "cc" | "st"
  property string query: ""
  property string fetchedKey: ""      // mode+query 已取数据的标识
  property var rows: []               // [{title, sub, url}]
  property bool fetching: false
  property bool fetchFailed: false

  // curl 直连失败自动退回本机 sing-box 代理端口
  property string fetchHelper: "fetch() { curl -m 10 -sf \"$@\" || curl -m 10 -sfx socks5h://127.0.0.1:20170 \"$@\"; }; "

  function init() {
  }

  Timer {
    id: debounce
    interval: 500
    repeat: false
    onTriggered: root.runFetch()
  }

  Process {
    id: fetcher
    stdout: StdioCollector {}
    stderr: StdioCollector {}
    onExited: exitCode => root.onFetchDone(exitCode)
  }

  function currentKey() {
    return mode + ":" + query;
  }

  function runFetch() {
    if (fetcher.running) {
      debounce.restart();
      return;
    }
    fetching = true;
    fetchFailed = false;
    fetchedKey = currentKey();

    let script;
    if (mode === "cc") {
      if (query === "") {
        const ids = cryptoWatchlist.map(c => c.id).join(",");
        script = "fetch 'https://api.coingecko.com/api/v3/simple/price?ids=" + ids + "&vs_currencies=usd&include_24hr_change=true'";
      } else {
        // 先 search 拿 id,再取价;jq 串起来一次进程搞定
        script = "ids=$(fetch 'https://api.coingecko.com/api/v3/search?query=" + encodeURIComponent(query) + "' | jq -r '[.coins[:5][].id] | join(\",\")');"
          + "[ -n \"$ids\" ] && fetch \"https://api.coingecko.com/api/v3/simple/price?ids=$ids&vs_currencies=usd&include_24hr_change=true\"";
      }
    } else {
      const symbols = query === "" ? stockWatchlist : [query.toUpperCase()];
      // 每个代码一行 "SYM|price|prevClose"
      script = "for s in " + symbols.join(" ") + "; do "
        + "fetch -A Mozilla \"https://query1.finance.yahoo.com/v8/finance/chart/$s?interval=1d&range=1d\" "
        + "| jq -r '.chart.result[0].meta | \"\\(.symbol)|\\(.regularMarketPrice)|\\(.chartPreviousClose)\"'; done";
    }
    fetcher.command = ["sh", "-c", fetchHelper + script];
    fetcher.running = true;
  }

  function onFetchDone(exitCode) {
    fetching = false;
    const text = (fetcher.stdout.text || "").trim();
    if (exitCode !== 0 && text === "") {
      rows = [];
      fetchFailed = true;
    } else {
      try {
        rows = (fetchedKey.startsWith("cc") ? parseCrypto(text) : parseStocks(text));
        fetchFailed = rows.length === 0;
      } catch (e) {
        Logger.e("Ticker", "Parse error:", e);
        rows = [];
        fetchFailed = true;
      }
    }
    if (currentKey() !== fetchedKey) {
      debounce.restart();
    }
    if (launcher && launcher.activeProvider == root) {
      launcher.updateResults();
    }
  }

  function parseCrypto(text) {
    const data = JSON.parse(text || "{}");
    const symbolFor = {};
    cryptoWatchlist.forEach(c => symbolFor[c.id] = c.symbol);
    // 关注列表按固定顺序,搜索结果按返回顺序
    const ids = Object.keys(data);
    ids.sort((a, b) => {
      const wa = cryptoWatchlist.findIndex(c => c.id === a);
      const wb = cryptoWatchlist.findIndex(c => c.id === b);
      return (wa === -1 ? 99 : wa) - (wb === -1 ? 99 : wb);
    });
    return ids.map(id => {
      const price = data[id].usd;
      const chg = data[id].usd_24h_change;
      return {
        title: (symbolFor[id] || id) + "  $" + formatPrice(price),
        sub: "24h " + formatChange(chg),
        url: "https://www.coingecko.com/en/coins/" + id
      };
    });
  }

  function parseStocks(text) {
    return text.split("\n").filter(l => l.includes("|")).map(line => {
      const parts = line.split("|");
      const price = parseFloat(parts[1]);
      const prev = parseFloat(parts[2]);
      const chg = prev > 0 ? ((price - prev) / prev) * 100 : NaN;
      return {
        title: parts[0] + "  $" + formatPrice(price),
        sub: isNaN(chg) ? "" : "today " + formatChange(chg),
        url: "https://finance.yahoo.com/quote/" + parts[0]
      };
    });
  }

  function formatPrice(v) {
    if (isNaN(v)) return "?";
    return v >= 1000 ? v.toLocaleString(Qt.locale("en_US"), 'f', 0)
         : v >= 1 ? v.toFixed(2)
         : v.toPrecision(3);
  }

  function formatChange(chg) {
    if (chg === undefined || chg === null || isNaN(chg)) return "";
    return (chg >= 0 ? "▲ +" : "▼ ") + chg.toFixed(2) + "%";
  }

  function handleCommand(searchText) {
    const m = searchText.trim().match(/^>(\S+)/);
    return m !== null && (m[1] === "cc" || m[1] === "st");
  }

  function commands() {
    return [
      {
        "name": ">cc",
        "description": "Crypto prices (CoinGecko)",
        "icon": "currency-bitcoin",
        "isTablerIcon": true,
        "isImage": false,
        "onActivate": function () { launcher.setSearchText(">cc "); }
      },
      {
        "name": ">st",
        "description": "Stock prices (Yahoo Finance)",
        "icon": "chart-line",
        "isTablerIcon": true,
        "isImage": false,
        "onActivate": function () { launcher.setSearchText(">st "); }
      }
    ];
  }

  function getResults(searchText) {
    const m = searchText.trim().match(/^>(cc|st)\s*(.*)$/);
    if (!m) {
      return [];
    }
    const newMode = m[1];
    const newQuery = m[2].trim();
    if (newMode !== mode || newQuery !== query) {
      mode = newMode;
      query = newQuery;
      debounce.restart();
    }

    const icon = mode === "cc" ? "currency-bitcoin" : "chart-line";
    if (fetching || currentKey() !== fetchedKey) {
      return [hintEntry("Fetching prices…", "", icon)];
    }
    if (fetchFailed) {
      return [hintEntry("Fetch failed", "check network / try again", icon)];
    }
    return rows.map(r => ({
      "name": r.title,
      "description": r.sub,
      "icon": icon,
      "isTablerIcon": true,
      "isImage": false,
      "singleLine": true,
      "provider": root,
      "onActivate": function () {
        Quickshell.execDetached(["xdg-open", r.url]);
        launcher.close();
      }
    }));
  }

  function hintEntry(title, description, icon) {
    return {
      "name": title,
      "description": description,
      "icon": icon,
      "isTablerIcon": true,
      "isImage": false,
      "onActivate": function () {}
    };
  }
}
