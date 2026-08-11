--!nonstrict
-- /cc — 币价(CoinGecko)。空查询显示关注列表;/cc pepe 搜任意币。
-- CoinGecko 要求带 UA,且 res.ok 在 4xx 时仍为 true,必须查 status。
-- 同一时刻只放一个请求,打字过程中的中间查询直接合并到最新。

local WATCHLIST = {
  { id = "bitcoin", symbol = "BTC" },
  { id = "ethereum", symbol = "ETH" },
  { id = "solana", symbol = "SOL" },
  { id = "ripple", symbol = "XRP" },
  { id = "dogecoin", symbol = "DOGE" },
}

local API = "https://api.coingecko.com/api/v3"
local UA = "User-Agent: noctalia-ticker/1.0 (github.com/xiongchenyu6/dotfiles)"

local inflight = false
local wanted = ""
-- 60s 结果缓存:重开 launcher / 回退输入时不再打 API,也缓解限流
local CACHE_TTL = 60
local cache = {} -- query -> { rows = …, at = os.time() }

local function ok(res)
  return res.ok and (res.status == nil or res.status < 400)
end

local function formatPrice(v)
  if v == nil then
    return "?"
  end
  if v >= 1000 then
    return string.format("%.0f", v)
  elseif v >= 1 then
    return string.format("%.2f", v)
  end
  return string.format("%.6g", v)
end

local function formatChange(chg)
  if chg == nil then
    return ""
  end
  local arrow = chg >= 0 and "▲ +" or "▼ "
  return "24h " .. arrow .. string.format("%.2f", chg) .. "%"
end

local function failRow(title, subtitle)
  return { { id = "", title = title, subtitle = subtitle or "", glyph = "alert-circle" } }
end

local function httpFailRows(res)
  if res.status == 429 then
    return failRow("Rate limited by CoinGecko", "wait ~1 minute and try again")
  end
  return failRow("Fetch failed (" .. tostring(res.status) .. ")")
end

local function priceRows(data, order, titles)
  local rows = {}
  for _, id in order do
    local info = data[id]
    if info then
      table.insert(rows, {
        id = "https://www.coingecko.com/en/coins/" .. id,
        title = (titles and titles[id] or id:upper()) .. "  $" .. formatPrice(info.usd),
        subtitle = formatChange(info.usd_24h_change),
        glyph = "currency-bitcoin",
      })
    end
  end
  if #rows == 0 then
    rows[1] = { id = "", title = "No results", subtitle = "", glyph = "mood-empty" }
  end
  return rows
end

local doSearch -- forward

local function finish(query, rows, cacheable)
  inflight = false
  if cacheable then
    cache[query] = { rows = rows, at = os.time() }
  end
  if wanted ~= query then
    doSearch(wanted)
    return
  end
  launcher.setResults(query, rows)
end

local function fetchPrices(query, order, titles)
  local url = API .. "/simple/price?ids=" .. table.concat(order, ",") .. "&vs_currencies=usd&include_24hr_change=true"
  noctalia.http({ url = url, headers = { UA } }, function(res)
    if not ok(res) then
      finish(query, httpFailRows(res))
      return
    end
    local data = noctalia.json.decode(res.body) or {}
    finish(query, priceRows(data, order, titles), true)
  end)
end

doSearch = function(query)
  inflight = true
  if query == "" then
    local ids, titles = {}, {}
    for _, coin in WATCHLIST do
      table.insert(ids, coin.id)
      titles[coin.id] = coin.symbol
    end
    fetchPrices(query, ids, titles)
    return
  end
  noctalia.http({ url = API .. "/search?query=" .. noctalia.string.urlEncode(query), headers = { UA } }, function(res)
    if not ok(res) then
      finish(query, httpFailRows(res))
      return
    end
    local data = noctalia.json.decode(res.body) or {}
    local ids = {}
    for i, coin in (data.coins or {}) do
      if i > 5 then
        break
      end
      table.insert(ids, coin.id)
    end
    if #ids == 0 then
      finish(query, failRow("No coins found for \"" .. query .. "\""))
      return
    end
    fetchPrices(query, ids, nil)
  end)
end

function onQuery(query)
  wanted = noctalia.string.trim(query)
  local hit = cache[wanted]
  if hit and os.time() - hit.at < CACHE_TTL then
    launcher.setResults(query, hit.rows)
    return
  end
  launcher.setResults(query, { { id = "", title = "Fetching prices…", subtitle = "", glyph = "currency-bitcoin" } })
  if not inflight then
    doSearch(wanted)
  end
end

function onActivate(id)
  if id ~= "" then
    noctalia.runAsync("xdg-open '" .. id .. "'")
  end
end
