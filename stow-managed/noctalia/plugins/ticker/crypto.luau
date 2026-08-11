--!nonstrict
-- /cc — 币价(CoinGecko)。空查询显示关注列表;/cc pepe 搜任意币。

local WATCHLIST = {
  { id = "bitcoin", symbol = "BTC" },
  { id = "ethereum", symbol = "ETH" },
  { id = "solana", symbol = "SOL" },
  { id = "ripple", symbol = "XRP" },
  { id = "dogecoin", symbol = "DOGE" },
}

local API = "https://api.coingecko.com/api/v3"

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

local function priceRows(data, order)
  local rows = {}
  local ids = order or {}
  if #ids == 0 then
    for id in data do
      table.insert(ids, id)
    end
  end
  for _, id in ids do
    local info = data[id]
    if info then
      table.insert(rows, {
        id = "https://www.coingecko.com/en/coins/" .. id,
        title = id:upper() .. "  $" .. formatPrice(info.usd),
        subtitle = formatChange(info.usd_24h_change),
        glyph = "currency-bitcoin",
      })
    end
  end
  return rows
end

local function fetchPrices(query, ids, order, symbolTitles)
  local url = API .. "/simple/price?ids=" .. table.concat(ids, ",") .. "&vs_currencies=usd&include_24hr_change=true"
  noctalia.http({ url = url }, function(res)
    if not res.ok then
      launcher.setResults(query, { { id = "", title = "Fetch failed", subtitle = "check network", glyph = "alert-circle" } })
      return
    end
    local data = noctalia.json.decode(res.body) or {}
    local rows = priceRows(data, order)
    -- 关注列表用短代码替换标题
    if symbolTitles then
      for i, row in rows do
        local info = data[order[i]]
        if info then
          row.title = symbolTitles[order[i]] .. "  $" .. formatPrice(info.usd)
        end
      end
    end
    if #rows == 0 then
      rows[1] = { id = "", title = "No results", subtitle = "", glyph = "mood-empty" }
    end
    launcher.setResults(query, rows)
  end)
end

function onQuery(query)
  local text = noctalia.string.trim(query)
  launcher.setResults(query, { { id = "", title = "Fetching prices…", subtitle = "", glyph = "currency-bitcoin" } })
  if text == "" then
    local ids, titles = {}, {}
    for _, coin in WATCHLIST do
      table.insert(ids, coin.id)
      titles[coin.id] = coin.symbol
    end
    fetchPrices(query, ids, ids, titles)
    return
  end
  -- 任意币:先 search 拿 id 再取价
  noctalia.http({ url = API .. "/search?query=" .. noctalia.string.urlEncode(text) }, function(res)
    if not res.ok then
      launcher.setResults(query, { { id = "", title = "Search failed", subtitle = "check network", glyph = "alert-circle" } })
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
      launcher.setResults(query, { { id = "", title = "No coins found for \"" .. text .. "\"", subtitle = "", glyph = "mood-empty" } })
      return
    end
    fetchPrices(query, ids, ids, nil)
  end)
end

function onActivate(id)
  if id ~= "" then
    noctalia.runAsync("xdg-open '" .. id .. "'")
  end
end
