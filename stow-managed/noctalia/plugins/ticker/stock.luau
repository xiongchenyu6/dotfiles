--!nonstrict
-- /st — 股价(Yahoo Finance chart 接口,免 key)。空查询显示关注列表;/st nflx 查任意代码。

local WATCHLIST = { "AAPL", "TSLA", "NVDA", "MSFT", "GOOGL" }

local function chartUrl(symbol)
  return "https://query1.finance.yahoo.com/v8/finance/chart/" .. noctalia.string.urlEncode(symbol) .. "?interval=1d&range=1d"
end

local function rowFor(meta)
  local price = meta.regularMarketPrice
  local prev = meta.chartPreviousClose
  local subtitle = ""
  if price and prev and prev > 0 then
    local chg = (price - prev) / prev * 100
    subtitle = "today " .. (chg >= 0 and "▲ +" or "▼ ") .. string.format("%.2f", chg) .. "%"
  end
  return {
    id = "https://finance.yahoo.com/quote/" .. meta.symbol,
    title = meta.symbol .. "  $" .. (price and string.format("%.2f", price) or "?"),
    subtitle = subtitle,
    glyph = "chart-line",
  }
end

function onQuery(query)
  local text = noctalia.string.trim(query)
  launcher.setResults(query, { { id = "", title = "Fetching prices…", subtitle = "", glyph = "chart-line" } })

  local symbols = {}
  if text == "" then
    symbols = WATCHLIST
  else
    symbols = { text:upper() }
  end

  local rows = {}
  local done = 0
  for index, symbol in symbols do
    noctalia.http({ url = chartUrl(symbol), headers = { "User-Agent: Mozilla/5.0" } }, function(res)
      done += 1
      if res.ok then
        local data = noctalia.json.decode(res.body)
        local meta = data and data.chart and data.chart.result and data.chart.result[1] and data.chart.result[1].meta
        if meta and meta.symbol then
          rows[index] = rowFor(meta)
        end
      end
      if done == #symbols then
        local shown = {}
        for i = 1, #symbols do
          if rows[i] then
            table.insert(shown, rows[i])
          end
        end
        if #shown == 0 then
          shown[1] = { id = "", title = "No data for \"" .. text .. "\"", subtitle = "", glyph = "mood-empty" }
        end
        launcher.setResults(query, shown)
      end
    end)
  end
end

function onActivate(id)
  if id ~= "" then
    noctalia.runAsync("xdg-open '" .. id .. "'")
  end
end
