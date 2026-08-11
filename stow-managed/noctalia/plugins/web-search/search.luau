--!nonstrict
-- /s — albert websearch 引擎迁移。
-- `/s` 列出全部引擎;`/s g rust` 回车用 Google 搜 rust。
-- 引擎行回车 = 进入该引擎(setQuery 停留在 provider 内)。

local ENGINES = {
  { trigger = "g",       name = "Google",               glyph = "brand-google",  url = "https://www.google.com/search?q=%s" },
  { trigger = "gh",      name = "GitHub",               glyph = "brand-github",  url = "https://github.com/search?q=%s" },
  { trigger = "gpt",     name = "ChatGPT",              glyph = "brand-openai",  url = "https://chat.openai.com/?q=%s" },
  { trigger = "dd",      name = "DuckDuckGo",           glyph = "search",        url = "https://duckduckgo.com/?q=%s" },
  { trigger = "yt",      name = "YouTube",              glyph = "brand-youtube", url = "https://www.youtube.com/results?search_query=%s" },
  { trigger = "gt",      name = "Google Translate",     glyph = "language",      url = "https://translate.google.com/?text=%s" },
  { trigger = "maps",    name = "Google Maps",          glyph = "map-pin",       url = "https://www.google.com/maps/search/%s/" },
  { trigger = "scholar", name = "Google Scholar",       glyph = "school",        url = "https://scholar.google.com/scholar?q=%s" },
  { trigger = "wa",      name = "Wolfram Alpha",        glyph = "math-function", url = "https://www.wolframalpha.com/input/?i=%s" },
  { trigger = "ama",     name = "Amazon",               glyph = "brand-amazon",  url = "https://www.amazon.com/s/?field-keywords=%s" },
  { trigger = "eb",      name = "Ebay",                 glyph = "shopping-cart", url = "https://www.ebay.com/sch/i.html?_nkw=%s" },
  { trigger = "af",      name = "Artifact Hub",         glyph = "packages",      url = "https://artifacthub.io/packages/search?ts_query_web=%s&sort=relevance&page=1" },
  { trigger = "hm",      name = "Home Manager Options", glyph = "settings",      url = "https://home-manager-options.extranix.com/?query=%s&release=master" },
}

local function findEngine(token)
  for _, engine in ENGINES do
    if engine.trigger == token then
      return engine
    end
  end
  return nil
end

local function engineRows(filter)
  local rows = {}
  for _, engine in ENGINES do
    if filter == "" or engine.trigger:find(filter, 1, true) == 1 or engine.name:lower():find(filter:lower(), 1, true) then
      table.insert(rows, {
        id = "engine:" .. engine.trigger,
        title = "/" .. engine.trigger .. "  " .. engine.name,
        subtitle = "Enter to search with " .. engine.name,
        glyph = engine.glyph,
      })
    end
  end
  return rows
end

function onQuery(query)
  local text = noctalia.string.trim(query)
  local token, rest = text:match("^(%S+)%s+(.*)$")
  local engine = token and findEngine(token) or findEngine(text)
  if engine and token then
    launcher.setResults(query, { {
      id = "open:" .. engine.trigger .. ":" .. rest,
      title = engine.name .. ": " .. rest,
      subtitle = "Enter to open in browser",
      glyph = engine.glyph,
    } })
  elseif engine then
    launcher.setResults(query, { {
      id = "engine:" .. engine.trigger,
      title = engine.name,
      subtitle = "type a query, then Enter",
      glyph = engine.glyph,
    } })
  else
    launcher.setResults(query, engineRows(text))
  end
end

function onActivate(id)
  local trigger = id:match("^engine:(.+)$")
  if trigger then
    launcher.setQuery(trigger .. " ")
    return
  end
  local openTrigger, query = id:match("^open:([^:]+):(.*)$")
  if openTrigger then
    local engine = findEngine(openTrigger)
    if engine and query ~= "" then
      local url = engine.url:gsub("%%s", noctalia.string.urlEncode(query))
      noctalia.runAsync("xdg-open '" .. url .. "'")
    end
  end
end
