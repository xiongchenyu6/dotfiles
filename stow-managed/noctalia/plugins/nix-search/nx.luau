--!nonstrict
-- /nx <query> — 本地 nix search nixpkgs,结果直接进 launcher,Enter 复制属性名。
-- runAsync 拿不到 stdout,走临时文件 + readFile。

local TMP = "/tmp/noctalia-nix-search.json"
local MAX_RESULTS = 30

local running = false
local resultsFor = nil -- 已有结果对应的查询
local rows = {}
local wanted = ""

local function sanitize(query)
  -- 只留安全字符,避免拼 shell 注入
  return query:gsub("[^%w%.%-_+ ]", "")
end

local function webRow(query)
  return {
    id = "web:" .. query,
    title = "Open on search.nixos.org",
    subtitle = "browser search for \"" .. query .. "\"",
    glyph = "world-search",
  }
end

local function hintRow(title, subtitle)
  return { id = "", title = title, subtitle = subtitle or "", glyph = "package" }
end

local function parseResults()
  local text = noctalia.readFile(TMP)
  local data = text and noctalia.json.decode(text) or nil
  local out = {}
  if data then
    for attr, info in data do
      -- legacyPackages.x86_64-linux.foo.bar -> foo.bar
      local short = attr:gsub("^legacyPackages%.[^%.]+%.", "")
      table.insert(out, {
        id = short,
        title = short .. (info.version ~= "" and (" (" .. info.version .. ")") or ""),
        subtitle = info.description or "",
        glyph = "package",
      })
      if #out >= MAX_RESULTS then
        break
      end
    end
  end
  return out
end

local function startSearch(query)
  running = true
  local cmd = "nix search nixpkgs --json " .. sanitize(query) .. " > " .. TMP .. " 2>/dev/null"
  noctalia.runAsync(cmd, function(success)
    running = false
    rows = success and parseResults() or {}
    resultsFor = query
    if wanted ~= query then
      startSearch(wanted)
      return
    end
    local shown = table.clone(rows)
    if #shown == 0 then
      table.insert(shown, hintRow("No results for \"" .. query .. "\""))
    end
    table.insert(shown, webRow(query))
    launcher.setResults(query, shown)
  end)
end

function onQuery(query)
  local text = noctalia.string.trim(query)
  wanted = text
  if #text < 2 then
    launcher.setResults(query, { hintRow("Type a package name…", "results appear shortly after you stop typing") })
    return
  end
  if resultsFor == text then
    local shown = table.clone(rows)
    if #shown == 0 then
      table.insert(shown, hintRow("No results for \"" .. text .. "\""))
    end
    table.insert(shown, webRow(text))
    launcher.setResults(query, shown)
    return
  end
  launcher.setResults(query, { hintRow("Searching nixpkgs for \"" .. text .. "\"…", "first search after boot can take a while") })
  if not running then
    startSearch(text)
  end
end

function onActivate(id)
  local webQuery = id:match("^web:(.*)$")
  if webQuery then
    noctalia.runAsync("xdg-open 'https://search.nixos.org/packages?query=" .. noctalia.string.urlEncode(webQuery) .. "'")
    return
  end
  if id ~= "" then
    noctalia.copyToClipboard(id, "text/plain")
    noctalia.notify("Nix Search", id .. " copied to clipboard")
  end
end
