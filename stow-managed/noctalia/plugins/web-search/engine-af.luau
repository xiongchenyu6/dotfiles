--!nonstrict
-- 由 gen-engines.sh 生成。/af <query> — Artifact Hub。
local NAME = "Artifact Hub"
local URL = "https://artifacthub.io/packages/search?ts_query_web=%s&sort=relevance&page=1"
local GLYPH = "packages"

function onQuery(query)
  local text = noctalia.string.trim(query)
  launcher.setResults(query, { {
    id = text,
    title = NAME .. ": " .. (text ~= "" and text or "…"),
    subtitle = text ~= "" and "Enter to open in browser" or "type a query, then Enter",
    glyph = GLYPH,
  } })
end

function onActivate(id)
  if id ~= "" then
    noctalia.runAsync("xdg-open '" .. string.format(URL, noctalia.string.urlEncode(id)) .. "'")
  end
end
