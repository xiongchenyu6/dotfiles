--!nonstrict
-- 由 gen-engines.sh 生成。/yt <query> — YouTube。
local NAME = "YouTube"
local URL = "https://www.youtube.com/results?search_query=%s"
local GLYPH = "brand-youtube"

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
