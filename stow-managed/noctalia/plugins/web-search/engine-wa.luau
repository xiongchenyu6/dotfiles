--!nonstrict
-- 由 gen-engines.sh 生成。/wa <query> — Wolfram Alpha。
local NAME = "Wolfram Alpha"
local URL = "https://www.wolframalpha.com/input/?i=%s"
local GLYPH = "math-function"

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
