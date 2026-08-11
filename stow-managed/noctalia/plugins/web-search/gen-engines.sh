#!/usr/bin/env bash
# 从引擎表生成每个前缀的 provider 入口 + plugin.toml。改引擎后重跑本脚本。
set -euo pipefail
cd "$(dirname "$0")"

# trigger|Name|glyph|url(%s 为查询占位)
ENGINES='g|Google|brand-google|https://www.google.com/search?q=%s
gh|GitHub|brand-github|https://github.com/search?q=%s
gpt|ChatGPT|brand-openai|https://chat.openai.com/?q=%s
dd|DuckDuckGo|search|https://duckduckgo.com/?q=%s
yt|YouTube|brand-youtube|https://www.youtube.com/results?search_query=%s
gt|Google Translate|language|https://translate.google.com/?text=%s
maps|Google Maps|map-pin|https://www.google.com/maps/search/%s/
scholar|Google Scholar|school|https://scholar.google.com/scholar?q=%s
wa|Wolfram Alpha|math-function|https://www.wolframalpha.com/input/?i=%s
ama|Amazon|brand-amazon|https://www.amazon.com/s/?field-keywords=%s
eb|Ebay|shopping-cart|https://www.ebay.com/sch/i.html?_nkw=%s
af|Artifact Hub|packages|https://artifacthub.io/packages/search?ts_query_web=%s&sort=relevance&page=1
hm|Home Manager Options|settings|https://home-manager-options.extranix.com/?query=%s&release=master'

cat > plugin.toml << TOML
# 由 gen-engines.sh 生成,不要手改;改引擎表后重跑脚本
id = "xiongchenyu6/web-search"
name = "Web Search"
version = "2.1.0"
plugin_api = 9
author = "xiongchenyu6"
license = "MIT"
dependencies = []
tags = ["launcher"]
icon = "world-search"
description = "albert-style web searches: /g /gh /hm … each engine has its own prefix."
TOML

rm -f engine-*.luau
while IFS='|' read -r trigger name glyph url; do
  cat > "engine-$trigger.luau" << LUAU
--!nonstrict
-- 由 gen-engines.sh 生成。/$trigger <query> — $name。
local NAME = "$name"
local URL = "$url"
local GLYPH = "$glyph"

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
LUAU
  cat >> plugin.toml << TOML

[[launcher_provider]]
id = "$trigger"
entry = "engine-$trigger.luau"
prefix = "$trigger"
glyph = "$glyph"
include_in_global_search = false
TOML
done <<< "$ENGINES"
echo "generated $(ls engine-*.luau | wc -l) engines"
