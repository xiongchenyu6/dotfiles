import QtQuick
import Quickshell
import qs.Commons

Item {
  id: root

  property var pluginApi: null

  // Provider metadata
  property string name: "Web Search"
  property var launcher: null
  property bool handleSearch: false
  property string supportedLayouts: "list"
  property bool supportsAutoPaste: false
  property bool ignoreDensity: false

  // 从 albert websearch 迁移的引擎表(nx 有原生插件,不在此列)
  property var engines: [
    { trigger: "g",       name: "Google",             icon: "brand-google",   url: "https://www.google.com/search?q=%s" },
    { trigger: "gh",      name: "GitHub",             icon: "brand-github",   url: "https://github.com/search?q=%s" },
    { trigger: "gpt",     name: "ChatGPT",            icon: "brand-openai",   url: "https://chat.openai.com/?q=%s" },
    { trigger: "dd",      name: "DuckDuckGo",         icon: "search",         url: "https://duckduckgo.com/?q=%s" },
    { trigger: "yt",      name: "YouTube",            icon: "brand-youtube",  url: "https://www.youtube.com/results?search_query=%s" },
    { trigger: "gt",      name: "Google Translate",   icon: "language",       url: "https://translate.google.com/?text=%s" },
    { trigger: "maps",    name: "Google Maps",        icon: "map-pin",        url: "https://www.google.com/maps/search/%s/" },
    { trigger: "scholar", name: "Google Scholar",     icon: "school",         url: "https://scholar.google.com/scholar?q=%s" },
    { trigger: "wa",      name: "Wolfram Alpha",      icon: "math-function",  url: "https://www.wolframalpha.com/input/?i=%s" },
    { trigger: "ama",     name: "Amazon",             icon: "brand-amazon",   url: "https://www.amazon.com/s/?field-keywords=%s" },
    { trigger: "eb",      name: "Ebay",               icon: "shopping-cart",  url: "https://www.ebay.com/sch/i.html?_nkw=%s" },
    { trigger: "af",      name: "Artifact Hub",       icon: "packages",       url: "https://artifacthub.io/packages/search?ts_query_web=%s&sort=relevance&page=1" },
    { trigger: "hm",      name: "Home Manager Options", icon: "settings",     url: "https://home-manager-options.extranix.com/?query=%s&release=master" }
  ]

  function init() {
  }

  function parseToken(searchText) {
    const m = searchText.trim().match(/^>(\S+)/);
    return m ? m[1] : null;
  }

  function findEngine(token) {
    for (let i = 0; i < engines.length; i++) {
      if (engines[i].trigger === token) {
        return engines[i];
      }
    }
    return null;
  }

  function handleCommand(searchText) {
    const token = parseToken(searchText);
    return token !== null && findEngine(token) !== null;
  }

  function commands() {
    return engines.map(engine => ({
      "name": ">" + engine.trigger,
      "description": "Search " + engine.name,
      "icon": engine.icon,
      "isTablerIcon": true,
      "isImage": false,
      "onActivate": function () {
        launcher.setSearchText(">" + engine.trigger + " ");
      }
    }));
  }

  function getResults(searchText) {
    const token = parseToken(searchText);
    const engine = token ? findEngine(token) : null;
    if (!engine) {
      return [];
    }
    const query = searchText.trim().slice(token.length + 1).trim();
    return [{
      "name": engine.name + ": " + (query || "…"),
      "description": query ? "Enter to open in browser" : "type a query, then Enter",
      "icon": engine.icon,
      "isTablerIcon": true,
      "isImage": false,
      "provider": root,
      "onActivate": function () {
        if (!query) {
          return;
        }
        Quickshell.execDetached(["xdg-open", engine.url.replace("%s", encodeURIComponent(query))]);
        launcher.close();
      }
    }];
  }
}
