import QtQuick
import Quickshell
import Quickshell.Io
import qs.Commons

Item {
  id: root

  property var pluginApi: null

  // Provider metadata
  property string name: "Nix Packages"
  property var launcher: null
  property bool handleSearch: false
  property string supportedLayouts: "list"
  property bool supportsAutoPaste: false
  property bool ignoreDensity: false

  property int maxResults: 30

  property string query: ""
  property string searchedQuery: ""
  property var results: []
  property bool searching: false
  property bool searchFailed: false

  function init() {
  }

  // 键入停顿 400ms 后才真正跑 nix search,避免每个字符都起进程
  Timer {
    id: debounce
    interval: 400
    repeat: false
    onTriggered: root.runSearch()
  }

  Process {
    id: searcher
    stdout: StdioCollector {}
    stderr: StdioCollector {}
    onExited: exitCode => root.onSearchDone(exitCode)
  }

  function runSearch() {
    if (query === "" || query === searchedQuery) {
      return;
    }
    if (searcher.running) {
      debounce.restart();
      return;
    }
    searching = true;
    searchFailed = false;
    searchedQuery = query;
    // ^ 前缀让 nix 按正则匹配任意位置;多词空格分隔 = AND
    searcher.command = ["nix", "search", "nixpkgs", "--json"].concat(query.split(/\s+/));
    searcher.running = true;
    Logger.i("NixSearch", "Searching:", query);
  }

  function onSearchDone(exitCode) {
    searching = false;
    if (exitCode !== 0) {
      results = [];
      searchFailed = true;
    } else {
      try {
        const parsed = JSON.parse(searcher.stdout.text || "{}");
        results = Object.keys(parsed).slice(0, maxResults).map(attr => {
          const info = parsed[attr];
          // legacyPackages.x86_64-linux.foo.bar -> foo.bar
          const shortAttr = attr.split(".").slice(2).join(".");
          return {
            attr: shortAttr,
            version: info.version || "",
            description: info.description || ""
          };
        });
        searchFailed = false;
      } catch (e) {
        Logger.e("NixSearch", "Parse error:", e);
        results = [];
        searchFailed = true;
      }
    }
    // 查询在等待期间又变了就接着搜
    if (query !== searchedQuery) {
      debounce.restart();
    }
    if (launcher && launcher.activeProvider == root) {
      launcher.updateResults();
    }
  }

  function handleCommand(searchText) {
    return searchText.startsWith(">nx");
  }

  function commands() {
    return [{
      "name": ">nx",
      "description": "Search nixpkgs packages",
      "icon": "package",
      "isTablerIcon": true,
      "isImage": false,
      "onActivate": function () {
        launcher.setSearchText(">nx ");
      }
    }];
  }

  function getResults(searchText) {
    const trimmed = searchText.trim();
    if (!trimmed.startsWith(">nx")) {
      return [];
    }
    const newQuery = trimmed.slice(3).trim();

    if (newQuery.length < 2) {
      return [hintEntry("Type a package name…", "results appear as you pause typing")];
    }

    if (newQuery !== query) {
      query = newQuery;
      debounce.restart();
    }

    let rows = [];
    if (searching || (query !== searchedQuery)) {
      rows.push(hintEntry("Searching nixpkgs for \"" + query + "\"…", "first search after boot can take a while (eval cache)"));
    } else if (searchFailed || results.length === 0) {
      rows.push(hintEntry("No results for \"" + searchedQuery + "\"", "Enter to search on search.nixos.org instead"));
    }

    rows = rows.concat(results.map(formatEntry));
    rows.push({
      "name": "Open on search.nixos.org",
      "description": "browser search for \"" + (query || "") + "\"",
      "icon": "world-search",
      "isTablerIcon": true,
      "isImage": false,
      "provider": root,
      "onActivate": function () {
        Quickshell.execDetached(["xdg-open", "https://search.nixos.org/packages?query=" + encodeURIComponent(root.query)]);
        launcher.close();
      }
    });
    return rows;
  }

  function hintEntry(title, description) {
    return {
      "name": title,
      "description": description,
      "icon": "package",
      "isTablerIcon": true,
      "isImage": false,
      "onActivate": function () {}
    };
  }

  function formatEntry(entry) {
    return {
      "name": entry.attr + (entry.version ? " (" + entry.version + ")" : ""),
      "description": entry.description,
      "icon": "package",
      "isTablerIcon": true,
      "isImage": false,
      "singleLine": true,
      "provider": root,
      // 选中 = 复制属性名到剪贴板,直接可以拿去 nix shell / home.packages
      "onActivate": function () {
        Quickshell.execDetached(["wl-copy", entry.attr]);
        launcher.close();
      }
    };
  }
}
