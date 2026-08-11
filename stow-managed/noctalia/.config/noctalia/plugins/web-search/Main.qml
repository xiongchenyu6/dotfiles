import QtQuick
import qs.Commons

Item {
  id: root
  property var pluginApi: null

  Component.onCompleted: {
    Logger.i("WebSearch", "Plugin loaded")
  }
}
