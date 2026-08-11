import QtQuick
import qs.Commons

Item {
  id: root
  property var pluginApi: null

  Component.onCompleted: {
    Logger.i("NixSearch", "Plugin loaded")
  }
}
