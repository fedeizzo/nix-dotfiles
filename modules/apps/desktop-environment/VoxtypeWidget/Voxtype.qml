import QtQuick
import Quickshell
import Quickshell.Io
import qs.Common
import qs.Modules.Plugins
import qs.Services
import qs.Widgets

BasePill {
    id: root

    property string currentState: "stopped"
    property string statusIcon: "\uf131"
    property color statusColor: Theme.surfaceVariantText

    readonly property var stateConfig: ({
        "idle": { icon: "\uf130", color: Theme.primary },
        "recording": { icon: "\uf111", color: Theme.error },
        "transcribing": { icon: "\uf110", color: Theme.warning },
        "stopped": { icon: "\uf131", color: Theme.surfaceVariantText }
    })

    // Process to poll Voxtype status
    Process {
        id: statusProcess
        command: ["voxtype", "status"]
        running: false

        stdout: StdioCollector {
            onStreamFinished: {
                var output = text.trim()
                if (output && output !== root.currentState) {
                    root.currentState = output
                    var config = root.stateConfig[output] || root.stateConfig["stopped"]
                    root.statusIcon = config.icon
                    root.statusColor = config.color
                }
            }
        }
    }

    Timer {
        interval: 500
        running: true
        repeat: true
        onTriggered: {
            if (!statusProcess.running) {
                statusProcess.running = true
            }
        }
    }

    // Command process to toggle recording state
    Process {
        id: toggleProcess
        command: ["voxtype", "record", "toggle"]
        running: false
    }

    // Click handler with ripple animation
    MouseArea {
        anchors.fill: parent
        cursorShape: Qt.PointingHandCursor
        onPressed: function(mouse) {
            root.triggerRipple(this, mouse.x, mouse.y)
            toggleProcess.running = true
        }
    }

    // Widget content automatically sized by BasePill
    content: Component {
        Row {
            spacing: Theme.spacingS

            StyledText {
                anchors.verticalCenter: parent.verticalCenter
                text: root.statusIcon
                font.family: "Symbols Nerd Font"
                font.pixelSize: Theme.barTextSize(root.barThickness, root.barConfig?.fontScale)
                color: root.statusColor
            }
        }
    }
}
