import QtQuick
import Quickshell
import Quickshell.Io
import qs.Common
import qs.Modules.Plugins
import qs.Widgets

PluginComponent {
    id: root

    layerNamespacePlugin: "voxtype"

    property string statusClass: "stopped"
    property string statusIcon: "\uf131"
    property string meetingPhase: "idle"
    readonly property bool isRecording: statusClass === "recording" || meetingPhase === "recording"
    readonly property bool isProcessing: statusClass === "transcribing" || meetingPhase === "stopping"
    property real recordingOpacity: 1.0

    pillClickAction: () => {
        if (!toggleProcess.running)
            toggleProcess.running = true;
    }
    pillRightClickAction: () => {
        if (!meetingToggleProcess.running)
            meetingToggleProcess.running = true;
    }

    Process {
        id: statusProcess
        command: ["voxtype", "status", "--format", "json", "--icon-theme", "nerd-font"]
        stdout: StdioCollector {
            onStreamFinished: {
                try {
                    const status = JSON.parse(text.trim());
                    root.statusClass = status.class || status.alt || "stopped";
                    root.statusIcon = status.text || "\uf131";
                } catch (error) {
                    root.statusClass = "stopped";
                }
            }
        }
        onExited: (exitCode, exitStatus) => {
            if (exitCode !== 0)
                root.statusClass = "stopped";
        }
    }

    Process {
        id: meetingStatusProcess
        command: ["voxtype", "meeting", "status"]
        stdout: StdioCollector {
            onStreamFinished: {
                const match = text.match(/Meeting Status: (\w+)/);
                root.meetingPhase = match ? match[1] : "idle";
            }
        }
    }

    Timer {
        interval: 250
        running: true
        repeat: true
        triggeredOnStart: true
        onTriggered: {
            if (!statusProcess.running)
                statusProcess.running = true;
            if (!meetingStatusProcess.running)
                meetingStatusProcess.running = true;
        }
    }

    SequentialAnimation {
        running: root.isRecording
        loops: Animation.Infinite
        NumberAnimation {
            target: root
            property: "recordingOpacity"
            from: 1.0
            to: 0.4
            duration: 650
            easing.type: Easing.InOutSine
        }
        NumberAnimation {
            target: root
            property: "recordingOpacity"
            from: 0.4
            to: 1.0
            duration: 650
            easing.type: Easing.InOutSine
        }
    }

    Process {
        id: toggleProcess
        command: ["voxtype", "record", "toggle"]
    }

    Process {
        id: meetingToggleProcess
        command: ["voxtype-meeting"]
    }

    component StatusIndicator: StyledText {
        text: root.isRecording ? "\uf111" : root.statusIcon
        color: root.isRecording ? "#ff4040" : root.isProcessing ? "#ffbf47" : Theme.surfaceText
        opacity: root.isRecording ? root.recordingOpacity : 1.0
        font.family: "Symbols Nerd Font"
        font.pixelSize: root.iconSize
        anchors.centerIn: parent
    }

    horizontalBarPill: Component { StatusIndicator {} }
    verticalBarPill: Component { StatusIndicator {} }
}
