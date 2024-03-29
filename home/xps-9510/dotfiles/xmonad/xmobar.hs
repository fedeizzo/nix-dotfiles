Config { 
    font = "xft:MesloLGL Nerd Font:size=8", 
    borderColor = "black",
    border = TopB,
    bgColor = "black",
    fgColor = "white",
    alpha = 0,
    position = Top,
    lowerOnStart = True,
    allDesktops = True,
    persistent = True,
    hideOnStart = False,
    overrideRedirect = True,
    commands = [
        Run MultiCpu ["-t",": <bar>","-L","30","-H","60","-h","#FFB6B0","-l","#A3BE8C","-n","#FFFFCC","-w","3"] 10,
        Run Memory ["-t"," <usedbar>","-H","8192","-L","4096","-h","#FFB6B0","-l","#A3BE8C","-n","#FFFFCC"] 10,
        Run Date "%a %b %_d %H:%M" "date" 10,
        Run Wireless "wlp0s20f3" ["-a","l","-t",": <essid>"] 10,
        Run DynNetwork ["-t"," <rx>,  <tx>","-H","200","-L","10","-h","#FFB6B0","-l","#A3BE8C","-n","#FFFFCC"] 10,
        Run Battery ["-t", "<acstatus>: <left> (<timeleft>) %", "--", "-O", "", "-i", "idle", "-o", "", "-h", "#A3BE8C", "-l", "red"] 10,
        Run DiskU [("/", " <usedp>%")] ["-L", "20", "-H", "50", "-m", "1", "-p", "3"] 20,
        Run MultiCoreTemp ["-t", "<avg>°C","-L", "60", "-H", "80","-l", "#A3BE8C", "-n", "yellow", "-h", "red","--", "--mintemp", "20", "--maxtemp", "100"] 10,
        Run ComX "nvidia-smi" [ "--query-gpu=temperature.gpu", "--format=csv,noheader"] "N/A" "gputemp" 10,
        Run Com "/bin/sh" [ "-c", "/home/fedeizzo/.xmonad/bin/trayer-padding-icon" ] "trayerpad" 10,
        Run StdinReader
    ],
    sepChar = "%",
    alignSep = "}{",
    template = "  %StdinReader% }{ %multicpu% %multicoretemp% |  %gputemp% °C | %memory% | %disku% | %wlp0s20f3wi% | %dynnetwork% | %battery% | %date%  %trayerpad%"
}

