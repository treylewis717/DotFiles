Config { font = "Ubuntu Nerd Font Bold 10"
       , bgColor = "#282c34"
       , fgColor = "#dfdfdf"
       , position = TopH 24 
       , lowerOnStart = True
       , persistent = True
       , hideOnStart = False
       , iconRoot = "/home/trey/.config/xmonad/xpm/"
       , allDesktops = True
       , commands = [
                    Run Cpu ["-t", "  CPU: <total>%", "-H","50",
                               "--normal","#98be65","--high","#ff6c6b"] 10
                    , Run Memory ["-t","  Memory: <usedratio>%"] 10
		            , Run Com "/home/trey/.config/xmonad/trayer-padding-icon.sh" [] "trayerpad" 20
                    , Run Mpris2 "spotify" ["-t", "  <title> - <artist>"] 1
                    , Run Date "  %A %m/%d/%Y" "date" 10
                    , Run Date "  %H:%M:%S" "time" 10
                    , Run UnsafeStdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = " <icon=haskell_20.xpm/> <fc=#5b6268>|</fc>%UnsafeStdinReader% }{ <fc=#4db5bd> %mpris2% </fc> <fc=#5b6268>|</fc> <fc=#98be65> %cpu% </fc> <fc=#5b6268>|</fc> <fc=#2257b0> %memory% </fc> <fc=#5b6268>|</fc> <fc=#ebbe7b>%date%</fc> <fc=#5b6268>|</fc> <fc=#da8548>%time%</fc> <fc=#5b6268>|</fc> %trayerpad%"
       }
