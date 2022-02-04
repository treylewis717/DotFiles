Config { font = "xft:Ubuntu:weight=bold:pixelsize=12:antialias=true:hinting=true"
       , additionalFonts = ["xft: RobotoMono Nerd Font:weight=bold:pixelsize=12:antialias=true:hinting=true"]

       , bgColor = "#282c34"
       , fgColor = "#ffffff"
       , position = Static { xpos = 0, ypos = 0, width = 1920, height = 24 }
       , lowerOnStart = True
       , persistent = True
       , hideOnStart = False
       , iconRoot = "/home/trey/.xmonad/xpm/" -- default: "."
       , allDesktops = True
       , commands = [
                      Run Network "wlan0" ["-t", "<fn=1> </fn>  <rx>kb  <fn=1> </fn>  <tx>kb"] 10
                    , Run Cpu ["-t", "<fn=1> </fn> CPU: (<total>%)", "-H","50",
                               "--normal","green","--high","red"] 10
                    , Run Memory ["-t","<fn=1> </fn> Memory: (<usedratio>%)"] 10
                    , Run Com "uname" ["-s","-r"] "" 3600
	            , Run Com "/home/trey/.local/bin/pacupdate" [] "pacupdate" 2000
		    , Run Com "/home/trey/.xmonad/trayer-padding-icon.sh" [] "trayerpad" 20
                    , Run Date "<fn=1> </fn> %A, %B %_d, %Y" "date" 10
                    , Run Date "<fn=1> </fn> %H:%M:%S" "time" 10
                    , Run UnsafeStdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "<icon=logo.xpm/><fc=#666666>|</fc>%UnsafeStdinReader% }{ <fc=#EC0000><fn=1> </fn> %uname% </fc> <fc=#666666>|</fc> <fc=#00EC00> %cpu% </fc> <fc=#666666>|</fc> <fc=#0000EC> %memory% </fc> <fc=#666666>|</fc> <fc=#C800C8> %wlan0% </fc> <fc=#666666>|</fc> <fc=#00CBFF> <fn=1>ﮮ </fn> %pacupdate% </fc> <fc=#666666>|</fc> <fc=#EEC800>%date%</fc> <fc=#666666>|</fc> <fc=#F26F00>%time%</fc> <fc=#666666>|</fc> %trayerpad%"
       }
