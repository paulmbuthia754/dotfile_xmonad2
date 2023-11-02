-- xmobar configuration file as a haskell file
--
-- Copied from xmobarrc written by David Bewer (-- Repository: https://github.com/davidbrewer/xmonad-ubuntu-conf )

import Xmobar

config :: Config
config = defaultConfig {
    font = "Ubuntu Nerd Font Bold 10",
    additionalFonts = [ "mononoki Nerd Font 10",
                        "Font Awesome 6 Free Bold 9",
                        "Font Awesome 6 Brands Bold 9",
                        "DejaVu Sans Mono Bold 9",
                        "Ubuntu Mono Bold 11"
                        ],
    bgColor = "black",
    fgColor = "grey",

    -- Position is top left, taking up 82% of screen.
    -- You are likely to have to tweak the width here based on the width
    -- of your screen to make it play nicely with stalonetray, which we
    -- want to be taking up the remainer of the space on the right side
    -- of your screen.
    position = TopW L 84,

    -- general behaviour
    lowerOnStart =   False,
    overrideRedirect = True,
    hideOnStart  =   False,
    
    -- Show on all desktops
    allDesktops = True,
    persistent  = True,
    iconRoot    = ".config/xmonad/xpm",

    -- list of commands which gather information about your system for
    -- presentation in the bar. 
    commands = [ 
        -- Gather and format CPU usage information. 
        -- If it's above 50%, we consider it high usage and make it red.  
        Run $ Cpu [
        "-p", "2",
        "--template", "<fn=2>\xf2db</fn>: <fn=5><total>%</fn>",
        "-H","50",
        "--high","red"
        ] 50,
        Run $ DiskU [
            ("/",                   "<fn=2>\xf390</fn>:<usedp>% "),
            ("/home",               "<fn=2>\xf015</fn>:<usedp>% "),
            ("/media/paul/Media",   "<fn=2>\xf008</fn>:<usedp>% ")
            ]
            ["-L", "20", "-H", "50", "-m", "1", "-p", "3"]
            60,


        -- Gather and format memory usage information
        Run $ Memory [
        "-t","<fn=2>\xf538</fn>: <usedratio>%"
        ] 10,

        -- Date formatting
        Run $ Date "%R  %a %0d-%0m" "date" 10,

        -- Echos a "battery" icon in front of the pacman updates.
        Run $ Com "echo" ["<fn=2>\xf242</fn>"] "baticon" 3600,

        -- Battery information. This is likely to require some customization
        -- based upon your specific hardware. Or, for a desktop you may want
        -- to just remove this section entirely.
        Run $ Battery [
        "-t", "<acstatus> <left>",
        "-H", "80",
        "-L", "20",
        "-h", "#11ffcc",
        "-l", "red",
        "--",
        "-H", "-5",
        "-L", "-15",
        "-O", "<fc=#11eeff>AC:</fc>",
        "-o", "<fc=grey>Batt:</fc>",
        "-i", "<fc=#ddff88>Full:</fc>",
        "-p", "blue",
        "-P",
        "-m", "orange",
        "-h", "green",
        "-l", "red"
        -- "-a", "notify-send -u critical 'Battery running out!!'",
        -- "-A", "3"
        ] 20,

        -- To get volume information, we run a custom bash script.
        -- This is because the built-in volume support in xmobar is disabled
        -- in Debian and derivatives like Ubuntu. 
        -- Run Com "/bin/bash" ["-c", "~/.config/xmonad/get-volume"] "myvolume" 10,
        -- Get mpris player
        Run $ Com "/bin/bash" ["-c", "~/bin/my-player-get"] "myplayer" 10,
        Run $ Com "echo" ["<fn=2>\xf144</fn>"] "playicon" 3600,
        Run $ Com "/bin/bash" ["-c", "~/bin/speaker-color #e6744c"] "speaker-color" 10,

        Run $ Alsa "default" "Master" [
            "-t", "<fn=2>\xf028</fn>: <volume>% <status>",
            "--",
            "--on", "",
            "--off", "[M]"
        ],

        -- Trayer Padding
        Run $ Com "~/.xmonad/trayer-padding-icon.sh" [] "trayerpad" 100,

        -- This line tells xmobar to read input from stdin. That's how we
        -- get the information that xmonad is sending it for display.
        -- Run UnsafeStdinReader,
        Run $ UnsafeNamedXPropertyLog "_TRAYPAD" "pad",
        Run UnsafeXMonadLog
    ],

    -- Separator character used to wrape variables in the xmobar template 
    sepChar = "%",

    -- Alignment separater characer used in the xmobar template. Everything
    -- before this will be aligned left, everything after aligned right.
    alignSep = "}{",

    -- Overall template for the layout of the xmobar contents. Note that
    -- space is significant and can be used to add padding.
    -- template = "%XMonadLog% }{ %battery% | %cpu% | %memory% | <action=`speaker-toggle`><fc=%speaker-color%> Vol: %myvolume% </fc></action> | <action=`my-player-next`>Plr: <fc=#6c97d4>%myplayer%</fc> </action> <fc=#e6744c><action=`gnome-calendar`>%date%</action></fc> "



    template = "<action=`xdotool key alt+n`><icon=xmonad-16.xpm/></action> %UnsafeXMonadLog% }{<fc=#e6744c><action=`gnome-calendar`>%date%</action></fc>   %baticon% %battery% | %cpu% | %memory% | %disku% | <action=`speaker-toggle`><fc=%speaker-color%>%alsa:default:Master% </fc></action> | <action=`my-player-next`><fc=#6c97d4> %playicon% %myplayer%</fc> </action>"
    }

main :: IO ()
main = configFromArgs config >>= xmobar
