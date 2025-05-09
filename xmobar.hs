-- xmobar configuration file as a haskell file

import Xmobar
import System.Directory ( listDirectory )
import Network.HostName (getHostName)
import Data.List ( foldl' )

config :: Config
config = defaultConfig {
    font = "Ubuntu Bold 10",
    additionalFonts = [ "mononoki Nerd Font 10",
                        "Font Awesome 6 Free Bold 9",
                        "Font Awesome 6 Brands Bold 9",
                        "DejaVu Sans Mono Bold 9",
                        "Ubuntu Mono Bold 11",
                        "Ubuntu Nerd Font Bold 10"
                        ],
    bgColor = "black",
    fgColor = "grey",

    -- Position is top left, taking up 82% of screen.
    -- You are likely to have to tweak the width here based on the width
    -- of your screen to make it play nicely with stalonetray, which we
    -- want to be taking up the remainer of the space on the right side
    -- of your screen.
    position = TopW L 91,
    -- general behaviour
    lowerOnStart =   False,
    overrideRedirect = True,
    hideOnStart  =   False,

    -- Show on all desktops
    allDesktops = True,
    persistent  = True,
    iconRoot    = ".config/xmonad/xpm",

    -- Separator character used to wrape variables in the xmobar template
    sepChar = "%",

    -- Alignment separater characer used in the xmobar template. Everything
    -- before this will be aligned left, everything after aligned right.
    alignSep = "}{"

    -- Overall template for the layout of the xmobar contents. Note that
    -- space is significant and can be used to add padding.
    -- template = "%XMonadLog% }{ %battery% | %cpu% | %memory% | <action=`speaker-toggle`><fc=%speaker-color%> Vol: %myvolume% </fc></action> | <action=`my-player-next`>Plr: <fc=#6c97d4>%myplayer%</fc> </action> <fc=#e6744c><action=`gnome-calendar`>%date%</action></fc> "

    -- template = "<action=`xdotool key alt+n`><icon=xmonad-16.xpm/></action> %UnsafeXMonadLog% }{<fc=#eed77c><action=`gnome-calendar`>%date%</action></fc>   %baticon% %battery% | %cpu% | %memory% | %disku% | <action=`speaker-toggle`><fc=%speaker-color%>%alsa:default:Master% </fc></action> | <action=`my-player-next`><fc=#6c97d4> %playicon% %myplayer%</fc> </action>"
    -- template = xmonadLog `align` iconRow
    }

myCommands :: [Runnable]
-- list of commands which gather information about your system for
-- presentation in the bar.
myCommands = [
    -- Gather and format CPU usage information.
    -- If it's above 50%, we consider it high usage and make it red.
    Run $ Cpu [
    "-p", "2",
    "--template", fn2 "\xf2db" <> ":" <> fn5 "<total>%",
    "-H","50",
    "--high","red"
    ] 50,
    Run $ DiskU [
        ("/",                   fn2 "\xf390" <> ":<usedp>% "),
        ("/home",               fn2 "\xf015" <> ":<usedp>% "),
        ("/media/paul/Media",   fn2 "\xf008" <> ":<usedp>% ")
        ]
        ["-L", "20", "-H", "50", "-m", "1", "-p", "3"]
        60,


    -- Gather and format memory usage information
    Run $ Memory [
    "-t",fn2 "\xf538" <> ": <usedratio>%"
    ] 10,

    -- Date formatting
    Run $ Date "%R  %a %0d-%0m" "date" 10,

    -- Echos a "battery" icon in front of the pacman updates.
    Run $ Com "echo" [fn2 "\xf242"] "baticon" 3600,

    -- To get volume information, we run a custom bash script.
    -- This is because the built-in volume support in xmobar is disabled
    -- in Debian and derivatives like Ubuntu.
    -- Run Com "/bin/bash" ["-c", "~/.config/xmonad/get-volume"] "myvolume" 10,
    -- Get mpris player
    Run $ Com "/bin/bash" ["-c", "~/bin/my-player-get"] "myplayer" 10,
    Run $ Com "echo" [fn2 "\xf144"] "playicon" 3600,
    Run $ Com "/bin/bash" ["-c", "~/bin/speaker-color #e6744c"] "speaker-color" 10,

    Run $ Alsa "default" "Master" [
        -- "-t", "<fn=2>\xf028</fn>: <volume>% <status>",
        "-t", fn2 "\xf028" <> ": <volume>% <status>",
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
    ]


batteryPlugin :: Runnable
batteryPlugin =
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
        ] 20

myTemplate :: Bool -> String
-- myTemplate =  xmonadLog `align` iconRow
myTemplate hasBatt = xmonadLog `align` iconRow hasBatt

    where
        xmonadLog :: String
        xmonadLog = xmonadIcon `sp_` arg "UnsafeXMonadLog"

        iconRow :: Bool -> String
        iconRow True = iconRowAddBatt batt
        iconRow False = iconRowAddBatt ""

        iconRowAddBatt :: String -> String
        iconRowAddBatt batt_ = calender <> spaces 4 <>
                    foldl' (.|) batt_ icons
        icons :: [String] 
        icons = [arg "cpu", arg "memory", sp (arg "disku"), speaker, player]



action :: String -> String -> String
action command text = "<action=`" <> command <> "`>" <> text <> "</action>"

fc, fontColor :: String -> String -> String
fontColor clr text = "<fc=" <> clr <> ">" <> text <> "</fc>"
fc = fontColor

icon :: String -> String
icon name = "<icon=" <> name <> "/>"

fn :: Int -> String -> String
fn n text = "<fn=" <> show n <> ">" <> text <> "</fn>"

fn1, fn2, fn3, fn4, fn5 :: String -> String
fn1 = fn 1
fn2 = fn 2
fn3 = fn 3
fn4 = fn 4
fn5 = fn 5

argument :: String -> String -> String
argument sepChar' text = sepChar' <> text <> sepChar'

alignSeperator :: String -> String -> String -> String
alignSeperator sep l r = l `sp_` sep `sp_` r

align :: String -> String -> String
align = alignSeperator $ alignSep config

arg :: String -> String
arg = argument $ sepChar config

space :: String -> String
space text = " " <> text

spaces :: Int -> String
spaces n = replicate n ' '

sp :: String -> String
sp = space

sp_ :: String -> String -> String
sp_ l r = l <> sp r

(.|), bar :: String -> String -> String
bar "" r = r
bar l ""  = l
bar l r = l `sp_` "|" `sp_` r
(.|) = bar

infixl 5 .|

xmonadIcon :: String
xmonadIcon = action "xdotool key alt+n" $ icon "xmonad-16.xpm"

calender :: String
calender = action "osmo -c" $ fontColor "#eed77c" $ arg "date"

speaker :: String
speaker = action "speaker-toggle" $ fontColor (arg "speaker-color") $ arg "alsa:default:Master"

player :: String
player = action "my-player-next" $ fc "#6c97d4" $ arg "playicon" <> " " <> arg "myplayer"

batt :: String
batt = arg "baticon" `sp_` arg "battery"


deviceHasBattery :: IO Bool
deviceHasBattery = not . null <$> listDirectory "/sys/class/power_supply"

--- >>> deviceHasBattery 
-- True

addBattery :: Config -> IO Config
addBattery cfg = do
    hasBatt <- deviceHasBattery
    return $ if hasBatt
        then cfg{template = myTemplate hasBatt, commands = batteryPlugin:myCommands}
        else cfg{template = myTemplate hasBatt, commands = myCommands}

isLaptop :: IO Bool
isLaptop = do 
    host <- getHostName
    return $ host == "paul-linux-g"

modifyForLaptop :: Config -> IO Config
modifyForLaptop cfg = do
    laptop <- isLaptop
    return $ if laptop 
        then cfg {position = TopSize L 85 16}
        else cfg


main :: IO ()
main = do 
    addBattery config 
        >>= modifyForLaptop
        >>= configFromArgs 
        >>= xmobar
