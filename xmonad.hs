{-# LANGUAGE DeriveDataTypeable, TypeApplications, LambdaCase #-}

import XMonad hiding ((|||))

import XMonad.Actions.OnScreen
import XMonad.Actions.CycleWS
import XMonad.Actions.SpawnOn

import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.SpawnOnce
import XMonad.Util.Run
import qualified XMonad.Util.ExtensibleState as XS
import qualified XMonad.Util.Paste as XP

import XMonad.Layout.ToggleLayouts
import XMonad.Layout.NoBorders
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.TwoPane
import XMonad.Layout.Combo
import XMonad.Layout.WindowNavigation

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog

import Data.Monoid
import qualified Data.Map as M

import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

--------------------------------------------------------------------------------------

data KeysToggle = KeysEnabled | KeysDisabled deriving Typeable
instance ExtensionClass KeysToggle where
    initialValue = KeysEnabled

makeToggleable :: KeySym -> (XConfig Layout -> M.Map (KeyMask, KeySym) (X ())) -> XConfig Layout -> M.Map (KeyMask, KeySym) (X ())

makeToggleable togSym origKeys conf =
    M.insert (0, togSym) toggleKeys $ M.mapWithKey ifKeys (origKeys conf)
  where

    ifKeys :: (KeyMask, KeySym) -> X () -> X ()
    ifKeys (km, ks) act = XS.get @KeysToggle >>= \case
      KeysEnabled -> act
      KeysDisabled -> XP.sendKey km ks

    toggleKeys :: X ()
    toggleKeys = XS.modify @KeysToggle $ \case
      KeysEnabled -> KeysDisabled
      KeysDisabled -> KeysEnabled

--------------------------------------------------------------------------------------

main :: IO ()
main = xmonad
     . ewmh
     =<< statusBar "xmobar" def toggleStrutsKey myConfig
--     $ myConfig
  where
    toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
    toggleStrutsKey XConfig{ modMask = m } = (m, xK_w)

myConfig = def
    { modMask  = mod1Mask  -- Rebind Mod to the Super key
    , keys = makeToggleable xK_Insert myKeys
    , mouseBindings = myMouseBindings
    , terminal = myTerminal
    , logHook = myLogHook
    , layoutHook = smartBorders (myLayout)  -- Use custom layouts
    , manageHook = manageSpawn <> myManageHook -- Match on certain windows
    , startupHook = myStartupHook
    , focusFollowsMouse = myFocusFollowsMouse
    , clickJustFocuses  = myClickJustFocuses
    , borderWidth = myBorderWidth
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , workspaces         = myWorkspaces
    }

--------------------------------------------------------------------------------------

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm                 , xK_Return), spawn $ myTerminal)
    , ((modm                 , xK_a  ), prevWS)
    , ((modm                 , xK_d  ), spawn $ "rofi -show drun")
    , ((modm                 , xK_s  ), nextWS)
    , ((modm                 , xK_q  ), kill)
    , ((modm .|.   shiftMask , xK_a  ), shiftToPrev)
    , ((modm                 , xK_c  ), sendMessage NextLayout)
    , ((modm                 , xK_f  ), sendMessage (Toggle "Full"))
    , ((modm .|.   shiftMask , xK_s  ), shiftToNext)
    , ((modm                 , xK_z  ), sendMessage (IncMasterN 1))
    , ((modm .|.   shiftMask , xK_q  ), io (exitWith ExitSuccess))
    , ((modm                 , xK_x  ), sendMessage (IncMasterN (-1)))
    , ((modm .|.   shiftMask , xK_r  ), spawn "xmonad --recompile; xmonad --restart")
    , ((modm .|. controlMask , xK_r  ), spawn "xmonad --restart")
    , ((modm                 , xK_m  ), windows W.focusMaster)
    , ((modm                 , xK_r  ), setLayout $ XMonad.layoutHook conf)
    , ((modm                 , xK_h  ), sendMessage Shrink)
    , ((modm                 , xK_p  ), withFocused $ windows . W.sink)
    , ((modm                 , xK_j  ), windows W.focusDown)
    , ((modm .|.   shiftMask , xK_j  ), windows W.swapDown)
    , ((modm                 , xK_k  ), windows W.focusUp)
    , ((modm .|.   shiftMask , xK_k  ), windows W.swapUp)
    , ((modm                 , xK_l  ), sendMessage Expand)
    , ((modm .|.   shiftMask , xK_m  ), windows W.swapMaster)
    , ((modm                 , xK_1  ), spawn "ibus engine xkb:it::ita")
    , ((modm                 , xK_2  ), spawn "ibus engine xkb:us::eng")
    , ((modm                 , xK_3  ), spawn "ibus engine xkb:ru::rus")
    , ((modm                 , xK_4  ), spawn "ibus engine xkb:jp::jpn")

    , ((modm                 , xK_r  ), spawn "sh ~/.welcome.sh")

    , ((modm                , xK_o), spawn "pactl set-sink-volume 0 +10%")
    , ((modm                , xK_i), spawn "pactl set-sink-volume 0 -10%")
    , ((modm                , xK_u), spawn "pactl set-sink-mute 0 toggle")
    , ((modm                , xK_Prior), spawn "sh -c 'xfce4-screenshooter --region --save ~/Immagini/Schermate/Screenshot_$(date +%Y-%m-%d_%H-%M-%S).png'")
    , ((modm                , xK_Next), spawn "sh -c 'xfce4-screenshooter --fullscreen --save ~/Immagini/Schermate/Screenshot_$(date +%Y-%m-%d_%H-%M-%S).png'")]

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    ]

--------------------------------------------------------------------------------------

myLayout = toggleLayouts (noBorders Full) (tiled ||| tabbed shrinkText myTabConfig ||| Mirror tiled) -- ||| combineTwo (TwoPane (3/100) (1/2)) myTabbed tall
  where
    tall = Tall 1 (3/100) (1/2)
    threeCol = ThreeColMid nmaster delta ratio
    tiled    = Tall nmaster delta ratio
    nmaster  = 1      -- Default number of windows in the master pane
    ratio    = 1/2    -- Default proportion of screen occupied by master pane
    delta    = 5/100  -- Percent of screen to increment by when resizing panes
--  myTabbed = tabbed shrinkText myTabConfig

myTabConfig = def { activeBorderColor = "#0000FF"
                  , inactiveBorderColor = "#000000"
                  , activeColor = "#0000FF"
                  , inactiveColor = "#000000"
                  , activeTextColor = "#FFFFFF"
                  , inactiveTextColor = "#FFFFFF"
                  , fontName = "xft:Bitstream Vera Sans Mono:size=9:bold:antialias=true"}

--------------------------------------------------------------------------------------

myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]



myLogHook = return ()

myStartupHook = do
    spawnOnce "xrandr --output HDMI-1-0 --mode 1920x1080 --rate 60 --right-of eDP-1 &"
    spawnOnce "xmodmap -e 'keycode 157 = Prior' &"
    spawnOnce "xmodmap -e 'keycode 107 = Next' &"
    spawnOnce "nitrogen --restore &"
    spawnOnce "/usr/lib/pentablet/PenTablet.sh /mini &"
    spawnOnce "/usr/libexec/polkit-agent-helper-1 &"
    spawnOnce "xhost +si:localuser:$USER &"
    spawnOnce "xfce4-power-manager &"

--------------------------------------------------------------------------------------

myTerminal = "/usr/local/bin/alacritty"
myWorkspaces    = ["1","2","3","4","5","6","7","8","9","0"]
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True
myClickJustFocuses :: Bool
myClickJustFocuses = False
myBorderWidth = 1
myNormalBorderColor  = "#444444"
myFocusedBorderColor = "#0000FF"

-- My Big PP -------------------------------------------------------------------------

myPP :: PP
myPP =  def
    { ppSep             = white " • "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = wrap " " ""-- . xmobarBorder "Top" "#8be9fd" 2
    , ppHidden          = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder = \(ws : l : _ : wins : _) -> [ws, l, wins]
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#FF00FF" ""
    blue     = xmobarColor "#0000FF" ""
    white    = xmobarColor "#FFFFFF" ""
    yellow   = xmobarColor "#FFFF00" ""
    red      = xmobarColor "#FF0000" ""
    lowWhite = xmobarColor "#bbbbbb" ""
