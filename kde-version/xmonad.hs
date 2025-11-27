{-# LANGUAGE DeriveDataTypeable, TypeApplications, LambdaCase #-}

import XMonad hiding ((|||))

import XMonad.Actions.OnScreen
import XMonad.Actions.CycleWS
import XMonad.Actions.SpawnOn

import XMonad.Util.EZConfig
-- import XMonad.Util.Loggers -- Removed: not used
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
import XMonad.Hooks.ManageHelpers
-- import XMonad.Hooks.DynamicLog -- Removed: not used without Xmobar/Polybar

import Data.Monoid
import qualified Data.Map as M

import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

--------------------------------------------------------------------------------------
-- Key Toggle (Unchanged)
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
-- Main (Modified)
--------------------------------------------------------------------------------------

main :: IO ()
main = xmonad
     . ewmhFullscreen -- Use ewmhFullscreen instead of just ewmh for full-screen compatibility
     . ewmh           -- Apply ewmh for general compatibility
     . docks          -- **Crucial**: Manages docking panels like the KDE panel, prevents tiling over them
     $ myConfig

-- Removed statusBar, as we're relying on the KDE panel

myConfig = def
    { modMask  = mod1Mask  -- Rebind Mod to the Super key
    , keys = makeToggleable xK_Insert myKeys
    , mouseBindings = myMouseBindings
    , terminal = myTerminal
    , logHook = myLogHook
    , layoutHook = avoidStruts $ (smartBorders (myLayout))  -- Use custom layouts
    , manageHook = manageSpawn <> myManageHook -- Match on certain windows
    , startupHook = myStartupHook
    , focusFollowsMouse = myFocusFollowsMouse
    , clickJustFocuses  = myClickJustFocuses
    , borderWidth = myBorderWidth
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , workspaces         = myWorkspaces
    }

--------------------------------------------------------------------------------------
-- myKeys / myMouseBindings (Unchanged - using original definitions)
--------------------------------------------------------------------------------------

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm                 , xK_Return), spawn $ myTerminal)
    , ((modm                 , xK_a  ), prevWS)
    , ((modm                 , xK_d  ), spawn $ "rofi -show drun")
    , ((modm                 , xK_s  ), nextWS)
    , ((modm                 , xK_q  ), kill)
    , ((modm .|.   shiftMask , xK_a  ), shiftToPrev)
    , ((modm                 , xK_c  ), sendMessage NextLayout)
    , ((modm                 , xK_f  ), sendMessage (Toggle "Full"))
    , ((modm .|.   shiftMask , xK_s  ), shiftToNext)
    , ((modm                 , xK_z  ), sendMessage (IncMasterN 1))
    , ((modm                 , xK_x  ), sendMessage (IncMasterN (-1)))
    , ((modm .|.   shiftMask , xK_r  ), spawn "xmonad --recompile; xmonad --restart")
    , ((modm .|. controlMask , xK_r  ), spawn "xmonad --restart")
    , ((modm                 , xK_m  ), windows W.focusMaster)
    , ((modm                 , xK_r  ), setLayout $ XMonad.layoutHook conf)
    , ((modm                 , xK_h  ), sendMessage Shrink)
    , ((modm                 , xK_p  ), withFocused $ windows . W.sink)
    , ((modm                 , xK_j  ), windows W.focusDown)
    , ((modm .|.   shiftMask , xK_j  ), windows W.swapDown)
    , ((modm                 , xK_k  ), windows W.focusUp)
    , ((modm .|.   shiftMask , xK_k  ), windows W.swapUp)
    , ((modm                 , xK_l  ), sendMessage Expand)
    , ((modm .|.   shiftMask , xK_m  ), windows W.swapMaster)
    , ((modm                 , xK_F1  ), spawn "ibus engine xkb:it::ita")
    , ((modm                 , xK_F2  ), spawn "ibus engine xkb:us::eng")
    , ((modm                 , xK_F3  ), spawn "ibus engine xkb:ru::rus")
    , ((modm                 , xK_F4  ), spawn "ibus engine xkb:jp:kana:jpn")

    , ((modm                 , xK_r  ), spawn "sh ~/.welcome.sh")   

    , ((modm                , xK_o), spawn "pactl set-sink-volume 0 +10%")
    , ((modm                , xK_i), spawn "pactl set-sink-volume 0 -10%")
    , ((modm                , xK_u), spawn "pactl set-sink-mute 0 toggle")
    , ((modm                , xK_Prior), spawn "sh -c 'xfce4-screenshooter --region --save ~/Immagini/Schermate/Screenshot_$(date +%Y-%m-%d_%H-%M-%S).png'")
    , ((modm                , xK_Next), spawn "sh -c 'xfce4-screenshooter --fullscreen --save ~/Immagini/Schermate/Screenshot_$(date +%Y-%m-%d_%H-%M-%S).png'")]

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    ]

--------------------------------------------------------------------------------------
-- Layouts (Unchanged)
--------------------------------------------------------------------------------------

myLayout = toggleLayouts (noBorders Full) (tiled ||| tabbed shrinkText myTabConfig ||| Mirror tiled)
  where
    -- threeCol = ThreeColMid nmaster delta ratio -- Removed: Not used in myLayout definition
    tiled    = Tall nmaster delta ratio
    nmaster  = 1
    ratio    = 1/2
    delta    = 5/100

myTabConfig = def { activeBorderColor = "#0000FF"
                  , inactiveBorderColor = "#000000"
                  , activeColor = "#0000FF"
                  , inactiveColor = "#000000"
                  , activeTextColor = "#FFFFFF"
                  , inactiveTextColor = "#FFFFFF"
                  , fontName = "xft:Bitstream Vera Sans Mono:size=9:bold:antialias=true"}

--------------------------------------------------------------------------------------
-- Hooks (Modified myManageHook, Simplified myLogHook)
--------------------------------------------------------------------------------------

myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    -- Add rules to explicitly manage KDE-related windows to prevent issues:
    , className =? "plasmashell"    --> doIgnore
    , isDialog                     --> doFloat
    ]

myLogHook = return () -- No external bar, so the logHook can remain simple

myStartupHook = do
    -- Removed "xhost +si:localuser:$USER &" as it's often unnecessary and potentially a security risk
    spawnOnce "xrandr --output HDMI-1-0 --mode 1920x1080 --rate 60 --right-of eDP-1 &"
    spawnOnce "xmodmap -e 'keycode 157 = Prior' &"
    spawnOnce "xmodmap -e 'keycode 107 = Next' &"
    spawnOnce "nitrogen --restore &"
    spawnOnce "/usr/lib/pentablet/PenTablet.sh /mini &"
    spawnOnce "/usr/libexec/polkit-agent-helper-1 &"
    spawnOnce "xfce4-power-manager &"

--------------------------------------------------------------------------------------
-- Variables (Unchanged)
--------------------------------------------------------------------------------------

myTerminal = "/usr/bin/alacritty"
myWorkspaces    = ["1","2","3","4","5","6","7","8","9","0"]
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True
myClickJustFocuses :: Bool
myClickJustFocuses = False
myBorderWidth = 1
myNormalBorderColor  = "#444444"
myFocusedBorderColor = "#0000FF"

-- Removed myPP definition as DynamicLog/Xmobar are no longer used.
