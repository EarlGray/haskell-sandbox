import XMonad
import qualified XMonad.StackSet as W
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Monitor
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.WindowArranger
import XMonad.Doc.Extending
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Actions.MouseResize
import XMonad.Actions.WindowNavigation
import XMonad.Actions.CycleWS

import Data.Maybe (fromJust)
import qualified Data.List as L

modm = mod4Mask --mod4Mask

myLayoutHooks = avoidStruts . mouseResize . windowArrange 
myLayouts = simpleTabbed ||| ThreeColMid 1 (3/100) (1/2)

myWorkspaces = ["1:work", "2:workr",   "3:wskype", "4:wweb",   "5:vms", "6:vmsr",
                "7:wvlc", "8:wvlcr",   "9:mdev", "0:mskype",   "-", "="]
vertDesks = 3
horDesks = 2
nscreens = 2

dbg  = withWindowSet $ \wset ->
  spawn $ "kdialog --msgbox \"" ++ (show (W.current wset)) ++ "\""

myUpWS = --windows id
  windows $ \ws -> 
    let offset = vertDesks * nscreens
        c = W.currentTag ws
        ci = fromJust . L.findIndex (== c) $ myWorkspaces
        basei = (ci `div` nscreens) * nscreens
        nbasei = let nbasei' = basei - offset
                 in if nbasei' < 0
                    then horDesks * vertDesks * nscreens + nbasei'
                    else nbasei'
        nc = nbasei + (ci `mod` nscreens)
        nw = myWorkspaces !! nc

        iter i ws = W.greedyView (myWorkspaces !! i) ws
    in W.greedyView nw $ foldr iter ws [nbasei .. (nbasei + nscreens - 1)]
-- -}

myShortcuts = [
  ((modm,           xK_grave),    nextScreen),
  ((modm .|. shiftMask, xK_grave), shiftNextScreen),

  ((modm .|. shiftMask,           xK_Left),     prevWS),
  ((modm .|. shiftMask,           xK_Right),    nextWS),

  ((modm,           xK_Up),       myUpWS),

  ((modm,           xK_0),        windows $ W.greedyView "0:mskype"),
  ((modm,           xK_minus),    windows $ W.greedyView "-"),
  ((modm,           xK_equal),    windows $ W.greedyView "="),

  ((modm,           xK_z),        dbg),

  ((modm .|. shiftMask, xK_l),    spawn "/usr/lib/kde4/libexec/kscreenlocker --forcelock") ]

manageFloating = composeAll [ 
  className =? "XClock"   --> doIgnore,
  className =? "Yakuake"   --> doFloat,
  className =? "KMix"     --> doFloat,
  className =? "Gimp"     --> doFloat,
  className =? "Skype"    --> doFloat,
  className =? "Tasks"    --> doFloat ]

main = do
  spawn "/home/mitra/.xmonad/startup.sh"
  myConf <- withWindowNavigation (xK_w, xK_a, xK_s, xK_d) $ defaultConfig {
    workspaces = myWorkspaces,

    borderWidth = 3,
    focusedBorderColor = "#ff4000",
    terminal = "konsole",
    modMask = modm,
    focusFollowsMouse = False,

    manageHook = manageFloating <+> manageHook defaultConfig <+> manageDocks,
    layoutHook = myLayoutHooks $ layoutHook defaultConfig ||| myLayouts 
  } `additionalKeys` myShortcuts
  xmonad myConf
