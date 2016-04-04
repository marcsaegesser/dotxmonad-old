{-# LANGUAGE OverloadedStrings #-}

import System.IO

import Control.Monad                   (liftM2)

import Data.List                       (isInfixOf)
import Data.Map                        (fromList, union)

import XMonad

import qualified XMonad.Actions.CycleWS as CWS

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.FadeInactive

import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Mosaic
import XMonad.Layout.ThreeColumns

import XMonad.Util.Run
import XMonad.Util.Cursor
import XMonad.Util.NamedScratchpad
import XMonad.Util.Scratchpad
import XMonad.Util.SpawnOnce

import XMonad.Actions.Warp
--import XMonad.Actions.Volume
import XMonad.Actions.CopyWindow

import qualified XMonad.StackSet as W

import XMonad.Prompt
import XMonad.Prompt.Shell

import DBus
import DBus.Client

-- equivalent to spawn "dbus-send --print-reply --dest=com.spotify.qt /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player" ++ cmd
mprisCommand :: String -> X ()
mprisCommand cmd = catchIO $ do
	dbusSession <- connectSession
	reply <- call_ dbusSession (methodCall "/org/mpris/MediaPlayer2" "org.mpris.MediaPlayer2.Player" (memberName_ cmd))
                                   { methodCallDestination = Just "org.mpris.MediaPlayer2.spotify" }
	disconnect dbusSession

main = do
  myStatusBarPipe <- spawnPipe myStatusBar
  xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
                { terminal	= myTerminal
                , workspaces	= myWorkspaces
                , modMask	= myModMask
                , layoutHook	= myLayoutHook
                , manageHook	= myManageHook <+> manageDocks
                , handleEventHook = docksEventHook
                , keys		= liftM2 union myKeys (keys defaultConfig)
                , startupHook	= myStartupHook
                , logHook	= myLogHook myStatusBarPipe
                , focusFollowsMouse = False
                }


myTerminal = "xterm"

myModMask = mod4Mask

-- List of X11 window classes which should never steal focus
noStealFocusWins = ["Pidgin"]

myManageHook = composeAll
  [  isDialog                                        --> doFloat
  ,  className =? "stalonetray"	                     --> doIgnore
  ,  className =? "trayer"                           --> doIgnore
  ,  className =? "Pidgin" <&&> title=? "Buddy List" --> doFloat <+> doShift "float"  -- Pidgen Buddy List to the Float workspace
  ,  className =? "Pidgin"                           --> doFloat                      -- Pidgen Conversation windows float anywhere
  ,  className =? "Spotify"                          --> doFloat <+> doShift "float"  -- Spotify to the float workspace
  ,  className =? "Wicd-client.py"                   --> doFloat
  ,  className =? "Gimp"                             --> doFloat <+> doShift "8"
  ,  fmap ( "Cinelerra" `isInfixOf` ) className      --> doFloat
  ,  fmap ( "Emacs" `isInfixOf` ) className          --> doShift "emacs"
  ,  fmap ( "Skype" `isInfixOf` ) className          --> doFloat <+> doShift "float"
  ,  className =? "Eclipse"                          --> doShift "code"
  ,  className =? "XTerm" <&&> title =? "Emacs"      --> doShift "emacs"
  ,  fmap ( "LibreOffice" `isInfixOf` ) (stringProperty "WM_NAME")      --> doShift "office"
  ,  scratchpadManageHookDefault
  ,  namedScratchpadManageHook myScratchpads
  ,  composeAll [className =? c --> doF W.focusDown | c <- noStealFocusWins]  ]

myScratchpads = [
      NS "htop" "xterm -name htop -e htop" (title =? "htop") (customFloating $ W.RationalRect (1/4) (1/6) (1/2) (2/3))
    , NS "nautilus" "nautilus --no-desktop" (className =? "Nautilus") (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
    ] where role = stringProperty "WM_WINDOW_ROLE"

myWorkspaces = [ "web", "emacs", "code", "vm", "office", "float", "xterms", "8", "9", "0"]

-- NOTE:  .|. is bitwise OR
myKeys conf@(XConfig {XMonad.modMask = modm}) = fromList $
  [ ((modm                , xK_a          ), sendMessage MirrorShrink)
  , ((modm                , xK_z          ), sendMessage MirrorExpand)
  , ((modm                , xK_g          ), warpToWindow (1/2) (1/2))  -- Move pointer to focused window
  , ((modm                , xK_n          ), namedScratchpadAction myScratchpads "nautilus")
  , ((modm                , xK_o          ), namedScratchpadAction myScratchpads "htop")
  , ((modm                , xK_s          ), shellPrompt myXPConfig)
  , ((modm                , xK_F2         ), scratchpadSpawnAction conf)
--  , ((modm                , xK_F5         ), lowerVolume 4 >> return ())
--  , ((modm                , xK_F6         ), raiseVolume 4 >> return ())
  , ((modm                , xK_F10        ), spawn "bin/stop-compton.sh")
  , ((modm                , xK_F11        ), spawn "bin/restart-compton.sh")
  , ((modm                , xK_F12        ), spawn "xscreensaver-command -lock")
  , ((modm .|. controlMask, xK_j          ), CWS.nextWS)    -- Cycle through workspaces
  , ((modm .|. controlMask, xK_k          ), CWS.prevWS)
  , ((modm                , xK_BackSpace  ), focusUrgent)     -- Urgency hints
  , ((modm .|. shiftMask  , xK_BackSpace  ), clearUrgents)
  , ((0                   , xK_Print      ), spawn "scrot")
  , ((modm                , xK_b          ), sendMessage ToggleStruts)
  , ((0                   , 0x1008ff16    ), mprisCommand "Previous")  -- previous track
  , ((0                   , 0x1008ff14    ), mprisCommand "PlayPause") -- play
  , ((0                   , 0x1008ff17    ), mprisCommand "Next")      -- next track
--  , ((0                   , 0x1008ff11    ), lowerVolume 4 >> return ())  -- Keyboard volume down
--  , ((0                   , 0x1008ff13    ), raiseVolume 4 >> return ())  -- Keyboard volume up
--  , ((0                   , 0x1008ff12    ), toggleMute >> return ())     -- Keyboard volume mute
  ]
  ++
  [ ((m .|. modm, k), windows $ f i)    -- Shift/Copy window
  | (i, k) <- zip myWorkspaces $ [ xK_1..xK_9 ] ++ [ xK_0 ]
  , (f, m) <- [ (W.view , 0                         )   -- No modifier -> switch to new workspace
              , (W.shift, shiftMask                 )   -- Shift modifier -> Shift window to new workspace
              , (copy   , shiftMask .|. controlMask )   -- Ctrl-Shift modifier -> Copy window to new workspace
              ]
  ]
  ++
  [ ((modm .|. shiftMask, xK_c ), kill1)
  , ((modm              , xK_v ), windows copyToAll)
  , ((modm .|. shiftMask, xK_v ), killAllOtherCopies )
  ]

--myStartupHook = setDefaultCursor xC_left_ptr
myStartupHook = do
	setDefaultCursor xC_left_ptr
        spawnOnce "feh --bg-scale `feh -U -z Pictures/IMG_4113-new4.jpg | head -1`"
        spawnOnce "trayer --edge bottom --align right --height 16 --width 192 --widthtype pixel --transparent true"
        spawnOnce "xscreensaver -no-splash"
        spawnOnce "compton"
        spawnOnce "autocutsel -fork"
        spawnOnce "autocutsel -t"
	spawnOnce "dropbox start"
	spawnOnce "SpiderOak"

--Xmobar is a script in ~/bin that kills the currently running xmobar and
--launches a new instance.  Without this xmonad --restart loses xmobar.
--There ought to be a better way.
myStatusBar = "Xmobar ~/.xmobarrc"

myLayoutHook = avoidStruts $ smartBorders $ standardLayouts
  where standardLayouts = tiled ||| mosaic 2 [3,2] ||| Mirror tiled ||| ThreeCol 1 (3 / 100) (1 / 2) ||| Full
        tiled = ResizableTall nmaster delta ratio []
        nmaster = 1
        delta = 0.03
        ratio = 0.6

myLogHook myStatusBarPipe =  do
  copies <- wsContainingCopies
  let check ws | ws == "NSP" = ""                                   -- Hide the scratchpad workspace
               | ws `elem` copies = xmobarColor "red" "black" $ ws  -- Workspaces with copied windows are red on black
               | otherwise = ws
  dynamicLogWithPP $ xmobarPP { ppHidden = check
                              , ppOutput = hPutStrLn myStatusBarPipe
                              , ppUrgent = xmobarColor "white" "red"
                              , ppTitle  = xmobarColor "green" "" . shorten 180
                              }
  fadeInactiveLogHook 0.6



myXPConfig = defaultXPConfig { position = Top
                             , searchPredicate = isInfixOf  -- This doesn't seem to work, I'm still only getting prefix matches
                             }
