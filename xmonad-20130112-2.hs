import System.IO
import System.Posix.Env

import Control.Monad                   (liftM2)

import Data.List                       (isPrefixOf, isInfixOf)
import Data.Map                        (fromList, union)  

import XMonad
import XMonad.Core
import XMonad.Config
import XMonad.Operations

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.FadeInactive

import XMonad.Layout
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ResizableTile
import XMonad.Layout.StackTile

import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.Cursor
import XMonad.Util.Scratchpad

import XMonad.Actions.Volume
import XMonad.Actions.CopyWindow

--import qualified Data.Map as M
import qualified XMonad.StackSet as W

myTerminal = "xterm"

myModMask = mod4Mask

myManageHook = composeAll
  [  className =? "stalonetray"	                     --> doIgnore
  ,  className =? "trayer"                           --> doIgnore
  ,  className =? "Pidgin" <&&> title=? "Buddy List" --> doFloat <+> doShift "float"
  ,  className =? "Pidgin"                           --> doFloat
  ,  className =? "Gimp"                             --> doFloat <+> doShift "8"
  ,  fmap ( "Emacs" `isInfixOf` ) className          --> doShift "emacs"
  ,  fmap ( "Skype" `isInfixOf` ) className          --> doFloat <+> doShift "float"
  ,  className =? "Eclipse"                          --> doShift "code"
  ,  className =? "XTerm" <&&> title =? "Emacs"      --> doShift "emacs"
  ,  scratchpadManageHookDefault
  ,  fmap ( "LibreOffice" `isInfixOf` ) (stringProperty "WM_NAME")      --> doShift "office"
  ,  className =? "TweetDeck" <&&> stringProperty "WM_ICON_NAME" =? ""	--> doFloat <+> doIgnore
  ,  className =? "TweetDeck" <&&> title =? "TweetDeck Settings"        --> doFloat
  ,  className =? "TweetDeck"                                           --> doShift "tweet"
  ]

myWorkspaces = [ "web", "emacs", "code", "vm", "office", "float", "xterms", "8", "9", "0"]

myKeys conf@(XConfig {XMonad.modMask = modm}) = fromList $
  [ ((modm                , xK_a          ), sendMessage MirrorShrink)
  , ((modm                , xK_z          ), sendMessage MirrorExpand)
  , ((mod1Mask            , xK_F2         ), scratchpadSpawnAction conf)  
  , ((modm                , xK_F5         ), lowerVolume 4 >> return ())
  , ((modm                , xK_F6         ), raiseVolume 4 >> return ())
  -- Restart xcompmgr because it gets confused often
  , ((modm                , xK_F11        ), spawn "bin/restart-xcompmgr.sh")
  , ((modm                , xK_F12        ), spawn "xscreensaver-command -lock") 
  , ((modm .|. controlMask, xK_F12        ), spawn "xscreensaver-command -lock") 
  -- Urgency hints
  , ((modm                , xK_BackSpace), focusUrgent)
  , ((modm .|. shiftMask  , xK_BackSpace), clearUrgents)
  , ((0                   , xK_Print      ), spawn "scrot")
  -- Built-in volume keys
  , ((0                   , 0x1008ff11    ), lowerVolume 4 >> return ())  -- Keyboard volume down
  , ((0                   , 0x1008ff13    ), raiseVolume 4 >> return ())  -- Keyboard volume up
  , ((0                   , 0x1008ff12    ), toggleMute >> return ())     -- Keyboard volume mute
  -- Eat Windows key to (useful for Windows VMs) 
  , ((0                   , xK_Super_L    ), return())   
  ]
  ++
  [ ((m .|. modm, k), windows $ f i)    -- Copy window
  | (i, k) <- zip (workspaces conf) [ xK_1 ..]
  , (f, m) <- [(W.view, 0), (W.shift, shiftMask), (copy, shiftMask .|. controlMask)]
  ]
  ++
  [ ((modm .|. shiftMask, xK_c ), kill1)
  , ((modm              , xK_v ), windows copyToAll)
  , ((modm .|. shiftMask, xK_v ), killAllOtherCopies )
  ]
  
myStartupHook = setDefaultCursor xC_left_ptr


myFont = "Inconsolata"
myFgColor = "#DCDCCC"
myBgColor = "#3f3f3f"
--myStatusBar = "xmobar /home/marc/.xmobarrc"
myStatusBar = "xmobar"

myLayoutHook = avoidStruts $ standardLayouts
  where standardLayouts = tiled ||| Mirror tiled ||| Full
        tiled = ResizableTall nmaster delta ratio []
        nmaster = 1 
        delta = 0.03
        ratio = 0.6

--myLogHook :: Handle -> X ()
myLogHook myStatusBarPipe =  do
  copies <- wsContainingCopies
  let check ws | ws `elem` copies = pad . xmobarColor "red" "black" $ ws
               | otherwise = ws
  dynamicLogWithPP $ xmobarPP { ppHidden = check
                              , ppOutput = hPutStrLn myStatusBarPipe
                              , ppUrgent = xmobarColor "white" "red"
                              , ppTitle = xmobarColor "green" "" . shorten 120
                              }
  fadeInactiveLogHook fadeAmount
    where fadeAmount = 0.6

--main = do
--  myStatusBarPipe <- spawnPipe myStatusBar
--  xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
--                { terminal	= myTerminal
--                , workspaces	= myWorkspaces
--                , modMask	= myModMask
--                , layoutHook	= myLayoutHook
--                , manageHook	= myManageHook
--                , keys		= liftM2 union myKeys (keys defaultConfig)
--                , startupHook	= myStartupHook
--                , logHook	= myLogHook myStatusBarPipe
--                }

main = xmonad =<< xmobar myConfig

myConfig = withUrgencyHook NoUrgencyHook $ defaultConfig
                { terminal	= myTerminal
                , workspaces	= myWorkspaces
                , modMask	= myModMask
                , layoutHook	= myLayoutHook
                , manageHook	= myManageHook
                , keys		= liftM2 union myKeys (keys defaultConfig)
                , startupHook	= myStartupHook
--                , logHook	= myLogHook myStatusBarPipe
                }
