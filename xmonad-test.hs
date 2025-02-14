import XMonad  
import XMonad.Config.Azerty  
import XMonad.Hooks.DynamicLog  
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks  
import XMonad.Util.Run(spawnPipe)  
import XMonad.Util.EZConfig  
import Graphics.X11.ExtraTypes.XF86  
import XMonad.Layout.Spacing  
import XMonad.Layout.NoBorders(smartBorders)  
import XMonad.Layout.PerWorkspace  
import XMonad.Layout.IM  
import XMonad.Layout.Grid  
--import XMonad.Actions.GridSelect  
import Data.Ratio ((%))  
import XMonad.Actions.CycleWS  
import qualified XMonad.StackSet as W  
import System.IO  
myWorkspaces  = ["1:main","2:chat","3:trades","4:Pidgin","5:graphic","6:files", "7:media"]  
myLayout = onWorkspace "4:Pidgin" pidginLayout $ onWorkspaces ["2:chat", "7:media"] nobordersLayout $ tiled1 ||| Mirror tiled1 ||| nobordersLayout  
 where  
   tiled1 = spacing 5 $ Tall nmaster1 delta ratio  
   --tiled2 = spacing 5 $ Tall nmaster2 delta ratio  
   nmaster1 = 1  
   nmaster2 = 2  
   ratio = 2/3  
   delta = 3/100  
   nobordersLayout = smartBorders $ Full  
   gridLayout = spacing 8 $ Grid       
   --gimpLayout = withIM (0.20) (Role "gimp-toolbox") $ reflectHoriz $ withIM (0.20) (Role "gimp-dock") Full  
   pidginLayout = withIM (18/100) (Role "buddy_list") gridLayout  
myManageHook = composeAll       
      [ className =? "File Operation Progress"   --> doFloat  
      , resource =? "desktop_window" --> doIgnore  
      , className =? "xfce4-notifyd" --> doIgnore  
      --, className =? "Iron" --> doShift "1:main"  
      , className =? "Firefox" --> doShift "3:trades"  
      , className =? "Xchat" --> doShift "2:chat"  
      , className =? "Pidgin" --> doShift "4:Pidgin"  
      , className =? "Shotwell" --> doShift "5:graphic"  
      --, className =? "Gimp" --> doShift "5:graphic"  
      --, className =? "Vlc" --> doShift "7:media"  
      --, className =? "Minitube" --> doShift "7:media"  
      ]  
main = do  
   xmproc <- spawnPipe "/usr/bin/xmobar /home/lulz/scripts/xmobarrc"  
   --spawn "xcompmgr -Cf"  
   xmonad $ azertyConfig  
     { manageHook = manageDocks <+> myManageHook -- make sure to include myManageHook definition from above  
             <+> manageHook defaultConfig  
     , layoutHook = avoidStruts $ myLayout  
     , logHook = dynamicLogWithPP xmobarPP  
             { ppOutput = hPutStrLn xmproc  
             , ppTitle = xmobarColor "#2CE3FF" "" . shorten 50  
                , ppLayout = const "" -- to disable the layout info on xmobar  
             }  
     , modMask = mod4Mask  
      , workspaces     = myWorkspaces  
     , normalBorderColor = "#60A1AD"  
     , focusedBorderColor = "#68e862"  
      , borderWidth    = 2  
     }`additionalKeys`  
      [ ((controlMask, xK_space), spawn "exe=`dmenu_run -b -nb black -nf yellow -sf yellow` && eval \"exec $exe\"") -- spawn dmenu  
      , ((mod4Mask, xK_Return), spawn "terminator") -- spawn terminator terminal  
      , ((mod4Mask, xK_w), spawn "firefox --private") -- firefox browser in porn mode  
      , ((mod1Mask, xK_F4), kill) -- to kill app  
      , ((mod4Mask, xK_x), spawn "xchat")  
      , ((mod4Mask, xK_f), spawn "thunar")  
      , ((0, xK_Help), spawn "/home/lulz/scripts/xmonad.open") -- hit a button to open the xmonad.hs file  
      , ((mod4Mask, xK_m), spawn "/home/lulz/scripts/mpd.run") -- hit a button to run mpd with ncmpcpp  
      , ((mod4Mask .|. shiftMask, xK_F4), spawn "sudo shutdown -h now") -- to shutdown  
      , ((mod4Mask .|. shiftMask, xK_r), spawn "sudo reboot") -- to restart  
      , ((controlMask, xK_p), spawn "ncmpcpp toggle") -- play/pause mpd  
      , ((0, xF86XK_AudioLowerVolume), spawn "amixer set Master 3%-") -- decrease volume  
      , ((0, xF86XK_AudioRaiseVolume), spawn "amixer set Master 3%+") -- increase volume  
      , ((mod4Mask .|. shiftMask, xK_l), spawn "slimlock")  
      , ((0, xF86XK_AudioMute), spawn "amixer set Master toggle") -- mute volume  
     , ((controlMask .|. shiftMask, xK_Right), spawn "ncmpcpp next") -- play next song in mpd  
     , ((controlMask .|. shiftMask, xK_Left), spawn "ncmpcpp prev") -- play previous song  
      , ((mod4Mask, xK_a ), windows W.swapUp) -- swap up window  
      , ((mod4Mask, xK_z ), windows W.swapDown) -- swap down window  
      , ((mod4Mask, xK_KP_Add ), sendMessage (IncMasterN 1)) -- increase the number of window on master pane  
      , ((mod4Mask, xK_KP_Subtract ), sendMessage (IncMasterN (-1))) -- decrease the number of window  
      , ((controlMask,        xK_Right   ), sendMessage Expand) -- expand master pane  
      , ((controlMask,        xK_Left   ), sendMessage Shrink) -- shrink master pane  
      , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s") -- capture screenshot of focused window       
     , ((0, xK_Print), spawn "scrot") -- capture screenshot of full desktop  
      , ((0, xF86XK_HomePage), spawn "iron") -- run iron  
      , ((mod4Mask, xK_p), spawn "/home/lulz/scripts/pidgin.open")  
      ]`removeKeys`     -- keys to remove  
      [ (mod4Mask .|. shiftMask, xK_c)  
     , (mod4Mask .|. shiftMask, xK_j)  
      , (mod4Mask .|. shiftMask, xK_k)  
      , (mod4Mask, xK_j)  
      , (mod4Mask, xK_k)  
      , (mod4Mask, xK_h)  
      , (mod4Mask, xK_l)  
      , (mod4Mask, xK_comma)  
      , (mod4Mask, xK_period)  
      ]`additionalMouseBindings`  
      [--((mod4Mask, button4),\_-> nextWS)  
      --,((mod4Mask, button5),\_-> prevWS)  
      ((mod4Mask, button4),\_-> spawn "amixer set Master 3%+")  
      ,((mod4Mask, button5),\_-> spawn "amixer set Master 3%-")  
      ,((mod4Mask, button3),\_-> return())  
      ,((mod4Mask, button1),\_-> return())  
      ]  