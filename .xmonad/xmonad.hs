-- Imports --

  -- Base --
import XMonad
import System.Directory
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

  -- Data --
import Data.Monoid
import Data.Maybe (fromJust)
import qualified Data.Map as M

  -- Actions --
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.MouseResize
import XMonad.Actions.SpawnOn
import XMonad.Actions.WithAll (killAll)
import XMonad.Actions.CycleWS (moveTo, WSType( WSIs ))

  -- System --
import System.Exit

  -- Hooks --
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.WorkspaceHistory
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.DynamicProperty (dynamicPropertyChange)
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops

  -- Util --
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.SpawnOnce
import XMonad.Util.Run

  -- Layouts --
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.ResizableTile
import XMonad.Layout.ThreeColumns

  -- Layouts Modifiers --
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import XMonad.Layout.WindowNavigation
import qualified XMonad.Layout.ToggleLayouts as T

-- Key Definitions --

myModMask :: KeyMask
myModMask = mod4Mask

altMask :: KeyMask
altMask = mod1Mask

-- Default Apps --

myTerminal :: String
myTerminal = "alacritty"

myBrowser :: String
myBrowser  = "brave"

myEditor :: String
myEditor = "emacsclient -c -a emacs"

-- Focus --

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Border Width --

myBorderWidth :: Dimension
myBorderWidth   = 2

-- Workspaces --

myWorkspaces :: [[Char]]
myWorkspaces = [" Main ", " Background ", " Gaming ", " Entertainment ", " VM ", " Extra "]
myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..] -- (,) == \x y -> (x,y)

clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
    where i = fromJust $ M.lookup ws myWorkspaceIndices

myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
    { swn_font              = "xft:RobotoMono Nerd Font:bold:size=60"
    , swn_fade              = 1.0
    , swn_bgcolor           = "#282C34"
    , swn_color             = "#ffffff"
    }

-- Border Colors --

myNormalBorderColor :: [Char]
myNormalBorderColor  = "#dddddd"
myFocusedBorderColor :: [Char]
myFocusedBorderColor = "#ff0000"

-- Functions --

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

-- Key Bindings --

myKeys :: [(String, X ())]
myKeys =
         -- launch default terminal --
         [ ("M-S-<Return>", spawn (myTerminal ++ " -e fish"))
         -- launch dmenu --
         , ("M-S-d d", spawn "dmenu_run")
         -- launch flameshot --
         , ("M-S-p", spawn "flameshot gui")
         -- launch brave --
         , ("M-S-b", spawn "brave")
         -- launch xmenu --
         , ("M-S-m", spawn "/home/trey/sourcecode/xmenu/xmenu.sh")
         -- close focused window
         , ("M-S-c", kill1)
         -- start emacs --
         , ("M-S-e e", spawn myEditor)
         -- start emacs everywhere
         , ("M-e", spawn "emacsclient --eval '(emacs-everywhere)'")
         -- start mu4e --
         , ("M-S-e m", spawn "emacsclient --eval '(mu4e)'")
         -- start dired --
         , ("M-S-e d", spawn "emacsclient --eval '(dired nil)'")
         -- close all windows in focused workspace --
         , ("M-S-a", killAll)
         -- change to next layout --
         , ("M-<Space>", sendMessage NextLayout)
         -- resize window to correct size --
         , ("M-n", refresh)
         -- move focus to next window --
         , ("M-<Tab>", windows W.focusDown)
         , ("M-j", windows W.focusDown)
         -- move focus to previous window --
         , ("M-k", windows W.focusUp)
         -- move focus to master window --
         , ("M-m", windows W.focusMaster)
         -- swap focused and master windows --
         , ("M-<Return>", windows W.swapMaster)
         -- swap focused and next windows --
         , ("M-S-j", windows W.swapDown)
         -- swap focused and previous windows --
         , ("M-S-k", windows W.swapUp)
         -- shrink the master area --
         , ("M-h", sendMessage Shrink)
         -- expand the master area --
         , ("M-l", sendMessage Expand)
         -- push window back into tiling --
         , ("M-t", withFocused $ windows . W.sink)
         -- toggle struts --
         , ("M-s", sendMessage ToggleStruts)
         -- quit xmonad --
         , ("M-S-q", io exitSuccess)
         -- restart xmonad --
         , ("M-q", spawn "xmonad --recompile; xmonad --restart")
         -- autoclick
         , ("M-S-<Insert>", spawn "cautoclick")
         -- multimedia keys --
         , ("<XF86AudioStop>", spawn "playerctl -p spotify stop")
         , ("<XF86AudioPlay>", spawn "playerctl -p spotify play-pause")
         , ("<XF86AudioPrev>", spawn "playerctl -p spotify previous")
         , ("<XF86AudioNext>", spawn "playerctl -p spotify next")
         , ("<XF86AudioMute>",   spawn "amixer set Master toggle")
         , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute")
         , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute")
         ]

-- Old Keybind layout for reference --
{- myKeysOld :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeysOld conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch default terminal --
    [ ((modm .|. shiftMask, xK_Return), spawn (myTerminal ++ " -e fish"))

    -- launch dmenu --
    , ((modm .|. shiftMask, xK_d     ), spawn "dmenu_run -h 24 -fn 'RobotoMono Nerd Font-9'")

    -- launch flameshot --
    , ((modm .|. shiftMask, xK_p     ), spawn "flameshot gui")

    -- launch xmenu --
    , ((modm .|. shiftMask, xK_m     ), spawn "/home/trey/sourcecode/xmenu/xmenu.sh")

    -- launch TS --
    -- , ((modm .|. shiftMask, xK_Tab   ), treeselectAction tsDefaultConfig)

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    , ((modm              , xK_s     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    ]
    ++
    
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]] -}

-- Mouse bindings --

myMouseBindings :: (XConfig l -> M.Map (KeyMask, Button) (Window -> X ()))
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

-- Layouts --

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

tall    = renamed [Replace "tall"]
          $ windowNavigation
          $ limitWindows 12
          $ mySpacing 4
          $ ResizableTall 1 (3/100) (1/2) []
monocle = renamed [Replace "monocle"]
          $ smartBorders
          $ windowNavigation
          $ subLayout [] (smartBorders Simplest)
          $ limitWindows 20 Full
grid    = renamed [Replace "grid"]
          $ windowNavigation
          $ limitWindows 12
          $ mySpacing 4
          $ Grid (16/10)

myLayoutHook = avoidStruts $ mouseResize $ windowArrange
             $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
             where
               myDefaultLayout =     tall
                                 ||| noBorders monocle
                                 ||| grid

-- Window Alterations --

myManageHook :: Query (Endo WindowSet)
myManageHook = composeAll
    [ className =? "pyrogenesis"        --> doFloat
    , className =? "qalculate-gtk"      --> doFloat
    , className =? "discord"            --> doShift ( myWorkspaces !! 1 )
    , className =? "Barrier"            --> doShift ( myWorkspaces !! 1 )
    , className =? "Steam"              --> doShift ( myWorkspaces !! 2 )
    , className =? "lbry"               --> doShift ( myWorkspaces !! 3 )
    , className =? "VirtualBox Manager" --> doShift ( myWorkspaces !! 4 )
    , resource  =? "desktop_window"     --> doIgnore ]

-- Event Handling --

spotifyEventHook:: Event -> X All
spotifyEventHook = dynamicPropertyChange "WM_NAME" (className =? "Spotify" --> doShift ( myWorkspaces !! 1 ))

myEventHook:: Event -> X All
myEventHook = handleEventHook def <+> spotifyEventHook

-- Logging --

myLogHook :: X ()
myLogHook = return ()

-- Startup --

myStartupHook :: X ()
myStartupHook = do
        spawnOnce "lxsession &"
        spawnOnce "nm-applet &"
        spawnOnce "blueman-applet &"
        spawnOnce "volumeicon &"
        spawnOnce "trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor 0 --transparent true --alpha 0 --tint 0x282c34  --height 22 &"
        spawnOnce "crd --start &"
        spawnOnce "discord &"
        spawnOnce "spotify &"
        spawnOnce "emacs --daemon &"
        spawnOnce "flameshot &"
        spawnOnce "barrier --config /home/trey/barrier/barrier.conf"
        spawnOnce "cadmus &"
        spawnOnce "steam &"
        spawnOnce "nitrogen --restore &"
        spawnOnce "play-with-mpv &"

-- Main --

main :: IO ()
main = do
  xmproc0 <- spawnPipe "xmobar -x 0 /home/trey/.xmonad/xmobar.hs"
  xmonad $ ewmhFullscreen . ewmh . docks $ def
        {
      -- Defining Things --
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- Key Bindings --
        mouseBindings      = myMouseBindings,

      -- Hooks --
        layoutHook         = myLayoutHook,
        manageHook         = myManageHook <+> manageSpawn,
        handleEventHook    = myEventHook,
        startupHook        = myStartupHook,
        logHook            = myLogHook <+> dynamicLogWithPP xmobarPP
                                { ppOutput = \x -> hPutStrLn xmproc0 x
                                , ppCurrent = xmobarColor "#98be65" "" . wrap "[" "]"
                                , ppVisible = xmobarColor "#98be65" ""-- . clickable
                                , ppHidden = xmobarColor "#82AAFF" "" . wrap "*" ""-- . clickable
                                , ppHiddenNoWindows = xmobarColor "#c792ea" ""-- . clickable
                                , ppTitle = xmobarColor "#b3afc2" "" . shorten 60
                                , ppSep = "<fc=#666666> <fn=1>|</fn> </fc>"
                                , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"
                                , ppExtras = [windowCount]
                                , ppOrder = \(ws:l:t:ex) -> [ws,l]++ex++[t]
                                }
} `additionalKeysP` myKeys -- More keybindings
