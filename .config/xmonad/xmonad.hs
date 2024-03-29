import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe)
import Data.Monoid
import System.Directory
import System.Exit
import System.IO (hPutStrLn)
import XMonad
import XMonad.Actions.CopyWindow (copyToAll, kill1)
import XMonad.Actions.CycleWS (WSType (WSIs), moveTo)
import XMonad.Actions.MouseResize
import XMonad.Actions.SpawnOn
import XMonad.Actions.WithAll (killAll)
import XMonad.Hooks.DynamicLog (PP (..), dynamicLogWithPP, shorten, wrap, xmobarColor, xmobarPP)
import XMonad.Hooks.DynamicProperty (dynamicPropertyChange)
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WindowSwallowing (swallowEventHook)
import XMonad.Hooks.WorkspaceHistory
import XMonad.Layout.GridVariants (Grid (Grid))
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (decreaseLimit, increaseLimit, limitWindows)
import XMonad.Layout.MultiToggle (EOT (EOT), mkToggle, single, (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers (MIRROR, NBFULL, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.ThreeColumns
import qualified XMonad.Layout.ToggleLayouts as T
import XMonad.Layout.WindowArranger (WindowArrangerMsg (..), windowArrange)
import XMonad.Layout.WindowNavigation
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import XMonad.Util.WindowProperties (getProp32)

-- Key Definitions --

myModMask :: KeyMask
myModMask = mod4Mask

altMask :: KeyMask
altMask = mod1Mask

-- Default Apps --

myTerminal :: String
myTerminal = "alacritty"

myBrowser :: String
myBrowser = "librewolf"

myEditor :: String
myEditor = "idea"

--myEditor = "emacsclient -c -a emacs"

-- Focus --

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Border --

myNormalBorderColor :: [Char]
myNormalBorderColor = "#dfdfdf"

myFocusedBorderColor :: [Char]
myFocusedBorderColor = "#ff6c6b"

myBorderWidth :: Dimension
myBorderWidth = 2

-- Workspaces --

myWorkspaces :: [[Char]]
myWorkspaces =
  [ " Main ",
    " Background ",
    " Gaming ",
    " Crypto ",
    " Extra "
  ]

myWorkspaceIndices = M.fromList $ zip myWorkspaces [1 ..] -- (,) == \x y -> (x,y)

clickable ws =
  "<action=xdotool key super+" ++ show i ++ ">" ++ ws ++ "</action>"
  where
    i = fromJust $ M.lookup ws myWorkspaceIndices

-- Functions --

windowCount :: X (Maybe String)
windowCount =
  gets $
    Just
      . show
      . length
      . W.integrate'
      . W.stack
      . W.workspace
      . W.current
      . windowset

-- Key Bindings --

myKeys :: [(String, X ())]
myKeys =
  -- launch default terminal --
  [ ("M-S-<Return>", spawn (myTerminal ++ " -e fish")),
    -- launch dmenu --
    ("M-S-d d", spawn "dmenu_run"),
    -- launch flameshot --
    ("M-S-p", spawn "flameshot gui"),
    -- launch brave --
    ("M-S-b", spawn myBrowser),
    -- close focused window
    ("M-S-c", kill1),
    -- start editor --
    ("M-S-e e", spawn myEditor),
    -- start emacs everywhere --
    ("M-e", spawn "emacsclient --eval '(emacs-everywhere)'"),
    -- start mu4e --
    ("M-S-e m", spawn "emacsclient --eval '(mu4e)'"),
    -- start dired --
    ("M-S-e d", spawn "emacsclient --eval '(dired nil)'"),
    -- start android-studio --
    ("M-S-e a", spawn "android-studio"),
    -- close all windows in focused workspace --
    ("M-S-a", killAll),
    -- change to next layout --
    ("M-<Space>", sendMessage NextLayout),
    -- resize window to correct size --
    ("M-n", refresh),
    -- move focus to next window --
    ("M-<Tab>", windows W.focusDown),
    ("M-j", windows W.focusDown),
    -- move focus to previous window --
    ("M-k", windows W.focusUp),
    -- move focus to master window --
    ("M-m", windows W.focusMaster),
    -- swap focused and master windows --
    ("M-<Return>", windows W.swapMaster),
    -- swap focused and next windows --
    ("M-S-j", windows W.swapDown),
    -- swap focused and previous windows --
    ("M-S-k", windows W.swapUp),
    -- push window back into tiling --
    ("M-t", withFocused $ windows . W.sink),
    -- toggle struts --
    ("M-s", sendMessage ToggleStruts),
    -- quit xmonad --
    ("M-S-q", io exitSuccess),
    -- restart xmonad --
    ("M-q", spawn "xmonad --recompile; xmonad --restart"),
    -- autoclick
    ("M-S-<Insert>", spawn "cautoclick"),
    -- multimedia keys --
    ("<XF86AudioStop>", spawn "playerctl -p spotify stop"),
    ("<XF86AudioPlay>", spawn "playerctl -p spotify play-pause"),
    ("<XF86AudioPrev>", spawn "playerctl -p spotify previous"),
    ("<XF86AudioNext>", spawn "playerctl -p spotify next"),
    ("<XF86AudioMute>", spawn "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"),
    ("<XF86AudioLowerVolume>", spawn "wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-"),
    ("<XF86AudioRaiseVolume>", spawn "wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%+")
  ]

-- Mouse bindings --

myMouseBindings :: (XConfig l -> M.Map (KeyMask, Button) (Window -> X ()))
myMouseBindings XConfig {XMonad.modMask = modm} =
  M.fromList
    [ ( (modm, button1),
        \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster
      ),
      -- Set the window to floating mode and resize by dragging
      ( (modm, button3),
        \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster
      )
    ]

-- Layouts --

mySpacing ::
  Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

tall =
  renamed [Replace "tall"] $
    windowNavigation $
      limitWindows 12 $
        mySpacing 4 $
          ResizableTall 1 (3 / 100) (1 / 2) []

monocle =
  renamed [Replace "monocle"] $
    smartBorders $
      windowNavigation $
        subLayout [] (smartBorders Simplest) $
          limitWindows 20 Full

grid =
  renamed [Replace "grid"] $
    windowNavigation $
      limitWindows 12 $
        mySpacing 4 $
          Grid (16 / 10)

myLayoutHook =
  avoidStruts $
    mouseResize $
      windowArrange $
        mkToggle
          (NBFULL ?? NOBORDERS ?? EOT)
          myDefaultLayout
  where
    myDefaultLayout = tall ||| noBorders monocle ||| grid

-- Window Alterations --

myManageHook :: Query (Endo WindowSet)
myManageHook =
  composeAll $
    [ hasNetWMState "_NET_WM_STATE_ABOVE" --> doFloat,
      className =? "discord" --> doShift (myWorkspaces !! 1),
      className =? "spot" --> doShift (myWorkspaces !! 1),
      title =? "Spotify" --> doShift (myWorkspaces !! 1),
      className =? "qbittorrent" --> doShift (myWorkspaces !! 2),
      className =? "Steam" --> doShift (myWorkspaces !! 2),
      className =? "heroic" --> doShift (myWorkspaces !! 2),
      className =? "ProtonUp-Qt" --> doShift (myWorkspaces !! 2),
      title =? "Monero" --> doShift (myWorkspaces !! 3),
      className =? "bisq.desktop.app.BisqApp" --> doShift (myWorkspaces !! 3),
      resource =? "desktop_window" --> doIgnore
    ]
  where
    getNetWMState :: Window -> X [Atom]
    getNetWMState w = do
      atom <- getAtom "_NET_WM_STATE"
      map fromIntegral . fromMaybe [] <$> getProp32 atom w

    hasNetWMState :: String -> Query Bool
    hasNetWMState state = do
      window <- ask
      wmstate <- liftX $ getNetWMState window
      atom <- liftX $ getAtom state
      return $ elem atom wmstate

-- Event Handling --

myEventHook :: Event -> X All
myEventHook =
  handleEventHook def
    <+> swallowEventHook
      (className =? "Alacritty")
      (return True)

-- Logging --

myLogHook :: X ()
myLogHook = return ()

-- Startup --

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "lxsession &"
  spawnOnce "picom -b &"
  spawnOnce "nm-applet &"
  spawnOnce "blueman-applet &"
  spawnOnce "volumeicon &"
  spawnOnce "trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor 0 --transparent true --alpha 0 --tint 0x282c34  --height 22 &"
  spawnOnce "flatpak run com.discordapp.Discord &"
  spawnOnce "spotify &"
  --  spawnOnce "emacs --daemon &"
  spawnOnce "flameshot &"
  spawnOnce "barrier --config /home/trey/barrier/barrier.conf"
  spawnOnce "steam &"
  spawnOnce "nitrogen --restore &"
  spawnOnce "dunst -config ~/.config/dunst/dunstrc &"
  spawnOnce "heroic &"
  spawnOnce "kdeconnect-indicator &"
  spawnOnce "net.davidotek.pupgui2 &"
  spawnOnce "flatpak run org.getmonero.Monero &"
  setWMName "LG3D"

-- Main --

main :: IO ()
main = do
  xmproc0 <- spawnPipe "xmobar -x 0 /home/trey/.config/xmonad/xmobar.hs"
  xmonad $
    ewmhFullscreen
      . ewmh
      . docks
      $ def
        { -- Defining Things --
          terminal = myTerminal,
          focusFollowsMouse = myFocusFollowsMouse,
          clickJustFocuses = myClickJustFocuses,
          borderWidth = myBorderWidth,
          modMask = myModMask,
          workspaces = myWorkspaces,
          normalBorderColor = myNormalBorderColor,
          focusedBorderColor = myFocusedBorderColor,
          -- Key Bindings --
          mouseBindings = myMouseBindings,
          -- Hooks --
          layoutHook = myLayoutHook,
          manageHook = myManageHook,
          handleEventHook = myEventHook,
          startupHook = myStartupHook,
          logHook =
            myLogHook
              <+> dynamicLogWithPP
                xmobarPP
                  { ppOutput = hPutStrLn xmproc0,
                    ppCurrent = xmobarColor "#98be65" "" . wrap "[" "]",
                    ppVisible = xmobarColor "#98be65" "" . clickable,
                    ppHidden = xmobarColor "#51afef" "" . wrap "*" "" . clickable,
                    ppHiddenNoWindows = xmobarColor "#a9a1e1" "" . clickable,
                    ppTitle = xmobarColor "#dfdfdf" "" . shorten 60,
                    ppSep = "<fc=#5b6268> <fn=1>|</fn> </fc>",
                    ppUrgent = xmobarColor "#ff6c6b" "" . wrap "!" "!",
                    ppOrder = \(ws : l : t : ex) -> [ws, l] ++ ex ++ [t]
                  }
        }
        `additionalKeysP` myKeys -- More keybindings
