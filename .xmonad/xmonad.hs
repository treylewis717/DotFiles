-- Imports --

  -- Base --
import XMonad
import System.Directory
import System.IO (hPutStrLn)
import System.Exit
import qualified XMonad.StackSet as W

  -- Data --
import Data.Monoid
import Data.Tree
import qualified Data.Map as M

  -- Actions --
import XMonad.Actions.MouseResize
import qualified XMonad.Actions.TreeSelect as TS

  -- System --
import System.Exit

  -- Hooks --
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.WorkspaceHistory
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))

  -- Util --
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
myEditor = "emacs "

-- Focus --
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Border Width --

myBorderWidth   = 2

-- Workspaces --

myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]

myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
    { swn_font              = "xft:Ubuntu:bold:size=60"
    , swn_fade              = 1.0
    , swn_bgcolor           = "#1c1f24"
    , swn_color             = "#ffffff"
    }

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#dddddd"
myFocusedBorderColor = "#ff0000"

-- Tree Select (For Later) --

-- treeselectAction :: TS.TSConfig (X ()) -> X ()
--treeselectAction a = TS.treeselectAction a
  --[ Node (TS.TSNode "+ Programming" (return ()))
    --  [ Node (TS.TSNode "+ Git" (return ()))
      --    [ Node (TS.TSNode ""

--tsDefaultConfig = TS.TSConfig a
--tsDefaultConfig = TS.TSConfig { TS.ts_hidechildren = True
  --                         , TS.ts_background   = 0xc0c0c0c0
    --                       , TS.ts_font         = "xft:Sans-16"
      --                     , TS.ts_node         = (0xff000000, 0xff50d0db)
        --                   , TS.ts_nodealt      = (0xff000000, 0xff10b8d6)
          --                 , TS.ts_highlight    = (0xffffffff, 0xffff0000)
            --               , TS.ts_extra        = 0xff000000
              --             , TS.ts_node_width   = 200
                --           , TS.ts_node_height  = 30
                  --         , TS.ts_originX      = 0
                    --       , TS.ts_originY      = 0
                      --     , TS.ts_indent       = 80
                        --   , TS.ts_navigate     = myTreeNavigation
                          -- }

--myTreeNavigation = M.fromList
  --  [ ((0, xK_Escape), cancel)
    --, ((0, xK_Return), select)
--    , ((0, xK_space),  select)
  --  , ((0, xK_Up),     movePrev)
    --, ((0, xK_Down),   moveNext)
--    , ((0, xK_Left),   moveParent)
  --  , ((0, xK_Right),  moveChild)
    --, ((0, xK_k),      movePrev)
--    , ((0, xK_j),      moveNext)
  --  , ((0, xK_h),      moveParent)
    --, ((0, xK_l),      moveChild)
--    , ((0, xK_o),      moveHistBack)
  --  , ((0, xK_i),      moveHistForward)
    --]

-- Functions --

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

-- Key Bindings --

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

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
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

-- Mouse bindings --

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
          $ windowNavigation
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

myManageHook = composeAll
    [ title =? "spotify"        --> doShift ( myWorkspaces !! 1 )
    , title =? "Discord"            --> doShift ( myWorkspaces !! 1 )
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook = return ()

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = do
        spawnOnce "nitrogen --restore &"
        spawnOnce "picom &"
        spawnOnce "nm-applet &"
        spawnOnce "blueman-applet &"
        spawnOnce "volumeicon &"
        spawnOnce "trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor 0 --transparent true --alpha 0 --tint 0x282c34  --height 22 &"
        spawnOnce "kdeconnect-indicator"
        spawnOnce "crd --start"
        spawnOnce "discord"
        spawnOnce "spotify"



-- Main --
main :: IO ()
main = do
  xmproc0 <- spawnPipe "xmobar -x 0 /home/trey/.config/xmobar/xmobarrc"
  xmonad $ docks def
        {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = showWName' myShowWNameTheme $ myLayoutHook,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        startupHook        = myStartupHook,
        logHook            = myLogHook <+> dynamicLogWithPP xmobarPP
                                { ppOutput = \x -> hPutStrLn xmproc0 x
                                , ppCurrent = xmobarColor "#98be65" "" . wrap "[" "]"
                                , ppVisible = xmobarColor "#98be65" ""
                                , ppHidden = xmobarColor "#82AAFF" "" . wrap "*" ""
                                , ppHiddenNoWindows = xmobarColor "#c792ea" ""
                                , ppTitle = xmobarColor "#b3afc2" "" . shorten 60
                                , ppSep = "<fc=#666666> <fn=1>|</fn> </fc>"
                                , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"
                                , ppExtras = [windowCount]
                                , ppOrder = \(ws:l:t:ex) -> [ws,l]++ex++[t]
                                }
	}
