import qualified Data.Map as M
import Data.Monoid as DM
import qualified Data.Text as T
import Graphics.X11.ExtraTypes.XF86
import System.Exit
import System.IO
import System.Process
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.GridSelect
import XMonad.Actions.Search
import XMonad.Actions.TagWindows
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WorkspaceNames (getCurrentWorkspaceName, setCurrentWorkspaceName)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Place
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ToggleLayouts
import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Input
import XMonad.Prompt.RunOrRaise
import qualified XMonad.StackSet as W
import XMonad.Util.NamedScratchpad
import XMonad.Util.NamedWindows
import XMonad.Util.Run (runProcessWithInput, spawnPipe)

------------------------------------------------------------------------
myTerminal = "st"

myLockscreen = "betterlockscreen -l blur --off 60 --blur 0.5"

mySuspend = "systemctl suspend"

myScreenshot = "flameshot gui"

myFileManager = "st /home/fedeizzo/.sources/lfrun"

myPasswordManager = "/home/fedeizzo/.sources/rbwAutofill"

myXPConfig =
  def
    { bgColor = "#2E3440",
      bgHLight = "#2E3440",
      fgColor = "#D8DEE9",
      fgHLight = "#A3BE8C",
      position = Top,
      promptBorderWidth = 0,
      searchPredicate = fuzzyMatch,
      sorter = fuzzySort
    }

myClipboardManager =
  let cmd :: String -> X ()
      cmd r = spawn $ "echo '" ++ r ++ "' | xclip -sel clip"

      complFun :: ComplFunction
      complFun s = do
        history <- runProcessWithInput "/home/fedeizzo/.nix-profile/bin/xcmenu" ["-l"] []
        mkComplFunFromList' (lines history) s
   in do
        input <- inputPromptWithCompl myXPConfig "clipboard" complFun
        case input of
          Just i -> cmd i
          _ -> return ()

myTagFunction s = do
  workspaceName <- getCurrentWorkspaceName
  case workspaceName of
    (Just name) ->
      sequence_
        [ withFocused (addTag s),
          addWorkspace s,
          setCurrentWorkspaceName s,
          withTaggedGlobalP s shiftHere,
          removeEmptyWorkspaceByTag name
        ]
    _ ->
      sequence_
        [ withFocused (addTag s),
          addWorkspace s,
          setCurrentWorkspaceName s,
          withTaggedGlobalP s shiftHere
        ]

myMoveTaggedFunction s =
  sequence_
    [ removeEmptyWorkspace,
      addWorkspace s,
      setCurrentWorkspaceName s,
      withTaggedGlobalP s shiftHere
    ]

myScratchpads =
  [ NS "telegram" "telegram-desktop" (className =? "TelegramDesktop") defaultFloating,
    NS "terminal" "st -c scratchpad" (className =? "scratchpad") defaultFloating,
    NS "lf" "st -c lfScratchpad lf" (className =? "lfScratchpad") defaultFloating,
    NS "spotify" "spotify" (className =? "Spotify") defaultFloating,
    NS "feh" "feh /home/fedeizzo/personalProject/layout-corne.png" (className =? "feh") defaultFloating
  ]

myXmobarrc = "~/.xmonad/xmobar.hs"

myWorkspaces = ["none", "void"]

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myBorderWidth = 1

myLayouts =
  avoidStruts
    ( spacingRaw
        False
        (Border 10 0 10 0)
        True
        (Border 0 10 0 10)
        True
        $ Tall 1 (3 / 100) (1 / 2)
    )
    ||| noBorders (fullscreenFull Full)

black = "#2E3440"

frostColor = "#596680"

myNormalBorderColor = black

myFocusedBorderColor = frostColor

xmobarTitleColor = "#e5e9f0"

xmobarCurrentWorkspaceColor = "#A3BE8C"

xmobarVisibleWorkspaceColor = "#FFFF00"

myGsconfig colorizer = (buildDefaultGSConfig colorizer) {gs_cellheight = 30, gs_cellwidth = 150}

myColorizedConfig =
  colorRangeFromClassName
    black -- lowest inactive bg
    (0x70, 0xFF, 0x70)
    black
    white
    white
  where
    black = minBound
    white = maxBound

myModMask = mod4Mask

myKeys conf@(XConfig {XMonad.modMask = modMask}) =
  M.fromList $
    [ ((modMask, xK_Return), spawn $ XMonad.terminal conf),
      ((modMask, xK_d), runOrRaisePrompt myXPConfig),
      ((modMask, xK_y), namedScratchpadAction myScratchpads "spotify"),
      ((modMask, xK_u), namedScratchpadAction myScratchpads "telegram"),
      ((modMask, xK_i), namedScratchpadAction myScratchpads "terminal"),
      ((modMask, xK_o), namedScratchpadAction myScratchpads "feh"),
      ((modMask, xK_s), tagPrompt myXPConfig (\s -> myTagFunction s)),
      ((modMask, xK_a), tagDelPrompt myXPConfig),
      ((modMask, xK_f), tagPrompt myXPConfig (\s -> myMoveTaggedFunction s)),
      ((modMask .|. shiftMask, xK_f), selectWorkspace myXPConfig),
      ((modMask, xK_p), spawn myScreenshot),
      ((modMask, xK_x), spawn myLockscreen),
      -- , ((modMask .|. shiftMask, xK_x), spawn mySuspend)
      ((modMask, xK_e), spawn myFileManager),
      ((modMask, xK_c), myClipboardManager),
      ((modMask, xK_b), spawn myPasswordManager),
      ((0, xF86XK_AudioMute), spawn "pamixer --toggle-mute"),
      ((0, xF86XK_AudioLowerVolume), spawn "pamixer --decrease 5"),
      ((0, xF86XK_AudioRaiseVolume), spawn "pamixer --increase 5"),
      ((0, xF86XK_AudioPrev), spawn "playerctl previous"),
      ((0, xF86XK_AudioPlay), spawn "playerctl play-pause"),
      ((0, xF86XK_AudioNext), spawn "playerctl next"),
      ((0, xF86XK_MonBrightnessUp), spawn "brightnessctl set 10%+"),
      ((0, xF86XK_MonBrightnessDown), spawn "brightnessctl set 10%-"),
      ((modMask .|. shiftMask, xK_q), kill),
      ((modMask, xK_space), sendMessage NextLayout),
      ((modMask .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf),
      ((modMask, xK_n), refresh),
      ((modMask, xK_j), windows W.focusDown),
      ((modMask, xK_k), windows W.focusUp),
      ((modMask .|. shiftMask, xK_Return), windows W.swapMaster),
      ((modMask .|. shiftMask, xK_j), windows W.swapDown),
      ((modMask .|. shiftMask, xK_k), windows W.swapUp),
      ((modMask, xK_h), sendMessage Shrink),
      ((modMask, xK_l), sendMessage Expand),
      ((modMask, xK_t), withFocused $ windows . W.sink),
      ((modMask, xK_comma), sendMessage (IncMasterN 1)),
      ((modMask, xK_period), sendMessage (IncMasterN (-1))),
      ((modMask .|. shiftMask, xK_z), io (exitWith ExitSuccess)),
      ((modMask .|. shiftMask, xK_c), restart "xmonad" True)
    ]
      ++ [((modMask, xK_Tab), swapNextScreen)]
      ++ [((modMask .|. shiftMask, xK_Tab), nextScreen)]

myLogHook = updatePointer (0.5, 0.5) (0, 0) -- exact centre of window

myManageHook =
  placeHook (fixed (0.5, 0.5)) <+> namedScratchpadManageHook myScratchpads
    <+> composeAll
      [ className =? "floatTerm" --> doFloat,
        className =? "Xmessage" --> doFloat,
        className =? "rbwAutofill" --> defaultFloating,
        className =? "Anki" --> doFloat,
        isFullscreen --> (doF W.focusDown <+> doFullFloat)
      ]

myNewApplicationEventHandler (MapRequestEvent {ev_window = w}) = do
  workspaceName <- getCurrentWorkspaceName
  case workspaceName of
    (Just name) -> addTag name w
    _ -> addTag "none" w
myNewApplicationEventHandler _ = pure ()

main = do
  xmproc <- spawnPipe ("xmobar " ++ myXmobarrc)
  xmonad $
    ewmh $
      docks
        defaults
          { logHook =
              dynamicLogWithPP $
                xmobarPP
                  { ppOutput = hPutStrLn xmproc,
                    ppTitle = xmobarColor xmobarTitleColor "" . shorten 100,
                    ppCurrent = xmobarColor xmobarCurrentWorkspaceColor "",
                    ppSep = "   ",
                    ppVisible = xmobarColor xmobarVisibleWorkspaceColor "" . wrap "[" "]"
                  },
            handleEventHook = \e -> myNewApplicationEventHandler e >> return (DM.All True)
          }

defaults =
  def
    { -- simple stuff
      terminal = myTerminal,
      focusFollowsMouse = myFocusFollowsMouse,
      borderWidth = myBorderWidth,
      modMask = myModMask,
      workspaces = myWorkspaces,
      normalBorderColor = myNormalBorderColor,
      focusedBorderColor = myFocusedBorderColor,
      -- key bindings
      keys = myKeys,
      -- mouseBindings      = myMouseBindings,

      -- hooks, layouts
      -- layoutHook         = smartBorders $ myLayouts,
      layoutHook = myLayouts,
      manageHook = myManageHook,
      -- startupHook        = myStartupHook
      logHook = myLogHook
    }
