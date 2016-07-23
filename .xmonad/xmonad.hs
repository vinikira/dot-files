import XMonad
import XMonad.Hooks.DynamicLog

main = xmonad =<< xmobar defaultConfig
      { terminal     = "xterm"
      , modMask      = mod4Mask
      , borderWidth  = 2
      }
