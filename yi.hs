{-# LANGUAGE OverloadedStrings #-}

import Yi
import Yi.Keymap.Vim (keymapSet, mkKeymapSet, defVimConfig, vimExCommandParsers)
import qualified Yi.Mode.Haskell as Haskell
import Yi.Style
import Yi.Style.Library

-- under lib/ folder
import qualified HelloWorld as HelloWorld

defaultVimUiTheme :: Theme
defaultVimUiTheme = defaultTheme  `override` \super self -> super {
        selectedStyle = modelineFocusStyle self
 }

myConfigUI :: UIConfig
myConfigUI = (configUI defaultVimConfig)  {
        configFontSize = Just 10,
        configTheme = defaultVimUiTheme,
        configWindowFill = '~'
 }

main :: IO ()
main = yi $ defaultVimConfig {
    configUI = myConfigUI,
    defaultKm = mkKeymapSet $ defVimConfig `override` \ super self -> super
            { vimExCommandParsers = myExCmdParsers ++ vimExCommandParsers super }
 }

myExCmdParsers = [HelloWorld.parse]
