-- Demonstrating a simple HelloWorld Ex command.

{-# LANGUAGE OverloadedStrings #-}

module HelloWorld (helloWorld, parse) where

import Data.Text                        ()
import Yi                               (withCurrentBuffer, insertN)
import Yi.Keymap                        (YiM, Action (YiA))
import Yi.Keymap.Vim.Common             (EventString)
import Yi.Keymap.Vim.Ex.Commands.Common (impureExCommand)
import Yi.Keymap.Vim.Ex.Types           (ExCommand (cmdAction, cmdShow))

-- A custom macro
helloWorld :: YiM ()
helloWorld = withCurrentBuffer $ insertN "Hello, world!"

-- The below uses EventString; previously, Parsec stuff was used directly.
-- Not sure if it's still possible to use Parsec in parsing Ex commands.

-- helloWorld Ex Command
-- cf. reload in Yi.Keymap.Vim.Ex.Commands.Reload v0.12
parse :: EventString -> Maybe ExCommand
parse "helloWorld" = Just $ impureExCommand {
    cmdShow = "helloWorld"
  , cmdAction = YiA $ helloWorld
  }
parse _ = Nothing
