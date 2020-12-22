#!/usr/bin/env run-script
module Application.Script.GenerateElmTypes where

import Application.Script.Prelude
import qualified Language.Elm.Simplification as Simplification
import qualified Language.Elm.Pretty as Pretty
import Language.Haskell.To.Elm
import Data.Text.IO
import Web.JsonTypes
import qualified System.Directory as Directory
import qualified Data.HashMap.Lazy as HashMap
import Application.Helper.View

run :: Script
run = do
    let
        definitions = Simplification.simplifyDefinition <$>
                        jsonDefinitions @Widget <> jsonDefinitions @BookJSON

        modules = Pretty.modules definitions

    Directory.createDirectoryIfMissing False "elm/Api"

    forEach (HashMap.toList modules) $ \(_moduleName, contents) ->
        writeFile "elm/Api/Generated.elm" (show contents)