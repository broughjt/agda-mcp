{-# LANGUAGE OverloadedStrings #-}

module Test.Options (tests) where

import Agda.Interaction.Library (parseLibName)
import Agda.Interaction.Options (CommandLineOptions (..), defaultOptions)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import AgdaMCP.Options (
  AgdaFlags (..),
  defaultFlags,
  renderFlags,
  toCommandLineOptions,
 )

tests :: TestTree
tests =
  testGroup
    "Options"
    [ testCase "the defaults are Agda's defaults" $ do
        let options = toCommandLineOptions defaultFlags
        optIncludePaths options @?= optIncludePaths defaultOptions
        optLibraries options @?= optLibraries defaultOptions
        optOverrideLibrariesFile options @?= optOverrideLibrariesFile defaultOptions
        optUseLibs options @?= optUseLibs defaultOptions
        optDefaultLibs options @?= optDefaultLibs defaultOptions
    , testCase "include paths are prepended, as the fold always did" $
        optIncludePaths (toCommandLineOptions defaultFlags {includes = ["a", "b"]})
          @?= ["b", "a"]
    , testCase "libraries are appended in order and parsed" $
        optLibraries
          (toCommandLineOptions defaultFlags {libraries = ["standard-library", "cubical"]})
          @?= [parseLibName "standard-library", parseLibName "cubical"]
    , testCase "a library file overrides the default and turns libraries on" $ do
        let options =
              toCommandLineOptions
                defaultFlags {libraryFile = Just "/x/libraries"}
        optOverrideLibrariesFile options @?= Just "/x/libraries"
        optUseLibs options @?= True
    , testCase "no-libraries turns libraries off" $
        optUseLibs (toCommandLineOptions defaultFlags {useLibs = False}) @?= False
    , testCase "no-default-libraries turns the defaults off" $
        optDefaultLibs (toCommandLineOptions defaultFlags {defaultLibs = False})
          @?= False
    , testCase "rendering gives argv in canonical order with long spellings" $
        renderFlags
          AgdaFlags
            { includes = ["src", "lib"]
            , libraries = ["standard-library", "cubical"]
            , libraryFile = Just "/x/libraries"
            , useLibs = False
            , defaultLibs = False
            }
          @?= [ "--include-path"
              , "src"
              , "--include-path"
              , "lib"
              , "--library"
              , "standard-library"
              , "--library"
              , "cubical"
              , "--library-file"
              , "/x/libraries"
              , "--no-libraries"
              , "--no-default-libraries"
              ]
    , testCase "the defaults render as nothing" $
        renderFlags defaultFlags @?= []
    ]
