{-# LANGUAGE OverloadedStrings #-}

module Test.Interact.Daemon.Key (tests) where

import System.Directory (canonicalizePath, createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import AgdaMCP.Interact.Daemon.Key (
  Key (..),
  Paths (..),
  findProjectRoot,
  keyHash,
  keyPaths,
  keyText,
  newKey,
  runtimeDirectoryFor,
 )
import AgdaMCP.Options (AgdaFlags (..), defaultFlags)

tests :: TestTree
tests =
  testGroup
    "Key"
    [ rootTests
    , textTests
    , pathTests
    ]

rootTests :: TestTree
rootTests =
  testGroup
    "findProjectRoot"
    [ testCase "the nearest ancestor holding an .agda-lib file is the root" $
        withProject $ \top -> do
          let cwd = top </> "src" </> "Deep"
          createDirectoryIfMissing True cwd
          root <- findProjectRoot cwd
          root @?= top
    , testCase "a directory that itself holds the file is its own root" $
        withProject $ \top -> do
          root <- findProjectRoot top
          root @?= top
    , testCase "with no library file anywhere the working directory is the root" $
        withSystemTempDirectory "agda-mcp-key" $ \directory -> do
          expected <- canonicalizePath directory
          root <- findProjectRoot directory
          root @?= expected
    , testCase "newKey uses the discovered root" $
        withProject $ \top -> do
          let cwd = top </> "src"
          createDirectoryIfMissing True cwd
          key <- newKey cwd defaultFlags
          keyRoot key @?= top
    , testCase "newKey makes the library file absolute against the working directory" $
        withProject $ \top -> do
          key <- newKey top defaultFlags {libraryFile = Just "libraries"}
          libraryFile (keyFlags key) @?= Just (top </> "libraries")
    , testCase "newKey leaves an absolute library file alone" $
        withProject $ \top -> do
          key <- newKey top defaultFlags {libraryFile = Just "/x/libraries"}
          libraryFile (keyFlags key) @?= Just "/x/libraries"
    , testCase "newKey leaves include paths as written, since Agda resolves them" $
        withProject $ \top -> do
          key <- newKey top defaultFlags {includes = ["src"]}
          includes (keyFlags key) @?= ["src"]
    ]
 where
  withProject :: (FilePath -> IO a) -> IO a
  withProject k =
    withSystemTempDirectory "agda-mcp-key" $ \directory -> do
      top <- canonicalizePath directory
      writeFile (top </> "project.agda-lib") "name: project\ninclude: src\n"
      k top

textTests :: TestTree
textTests =
  testGroup
    "keyText and keyHash"
    [ testCase "the text is the root then each rendered flag, NUL-separated" $
        keyText sampleKey
          @?= "/work/proj\0--include-path\0src\0--library\0standard-library"
    , testCase "flag order is preserved" $
        keyText sampleKey {keyFlags = (keyFlags sampleKey) {includes = ["b", "a"]}}
          @?= "/work/proj\0--include-path\0b\0--include-path\0a\0--library\0standard-library"
    , testCase "a key with no flags is the root and a NUL" $
        keyText Key {keyRoot = "/work/proj", keyFlags = defaultFlags}
          @?= "/work/proj\0"
    , testCase "the hash is FNV-1a 64 of the text as sixteen hex digits" $
        keyHash sampleKey @?= "ded44e6ac91e5c53"
    , testCase "the hash of the bare root" $
        keyHash Key {keyRoot = "/work/proj", keyFlags = defaultFlags}
          @?= "ecc3ec950f0b317d"
    ]

pathTests :: TestTree
pathTests =
  testGroup
    "paths"
    [ testCase "the runtime directory is under XDG_RUNTIME_DIR when set" $
        runtimeDirectoryFor (Just "/run/user/1000") 1000
          @?= "/run/user/1000/agda-interact"
    , testCase "the runtime directory falls back to a per-user directory in /tmp" $
        runtimeDirectoryFor Nothing 1000 @?= "/tmp/agda-interact-1000"
    , testCase "an empty XDG_RUNTIME_DIR counts as unset" $
        runtimeDirectoryFor (Just "") 1000 @?= "/tmp/agda-interact-1000"
    , testCase "every file lives under the hash directory" $
        keyPaths "/run/user/1000/agda-interact" sampleKey
          @?= Paths
            { socketPath = "/run/user/1000/agda-interact/ded44e6ac91e5c53/socket"
            , infoPath = "/run/user/1000/agda-interact/ded44e6ac91e5c53/info"
            , logPath = "/run/user/1000/agda-interact/ded44e6ac91e5c53/log"
            , spawnLockPath = "/run/user/1000/agda-interact/ded44e6ac91e5c53/spawn.lock"
            , daemonLockPath = "/run/user/1000/agda-interact/ded44e6ac91e5c53/daemon.lock"
            }
    ]

sampleKey :: Key
sampleKey =
  Key
    { keyRoot = "/work/proj"
    , keyFlags =
        defaultFlags {includes = ["src"], libraries = ["standard-library"]}
    }
