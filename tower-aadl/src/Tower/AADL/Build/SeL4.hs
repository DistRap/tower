-- Create Ramses build script.
--
-- (c) 2015 Galois, Inc.
--

module Tower.AADL.Build.SeL4 where

import Data.Char
import System.FilePath ((</>))

import Tower.AADL.Config (AADLConfig(..), lib)
import Tower.AADL.Build.Common

--------------------------------------------------------------------------------
-- Ramses build

-- Ramses Makefile ------------------------------------------------------------
ramsesMakefile :: AADLConfig -> [MkStmt]
ramsesMakefile c =
  [ include    aadlFilesMk
  , includeOpt "../RAMSES_PATH.mk"
  , "RAMSES_PATH" ?= "./"
  , "SMACCM_PATH" ?= "./"
  , export $"RAMSES_DIR" === "$(RAMSES_PATH)/ramses_resource"
  , export $"AADL2RTOS_CONFIG_DIR" === "$(RAMSES_PATH)/aadl2rtos_resource"
  , Target ".PHONY" ["all", "tower-clean", "ramses", "camkesmakefile.mk"] []
  , Target "ramses" []
    ["java -jar $(RAMSES_PATH)/ramses.jar -g rtos -i $(AADL2RTOS_CONFIG_DIR)\
           \-o . -l trace -s sys.impl -m SMACCM_SYS.aadl,$(AADL_LIST)"]
  , Target camkesMakefileName []
    [ "make ramses"
    , if configCustomMakefile c
        then ""
        else unwords ["cp ", mkTp, camkesMakefileName]
    , "rm -rf ../../libs/" ++ lib c
    , unwords ["cp -r ", lib c, "../../libs/"]
    , unwords ["cp -r ", lib c </> "include" </> "*", configHdrDir c]
    ]
  , Target "tower-clean" []
    [ rm aadlFilesMk
    , rm kbuildName
    , rm kconfigName
    , rm camkesMakefileName
    , rm "*.aadl"
    , rm (configSrcsDir c)
    , rm (configHdrDir  c)
    ]
  ]
  where
  rm s = "-rm -rf " ++ s
  mkTp = "make_template" </> makefileName

--------------------------------------------------------------------------------
-- Kbuild, Kconfig

kbuildLib :: String -> [MkStmt]
kbuildLib dir =
  [ "libs-$(CONFIG_LIB_" ++ shellVar dir ++ ")" += dir
  , Target dir ["common", "$(libc)"] []
  ]

kbuildApp :: String -> String -> [MkStmt]
kbuildApp libdir dir =
  [ "apps-$(CONFIG_APP_" ++ shellVar dir ++ ")" += dir
  , Target dir  ["libsel4", "libmuslc", "libsel4platsupport", "libsel4muslccamkes"
                ,"libsel4camkes", "libsel4sync", "libsel4debug", "libsel4bench"
                ,libdir] []
  ]

kbuildName :: String
kbuildName = "Kbuild"

kconfigLib :: String -> String -> String
kconfigLib prog dir = unlines
  [ "menuconfig LIB_" ++ shellVar dir
  , "    bool \"Shared code for " ++ prog ++ " app.\""
  , "    default n"
  , "    help"
  , "        Generated from Ivory/Tower."
  ]

kconfigApp :: String -> String -> String
kconfigApp prog dir = unlines
  [ "config APP_" ++ shellVar dir
  , "    bool \"Generated code for " ++ prog ++ " .\""
  , "    default n"
  , "    select APP_CAMKES_EXECUTIVE"
  , "    help"
  , "        Generated AADL from " ++ prog
  ]

kconfigName :: String
kconfigName = "Kconfig"

camkesMakefileName :: String
camkesMakefileName = "camkesmakefile.mk"

makefileLib :: AADLConfig -> [MkStmt]
makefileLib c =
  [ Comment "Targets"
  , "TARGETS" =: lib c ++ ".a"
  , Comment "Header files/directories this library provides"
  , "HDRFILES" =: "$(wildcard ${SOURCE_DIR}/include/*)"
  , "CFILES" =: "$(patsubst $(SOURCE_DIR)/%,%,$(wildcard $(SOURCE_DIR)/src/*.c))"
  , include "$(SEL4_COMMON)/common.mk"
  , "CFLAGS" += "-DODROID"
  ]

makefileApp :: String -> [MkStmt]
makefileApp dir =
  [ Comment "Include assumes this is driven by seL4 build."
  , "CFLAGS" += "-DODROID"
  , includeOpt (fromApps componentLibsName)
  , includeOpt (fromApps camkesMakefileName)
  , includeOpt ramsesMakefileName
  ]
  where
  fromApps fl = "apps" </> dir </> fl
