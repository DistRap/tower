
module Ivory.Tower.Compile.AADL.SystemDoc
  ( systemDoc
  ) where

import System.FilePath

import           Ivory.Compile.AADL.AST
import           Ivory.Compile.AADL.Identifier
import           Ivory.Compile.AADL.Monad
import           Ivory.Compile.AADL.Gen (mkType, typeImpl)

import           Ivory.Language
import           Ivory.Tower
import           Ivory.Tower.Types.Unique
import qualified Ivory.Tower.AST as AST
import qualified Ivory.Tower.AST.Directory as D

type CompileAADL = CompileM (Unique,[String])

systemDoc :: String -> [Module] -> AST.System p
          -> TypeCtxM (Document, [Warning])
systemDoc name ms sysast = runCompile ms virtMod $ do
  writeImport "SMACCM_SYS"
  tg <- threadGroup [] (AST.system_tasks sysast)
  writeThreadGroup tg
  where
  virtMod = package name $ do
    mapM_ depend ms


type ThreadGroup = [ThreadDef] -- XXX change later.

writeThreadGroup :: ThreadGroup -> CompileAADL ()
writeThreadGroup = mapM_ writeThreadDefinition

threadGroup :: [Unique] -> D.Dir Unique (AST.Task p) -> CompileAADL ThreadGroup
threadGroup path (D.Dir ts subdirs) = do
  tdefs   <- mapM threadDef ts
  subdefs <- mapM recur   subdirs
  return (concat (tdefs : subdefs))
  where
  recur (D.Subdir n d) = threadGroup (n:path) d


threadDef :: AST.Task p -> CompileAADL ThreadDef
threadDef t = do
  threadname <- introduceUnique (unique_name (AST.task_name t))
                                (AST.task_name t) []
  features <- featuresDef threadname t (loopsource <.> "h")
  let props = [ ThreadProperty "Source_Text"
                  (PropList [PropString (usersource <.> "c")])
              , ThreadProperty "Initialize_Source_Text"
                  (PropString ("task_init_" ++ (showUnique (AST.task_name t))))
              , ThreadProperty "Priority"
                  (PropInteger (AST.task_priority t))
              , ThreadProperty "Source_Stack_Size"
                  (PropUnit (AST.task_stack_size t) "bytes")
              , ThreadProperty "SMACCM_SYS::Language"
                  (PropString "Ivory")
              ]

  return (ThreadDef threadname features props)
  where
  loopsource = "tower_task_loop_" ++ (showUnique (AST.task_name t))
  usersource = "tower_task_loop_" ++ (showUnique (AST.task_name t))

data RxType = Poll | Event

featuresDef :: String -> AST.Task p -> FilePath -> CompileAADL [ThreadFeature]
featuresDef scope taskast headername = do
  es  <- mapM emitterDef          (AST.task_chan_emitters        taskast)
  prs <- mapM (receiverDef Poll)  (AST.task_chan_poll_receivers  taskast)
  ers <- mapM (receiverDef Event) (AST.task_chan_event_receivers taskast)
  rs  <- mapM readerDef           (AST.task_chan_readers         taskast)
  return $ concat [es, prs, ers, rs]
  where
  emitterDef :: AST.ChanEmitter -> CompileAADL ThreadFeature
  emitterDef ce = do
    chtype <- channelTypename (AST.chanemitter_chan ce)
    portname <- introduceUnique (AST.chanemitter_annotation ce)
                                (AST.chanemitter_name ce) [scope]
    let ps = channelprops (AST.chanemitter_name ce)
                          (AST.chan_size (AST.chanemitter_chan ce))
    return $ ThreadFeatureEventPort portname Out chtype ps

  receiverDef :: RxType -> AST.ChanReceiver -> CompileAADL ThreadFeature
  receiverDef rxt cr = do
    chtype <- channelTypename (AST.chanreceiver_chan cr)
    portname <- introduceUnique (AST.chanreceiver_annotation cr)
                                (AST.chanreceiver_name cr) [scope]
    let ps = channelprops (AST.chanreceiver_name cr)
                          (AST.chan_size (AST.chanreceiver_chan cr))
    let p = case rxt of -- XXX hack for now
              Poll -> ThreadProperty "ReceiverType" (PropString "Poll")
              Event -> ThreadProperty "ReceiverType" (PropString "Event")
    return $ ThreadFeatureEventPort portname In chtype (p:ps)

  readerDef :: AST.ChanReader -> CompileAADL ThreadFeature
  readerDef cr = do
    chtype <- channelTypename (AST.chanreader_chan cr)
    portname <- introduceUnique (AST.chanreader_annotation cr)
                                (AST.chanreader_name cr) [scope]
    return $ ThreadFeatureDataPort portname chtype []

  channelprops :: Unique -> Integer -> [ThreadProperty]
  channelprops usourcetext chansize =
    [ ThreadProperty "Queue_Size" (PropInteger chansize)
    , smaccmProp "CommPrim_Source_Header" headername
    , smaccmProp "CommPrim_Source_Text"   (showUnique usourcetext) -- XXX fix once we do codegen
    ]

channelTypename :: AST.Chan -> CompileAADL TypeName
channelTypename chan = do
  t <- mkType (AST.chan_ityp chan)
  return $ implNonBaseTypes t
  where
  -- HACK: user declared datatypes always need .impl appended
  implNonBaseTypes :: TypeName -> TypeName
  implNonBaseTypes t = case t of
      QualTypeName "Base_Types" _ -> t
      DotTypeName _ _ -> t
      _ -> DotTypeName t "impl"



-- Uniqueness managment -------------------------------------------------------

introduceUnique :: String -> Unique -> [String] -> CompileAADL String
introduceUnique s u scope = do
  m <- getIdentifierMap
  let mm = filter ((== scope) . snd . fst) m
  case elem s (map snd mm)  of
    False -> do
      setIdentifierMap (((u,scope),s):m)
      return s
    True -> do
      uniquenessWarning warning
      setIdentifierMap (((u,scope),us):m)
      return us
  where
  us = showUnique u
  warning = "User provided identifier \"" ++ s ++ "\" for " ++ us
         ++ " is not unique in tower-aadl Assembly."

_uniqueIdentifier :: Unique -> String -> CompileAADL String
_uniqueIdentifier u ctx = do
  m <- getIdentifierMap
  -- If this lookup fails, its because the CompileM code that should have used
  -- introduceUnique to create names was not implemented correctly.
  case lookup u (map withoutScope m) of
    Just n -> return n
    Nothing -> error ("Failed to find unique identifier " ++ (showUnique u)
                      ++ " in " ++ ctx ++ " context.\nName Map Dump\n" ++ (show m))
  where
  withoutScope ((k,_s),v) = (k,v)

-- Constructor Helper Functions -----------------------------------------------

threadProp :: String -> String -> ThreadProperty
threadProp k v = ThreadProperty k (PropString v)

smaccmProp :: String -> String -> ThreadProperty
smaccmProp k v = threadProp ("SMACCM_SYS::" ++ k) v



