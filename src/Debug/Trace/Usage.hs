module Debug.Trace.Usage (traceUsage, traceUsageOfMonad, reportUsage, callStack) where

import Prelude
import qualified Text.PrettyPrint as Ppr
import Data.IORef
import System.IO.Unsafe
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import GHC.Stack
import Data.List (sortBy, stripPrefix, findIndex)
import Data.Ord (comparing)
import qualified Data.Trie.Map as TrieMap
import Type.Reflection (Typeable, typeRep)

-- CallStack Trie Key
data CstKey = Cstk !String !SrcLoc
  deriving stock Eq

instance Ord CstKey where
  compare (Cstk s1 l1) (Cstk s2 l2) =
      compare s1 s2 <> compareSrcLoc l1 l2
    where
      compareSrcLoc
          (SrcLoc pkg1 mod1 file1 sline1 scol1 eline1 ecol1)
          (SrcLoc pkg2 mod2 file2 sline2 scol2 eline2 ecol2)
        =
          compare   pkg1   pkg2 <>
          compare   mod1   mod2 <>
          compare  file1  file2 <>
          compare sline1 sline2 <>
          compare  scol1  scol2 <>
          compare eline1 eline2 <>
          compare  ecol1  ecol2

toCstKey :: (String, SrcLoc) -> CstKey
toCstKey (s, l) = Cstk s l

toCstKeys :: CallStack -> [CstKey]
toCstKeys = map toCstKey . compress . reverse . getCallStack

-- Removes uninteresting recursion from the CallStack.
compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:xs) =
  case findIndex (==x) xs of
    Nothing -> x : compress xs
    Just i ->
      let repetend = x : take i xs in
      repetend ++ compress (stripPrefixes repetend (drop i xs))

stripPrefixes :: Eq a => [a] -> [a] -> [a]
stripPrefixes prefix xs =
  case stripPrefix prefix xs of
    Nothing -> xs
    Just xs' -> stripPrefixes prefix xs'

{-# NOINLINE tracingData #-}
tracingData :: IORef (Map String (Int, TrieMap.TMap CstKey Int))
tracingData = unsafePerformIO (newIORef Map.empty)

{-# NOINLINE traceUsage #-}
traceUsage :: String -> CallStack -> a -> a
traceUsage usageStr callStk r =
  unsafePerformIO (
    atomicModifyIORef tracingData (\m ->
      (Map.insertWith
         (\(i, m1) (j, m2) -> (i + j, TrieMap.unionWith (+) m1 m2))
         usageStr
         (1, TrieMap.singleton (toCstKeys callStk) 1)
         m, ())
    )
  ) `seq` r

{-# INLINE traceUsageOfMonad #-}
traceUsageOfMonad  :: forall m a. Typeable m => CallStack -> m a -> m a
traceUsageOfMonad callStk r =
  traceUsage (show (typeRep @m)) callStk r

reportUsage :: IO ()
reportUsage = do
  m <- readIORef tracingData
  putStrLn (Ppr.render (pprTracingData m))

pprTracingData :: Map String (Int, TrieMap.TMap CstKey Int) -> Ppr.Doc
pprTracingData m =
  Ppr.vcat
    [ Ppr.hang (Ppr.int n Ppr.<+> Ppr.text usageStr) 4 (pprContexts ctxts)
    | (usageStr, (n, ctxts)) <- sortBy (comparing (fst . snd)) (Map.toList m)
    ]

pprContexts :: TrieMap.TMap CstKey Int -> Ppr.Doc
pprContexts m =
  Ppr.vcat
    [ Ppr.hang (Ppr.int n Ppr.<+> Ppr.text "calls from:") 4 (pprCallStack ctxt)
    | (ctxt, n) <- sortBy (comparing snd) (TrieMap.toList m)
    ]

pprCallStack :: [CstKey] -> Ppr.Doc
pprCallStack [] = Ppr.text "unknown location"
pprCallStack ks =
  Ppr.vcat $
    [ Ppr.text name Ppr.<+> Ppr.text "at" Ppr.<+> pprSrcLoc loc
    | Cstk name loc <- ks
    ]

pprSrcLoc :: SrcLoc -> Ppr.Doc
pprSrcLoc loc =
  Ppr.text (srcLocFile loc) Ppr.<> Ppr.char ':' Ppr.<>
  Ppr.int (srcLocStartLine loc) Ppr.<> Ppr.char ':' Ppr.<>
  Ppr.int (srcLocStartCol loc)
