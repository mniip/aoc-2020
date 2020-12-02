{-# LANGUAGE BangPatterns, ConstraintKinds, DataKinds, DefaultSignatures, DeriveAnyClass, DeriveDataTypeable, DeriveGeneric, DeriveLift, DeriveTraversable, DerivingVia, EmptyCase, EmptyDataDecls, EmptyDataDeriving, ExistentialQuantification, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, HexFloatLiterals, ImplicitParams, ImportQualifiedPost, InstanceSigs, KindSignatures, LambdaCase, LiberalTypeSynonyms, MagicHash, MultiParamTypeClasses, MultiWayIf, NoMonomorphismRestriction, NumericUnderscores, OverloadedLabels, PackageImports, ParallelListComp, PartialTypeSignatures, PatternGuards, PatternSynonyms, PolyKinds, PostfixOperators, QuantifiedConstraints, QuasiQuotes, RankNTypes, RecordWildCards, RecursiveDo, RoleAnnotations, ScopedTypeVariables, StandaloneDeriving, StandaloneKindSignatures, TemplateHaskell, TupleSections, TypeApplications, TypeFamilies, TypeFamilyDependencies, TypeInType, TypeOperators, UnboxedSums, UnboxedTuples, UnicodeSyntax, UnliftedNewtypes, ViewPatterns, StarIsType #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Main where

import Control.Applicative
import Control.Arrow
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Comonad.Store
import Control.Exception
import Control.Exception.Lens
import Control.Monad
import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Error.Lens
import Control.Monad.Fix
import Control.Monad.Identity
import Control.Monad.RWS
import Control.Monad.Reader
import Control.Monad.ST
import Control.Monad.State
import Control.Monad.Writer
import Data.Array
import Data.Array.Lens
import Data.Bits
import Data.Bits.Lens
import Data.Bool
import Data.Char
import Data.Complex
import Data.Complex.Lens
import Data.Data
import Data.Data.Lens
import Data.Dynamic
import Data.Dynamic.Lens
import Data.Either
import Data.Eq
import Data.Fixed
import Data.Function
import Data.Graph
import Data.IORef
import Data.Int
import Data.Ix
import Data.Kind
import Data.List
import Data.List.Lens
import Data.List.Split
import Data.Maybe
import Data.Monoid
import Data.Number.CReal
import Data.Number.Interval
import Data.Number.Natural
import Data.Ord
import Data.Ratio
import Data.STRef
import Data.Tree
import Data.Tree.Lens
import Data.Tuple
import Data.Typeable
import Data.Typeable.Lens
import Data.Void
import Data.Word
import GHC.Prim
import GHC.Word
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Numeric
import Numeric.Lens
import Text.Printf
import Data.ByteString.Lens
import Data.Foldable
import Data.Sequence.Lens
import Data.Set.Lens
import Data.Traversable
import Debug.Trace
import System.Environment
import System.Directory
import System.IO
import System.IO.Unsafe
import Unsafe.Coerce

import qualified Control.Category as C
import Control.Category hiding (id, (.))
import Control.Lens hiding (elements, index, indices, levels, uncons)
import qualified Control.Lens as Lens
import Data.Number.Symbolic hiding (var)
import qualified Data.Number.Symbolic as Sym
import System.Random hiding (split)
import qualified System.Random as R
import qualified Text.ParserCombinators.ReadP as P
import Text.PrettyPrint.HughesPJ hiding (empty, first, (<>))
import qualified Text.PrettyPrint.HughesPJ
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Constraint (Dict(..), withDict, (:-)(..), mapDict, unmapDict)
import qualified Data.Foldable as F
import Data.Functor.Base hiding (head, tail)
import Data.Functor.Foldable hiding (fold, gunfold)
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Traversable as T
import GHC.Exts hiding (toList)

main :: IO ()
main = do
  args <- getArgs
  input <- readInput <$> getContents
  when ("1" `elem` args || null args) $ printOutput $ solve1 input
  when ("2" `elem` args || null args) $ printOutput $ solve2 input

fromFile filename = readInput $ unsafePerformIO $ readFile filename

readInput :: String -> _
readInput = map (read @Int) . lines

printOutput :: _ -> IO ()
printOutput = print @Int

solve1 :: _ -> _
solve1 xs = error "solve1"

solve2 :: _ -> _
solve2 xs = error "solve2"
