{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Extract where

import Language.Syntactic

import Feldspar hiding (Range)
import Feldspar.Core.Constructs (SyntacticFeld)

import Data.List
import Data.Tree

toTreeString :: (SyntacticFeld a) => a -> Tree String
toTreeString = toTree renderSym . reifyFeld defaultFeldOpts N32

type Expr = Tree String
data Def  = Simple Expr
          | Range Expr Expr

fvDef (Simple e) = fv e
fvDef (Range e1 e2) = fv e1 ++ fv e2

instance Show Expr where
    show = showExp

showExp = show . flatten

instance Show Def where
    show (Simple e) = unwords ["Simple", showExp e]
    show (Range e1 e2) = unwords ["Range", showExp e1, showExp e2]

data Analysis = Indexing { arr :: String         -- ^ array variable
                         , ix  :: Expr           -- ^ index expression
                         , vs  :: [String]       -- ^ free variables in index expression
                         , ds  :: [(String,Def)] -- ^ info on free variables
                         , ps  :: [Expr]         -- ^ preconditions
                         }
  deriving (Show)

toLogic :: Analysis -> Logic
toLogic (Indexing{..}) = foldr Forall simps $ nub vs ++ map fst ws
  where
    simps    =  foldr (:&) ranges [Equal (Node v []) d | (v,Simple d) <- ds]
    ranges   =  foldr (:=>) preds [InRange v l u | (v,Range l u) <- ds]
    preds    =  foldr (:=>) inequals $ map IsTrue ps
    inequals =  UnEqual ix (subst "i" "j" ix)

    ws = [('w':tail v,Range (Node v []+1) u) | (v,Range l u) <- ds]

    subst from to = fmap go
      where
        go s | s Prelude.== from = to
        go s = s

data Logic = Forall String Logic
           | Logic :=> Logic
           | Logic :& Logic
           | InRange String Expr Expr
           | IsTrue Expr
           | Equal Expr Expr
           | UnEqual Expr Expr
  deriving (Show)

infixr 5 :=>
infixr 6 :&

log1 = Forall "v"
     $ Forall "i"
     $ Forall "j"
     $ InRange "i" 0 (Node "getLength" [Node "v" []] - 1)
     :=> InRange "j" 0 (Node "getLength" [Node "v" []] - 1)
     :=> UnEqual (Node "i" []) (Node "j" [])
     :=> UnEqual (Node "bla" [Node "i" []]) (Node "bla" [Node "j" []])

indexing = go . toTreeString
  where
    go (Node "(!)" [Node var _,ix])
        | "var" `isPrefixOf` var = [Indexing var ix (fv ix) [] []]
    go (Node "parallel" [len,Node lam [body]])
        | ("Lambda ",i) <- splitAt 7 lam
        = let [li,bi] = map go [len,body]
          in hlp ("var" ++ i) (Range 0 $ len - 1) $ merge [li,bi]
    go (Node "condition" args@[c,t,e]) = let [ci,ti,ei] = map go args
                                         in merge [ci,assert c ti, assert (nt c) ei]
    go (Node "Let" [e,Node lam [body]])
        | ("Lambda ",i) <- splitAt 7 lam
        = hlp ("var" ++ i) (Simple e) $ go body
    go (Node _ args) = concatMap go args

    hlp i def = map (\info@Indexing{..} -> info{ vs=vs++fvDef def, ds=(i,def):ds} )

    merge = concat

    assert p = map (\info@Indexing{..} -> info{ ps=p:ps })

nt e = Node "not" [e]

instance Num Expr where
    fromInteger i = Node (show i) []
    a + b = Node "(+)" [a,b]
    a - b = Node "(-)" [a,b]

fv :: Tree String -> [String]
fv = go
  where
    go (Node var [])     | "var" `isPrefixOf` var = [var]
    go (Node bnd [body]) | ("Lambda ",i) <- splitAt 7 bnd = go body \\ ["var" ++ i]
    go (Node _ args) = concatMap go args

-- indexTree = fmap go . toTreeString
--   where
--     -- go (Node "(!)" [Node var _,ix])
--     --     | "var" `isPrefixOf` var = Node var $ map go ix
--     go (Node s args) = Node s $ map go args
