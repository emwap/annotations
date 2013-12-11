{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Extract where

import Language.Syntactic
import Language.Syntactic.Constructs.Binding (Variable(..), VarId)

import Feldspar
import Feldspar.Core.Constructs (SyntacticFeld)
import Feldspar.Core.Interpretation (prjF)

vars :: (SyntacticFeld a)
     => a -> [VarId]
vars = listFold (\a vs -> concat $ varId a : vs) . reifyFeld defaultFeldOpts N32

varId :: (Project (Variable :|| Type) sup)
      => sup sig -> [VarId]
varId (prjF -> (Just (C' (Variable v)))) = [v]
varId _ = []

