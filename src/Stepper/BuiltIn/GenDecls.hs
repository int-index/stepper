{-# LANGUAGE TemplateHaskellQuotes #-}

module Stepper.BuiltIn.GenDecls
  ( InternableString(..),
    genBuiltInStrings
  ) where

import Data.List (foldl')
import Data.IText (IText, ITextPool, intern)
import Control.Monad.State.Strict (runState, state)
import Data.String (IsString(fromString))
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

data InternableString =
  InternableString {
    str :: !String,           -- the interned string
    alias :: !String          -- alphanumeric alias (for field names)
  }

instance IsString InternableString where
  fromString s = InternableString s s

genBuiltInStrings :: [InternableString] -> TH.DecsQ
genBuiltInStrings strings = do
    rec_decl <-
      -- data BuiltInStrings =
      --   BuiltInStrings {
      --     _Type   :: !IText,
      --     _String :: !IText,
      --     ...
      --   }
      TH.dataD (return []) rec_ty_name [] Nothing [rec_con] []
    fun_decls <-
      [d|
        internBuiltInStrings :: ITextPool -> ($rec_ty, ITextPool)
        internBuiltInStrings = runState $mk_built_in_strings_exp
      |]
    return (rec_decl : fun_decls)
  where
    mk_field_decl :: InternableString -> TH.VarBangTypeQ
    mk_field_decl str = do
      bang <- TH.bang TH.noSourceUnpackedness TH.sourceStrict
      typ  <- [t| IText |]
      return (TH.mkName ('_':str.alias), bang, typ)

    rec_ty_name, rec_con_name :: TH.Name
    rec_ty_name  = TH.mkName "BuiltInStrings"
    rec_con_name = TH.mkName "BuiltInStrings"

    rec_con :: TH.ConQ
    rec_con = TH.recC rec_con_name (map mk_field_decl strings)

    rec_ty :: TH.TypeQ
    rec_ty = TH.conT rec_ty_name

    ap_exp :: TH.ExpQ -> TH.ExpQ -> TH.ExpQ
    ap_exp e1 e2 = TH.infixE (Just e1) [| (<*>) |] (Just e2)

    state_intern_exp :: InternableString -> TH.ExpQ
    state_intern_exp s = [| state (intern $(TH.liftString s.str)) |]

    mk_built_in_strings_exp :: TH.ExpQ
    mk_built_in_strings_exp =
      foldl' ap_exp
        [| pure BuiltInStrings |]
        (map state_intern_exp strings)