module QQ where

import qualified Language.Haskell.TH.Quote as TH
import qualified Language.Haskell.TH.Syntax as TH

string :: TH.QuasiQuoter
string = TH.QuasiQuoter
  { TH.quoteDec = \ _ -> fail "cannot use [string|...|] quasi-quotation as a declaration"
  , TH.quoteExp = \ x -> pure (TH.LitE (TH.StringL x))
  , TH.quotePat = \ x -> pure (TH.LitP (TH.StringL x))
  , TH.quoteType = \ x -> pure (TH.LitT (TH.StrTyLit x))
  }
