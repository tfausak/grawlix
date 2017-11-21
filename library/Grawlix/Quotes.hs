module Grawlix.Quotes
  ( string
  ) where

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as TH


string :: TH.QuasiQuoter
string = TH.QuasiQuoter
  { TH.quoteDec = const $ fail "cannot use as a declaration"
  , TH.quoteExp = pure . TH.LitE . TH.StringL
  , TH.quotePat = pure . TH.LitP . TH.StringL
  , TH.quoteType = pure . TH.LitT . TH.StrTyLit
  }
