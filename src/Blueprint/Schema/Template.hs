module Blueprint.Schema.Template where

import Language.Haskell.TH

mkDecl :: String -> DecQ
mkDecl nameString = do
  let name = mkName nameString
      fields = [
        (name, IsStrict, AppT (AppT ArrowT (ConT GHC.Base.String)))
        ]
      constructor = RecC name fields
  dataD [] name [] constructor

-- | data Starship :: Starship { name :: String }
