module Error
(
      mkErr
    , mkErr'
    , Error
)
where

import           Lexer (Posn (..))


data Error = MkError !Posn !String !(Maybe String)

mkErr :: Posn -> String -> Error
mkErr pos msg = MkError pos msg Nothing

mkErr' :: Posn -> String -> String -> Error
mkErr' pos msg hint = MkError pos msg (Just hint)

instance Show Error where
    show (MkError (Posn line column) msg hint) = mconcat
        [ errHeader line column msg
        , errHint hint
        ]

errHeader :: Int -> Int -> String -> String
errHeader line col msg =
    "\x1b[31mError at line " <> show line <> ", column " <> show col <> ":\x1b[m\n\n"
    <> "    " <> msg <> "\n\n"

errHint :: Maybe String -> String
errHint (Just hint) = "    Hint: " <> hint <> "\n\n"
errHint _           = ""
