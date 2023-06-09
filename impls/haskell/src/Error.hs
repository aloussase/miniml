{-# LANGUAGE DeriveAnyClass #-}
module Error
(
      mkErr
    , mkErr'
    , mkErrNoPos
    , Error
)
where

import           Control.Exception (Exception)
import           Lexer             (Posn (..))


data Error = MkError !(Maybe Posn) !String !(Maybe String) deriving Exception

mkErrNoPos :: String -> Error
mkErrNoPos msg =  MkError Nothing msg Nothing

mkErr :: Posn -> String -> Error
mkErr pos msg = MkError (Just pos) msg Nothing

mkErr' :: Posn -> String -> String -> Error
mkErr' pos msg hint = MkError (Just pos) msg (Just hint)

instance Show Error where
    show (MkError Nothing msg hint) = mconcat
        [ "Error: ", msg, "\n\n"
        , errHint hint
        ]
    show (MkError (Just (Posn line column)) msg hint) = mconcat
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
