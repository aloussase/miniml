{

module Lexer 
(
    Token (..)
  , AnnotatedToken (..)
  , Posn (..)
  , runScanner
  , alexMonadScan
  , Alex
  , alexError
  , runAlex
  , alexGetInput
)
where

}

%wrapper "monad"

$digit = 0-9
$alpha = [a-zA-Z]

@id     = $alpha [$alpha $digit \_]*
@number = $digit+

tokens :-
  
  $white+                   { skip              }
  @number                   { mkToken Tint      }
  let                       { mkToken Tlet      }
  int                       { mkToken Tt_int    }
  bool                      { mkToken Tt_bool   }
  true                      { mkToken Ttrue     }
  false                     { mkToken Tfalse    }
  fun                       { mkToken Tfun      }
  is                        { mkToken Tis       }
  if                        { mkToken Tif       }
  then                      { mkToken Tthen     }
  else                      { mkToken Telse     }
  ";;"                      { mkToken Tsemisemi } 
  "="                       { mkToken Tequal    } 
  "<"                       { mkToken Tless     } 
  "->"                      { mkToken Tarrow    } 
  ":"                       { mkToken Tcolon    } 
  "("                       { mkToken Tlparen   } 
  ")"                       { mkToken Trparen   } 
  "+"                       { mkToken Tplus     } 
  "-"                       { mkToken Tminus    } 
  "*"                       { mkToken Ttimes    } 
  @id                       { mkToken Tident    }

{

data Posn = Posn !Int !Int deriving (Eq, Show)

data AnnotatedToken = ATok Token Posn String deriving (Eq, Show)

data Token
  = Tlet
  | Tt_int
  | Tt_bool
  | Ttrue
  | Tfalse
  | Tfun
  | Tis
  | Tif
  | Tthen
  | Telse
  | Tsemisemi
  | Tequal
  | Tless
  | Tarrow
  | Tcolon
  | Tlparen
  | Trparen
  | Tplus
  | Tminus
  | Ttimes
  | Tident
  | Tint
  | Teof
  deriving (Eq, Show)

alexEOF = do
    (AlexPn _ line col, _, _, _) <- alexGetInput
    pure $ ATok Teof (Posn line col) "<eof>"

mkToken :: Token -> AlexInput -> Int -> Alex AnnotatedToken
mkToken tk (AlexPn _ line col, _, _, input) len = pure $ ATok tk (Posn line col) (take len input)

runScanner source = runAlex source $ do
    let go ts = do t@(ATok tk _ _) <- alexMonadScan
                   if tk == Teof
                      then pure $ reverse (t:ts)
                      else go (t:ts)
    go []

}
