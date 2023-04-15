{

module Parser
(
      parseFile
    , parseTopLevel
    , AnnotatedStmt (..)
    , Stmt (..)
    , AnnotatedExpr (..)
    , Expr (..)
    , Ty (..)
)
where

import Lexer

}

%name file_parser     File
%name toplevel_parser Toplevel

%tokentype { AnnotatedToken }
%error { parserError }
%monad { Alex }
%lexer { lexer } { ATok Teof _ _ }
%expect 0

%nonassoc is
%nonassoc else
%nonassoc equal less
%left     plus minus
%right    times
%right    arrow

%token
      let     { ATok Tlet _ _    }
      ident   { ATok Tident _ _  }
      equal   { ATok Tequal _ _  }
      true    { ATok Ttrue _ _   }
      false   { ATok Tfalse _ _  }
      lparen  { ATok Tlparen _ _ }
      rparen  { ATok Trparen _ _ }
      int     { ATok Tint _ _    }
      minus   { ATok Tminus _ _  }
      plus    { ATok Tplus _ _   }
      times   { ATok Ttimes _ _  }
      less    { ATok Tless _ _   }
      semisemi { ATok Tsemisemi _ _ }
      if      { ATok Tif _ _     }
      then    { ATok Tthen _ _   }
      else    { ATok Telse _ _   }
      fun     { ATok Tfun _ _    }
      arrow   { ATok Tarrow _ _  }
      colon   { ATok Tcolon _ _  }
      is      { ATok Tis _ _     }
      t_bool  { ATok Tt_bool _ _ }
      t_int   { ATok Tt_int _ _  }

%%

File : {- empty -}        { [] }
     | Expr               { [mkExprStmt $1] }
     | Expr semisemi File { (mkExprStmt $1):$3 }
     | Defs               { $1 }

Defs : Def         { [$1]  }
     | Defs Def    { $2:$1 }

Toplevel : Def semisemi  { $1 }
         | Expr semisemi { mkExprStmt $1 }

Def : let ident equal Expr  { mkAStmt $1 $ SLet (extractLexeme $2) (extractExpr $4) }

Expr : App { $1 }
     | minus int       { mkAExpr $1 $ EInt ((* (-1)) . read . extractLexeme $ $2) }
     | Expr plus Expr  { mkAExpr $2 $ EPlus (extractExpr $1) (extractExpr $3)     }
     | Expr minus Expr { mkAExpr $2 $ EMinus (extractExpr $1) (extractExpr $3)    }
     | Expr times Expr { mkAExpr $2 $ ETimes (extractExpr $1) (extractExpr $3)    }
     | Expr less Expr  { mkAExpr $2 $ ELess (extractExpr $1) (extractExpr $3)     }
     | Expr equal Expr { mkAExpr $2 $ EEqual (extractExpr $1) (extractExpr $3)    }
     | if Expr then Expr else Expr
       { mkAExpr $1 $ EIf (extractExpr $2) (extractExpr $4) (extractExpr $6) }
     | fun ident lparen ident colon Ty rparen colon Ty is Expr
       { mkAExpr $1 $ EFun (extractLexeme $2) (extractLexeme $4) $6 $9 (extractExpr $11) }

App : App Atom { mkAppExpr $1 $2  }
    | Atom     { $1               }

Atom : ident              { mkAExpr $1 $ EIdent (extractLexeme $1)         }
     | true               { mkAExpr $1 $ EBool True                        }
     | false              { mkAExpr $1 $ EBool False                       }
     | int                { mkAExpr $1 $ EInt (read . extractLexeme $ $1)  }
     | lparen Expr rparen { $2                                             }

Ty : t_bool           { TBool }
   | t_int            { TInt  }
   | Ty arrow Ty      { TArrow $1 $3 }
   | lparen Ty rparen { $2 }

{

data Ty = TBool
        | TInt
        | TArrow Ty Ty
        deriving Eq

instance Show Ty where
    show TBool          = "bool"
    show TInt           = "int"
    show (TArrow t1 t2) = show t1 <> " -> " <> show t2

data AnnotatedExpr = AExpr Expr Posn deriving (Eq, Show)

data Expr = EIdent String
          | EBool Bool
          | EInt Int
          | EApp Expr Expr
          | EPlus Expr Expr
          | ETimes Expr Expr
          | ELess Expr Expr
          | EMinus Expr Expr
          | EEqual Expr Expr
          | EIf !Expr !Expr !Expr
          | EFun !String !String !Ty !Ty !Expr
          deriving (Eq, Show)

data AnnotatedStmt = AStmt Stmt Posn deriving (Eq, Show)

data Stmt = SLet String Expr
          | SExpr Expr
          deriving (Eq, Show)

parserError :: AnnotatedToken -> Alex a
parserError (ATok tk _ _) = do
  ((AlexPn _ line column), _, _, remaining) <- alexGetInput

  let error = case tk of
                Tis -> "Invalid use of the 'is' keyword"
                _   -> "Unexpected input: " <> if null remaining then "eof" else take 10 remaining <> "..."

  let hint = case tk of
                Teof -> "Maybe you are missing a ';;' at the end?"
                Tis  -> "Provide a body for the function"
                _    -> ""

  alexError $ mconcat 
    [ "\x1b[31m"
    , "Error at line ", show line, ", column ", show column, ": "
    , error, "\n"
    , "\x1b[m"
    , if null hint
        then ""
        else ("\n  \x1b[34mHint:\x1b[m " <> hint <> "\n")
    ]

extractExpr :: AnnotatedExpr -> Expr
extractExpr (AExpr expr _) = expr

extractLexeme :: AnnotatedToken -> String
extractLexeme (ATok _ _ lexeme) = lexeme

mkExprStmt :: AnnotatedExpr -> AnnotatedStmt
mkExprStmt (AExpr expr pos) = AStmt (SExpr expr) pos

mkAStmt :: AnnotatedToken -> Stmt -> AnnotatedStmt
mkAStmt (ATok _ pos _) stmt = AStmt stmt pos

mkAExpr :: AnnotatedToken -> Expr -> AnnotatedExpr
mkAExpr (ATok _ pos _) expr = AExpr expr pos

mkAppExpr :: AnnotatedExpr -> AnnotatedExpr -> AnnotatedExpr
mkAppExpr (AExpr e1 pos) (AExpr e2 _) = AExpr (EApp e1 e2) pos

lexer = (alexMonadScan >>=)

parseFile s = runAlex s file_parser

parseTopLevel s = runAlex s toplevel_parser

}
