module Machine
(
    run
    , Frame
    , MValue (..)
    , Inst (..)
    , Environ
)
where

import           Control.Exception (throw)

import           Error

type Frame = [Inst]

type Environ = [(String, MValue)]

type Stack = [MValue]

data MValue = MInt !Int
            | MBool !Bool
            | MClosure !String !Frame !Environ
            deriving Eq

instance Show MValue where
    show (MInt n)          = show n
    show (MBool b)         = show b
    show (MClosure _ _ _ ) = "<function>"

data Inst = IMult
          | IAdd
          | ISub
          | IEqual
          | ILess
          | IVar !String
          | IInt !Int
          | IBool !Bool
          | IClosure !String !String !Frame
          | IBranch !Frame !Frame
          | ICall
          | IPopEnv
          deriving (Eq, Show)

machineError :: String -> a
machineError msg = throw $ mkErrNoPos msg

lookupEnv :: [Environ] -> String -> MValue
lookupEnv [] ident = machineError $ "Unbound variable: " <> ident
lookupEnv (env:_) ident =
    case lookup ident env of
        Just value -> value
        _          -> machineError $ "Unbound variable: " <> ident

pop :: Stack -> (MValue, Stack)
pop (x : xs) = (x, xs)
pop _        = machineError "Empty stack"

popBool :: Stack -> (Bool, Stack)
popBool (MBool b : s) =  (b, s)
popBool _             = machineError "Expected bool"

popApp :: Stack -> (MValue, String, Frame, Environ, Stack)
popApp (v: MClosure ident body env : s) = (v, ident, body, env, s)
popApp _ = machineError "Expected function application"

mult :: Stack -> Stack
mult (MInt x : MInt y : s) = MInt (y * x):s
mult _ = machineError "int and int expected for binary operator '*'"

add :: Stack -> Stack
add (MInt x : MInt y : s) = MInt (y + x):s
add _ = machineError "int and int expected for binary operator '+'"

sub :: Stack -> Stack
sub (MInt x : MInt y : s) = MInt (y - x):s
sub _ = machineError "int and int expected for binary operator '-'"

equal :: Stack -> Stack
equal (MInt x : MInt y : s) = MBool (y == x):s
equal _ = machineError "int and int expected for binary operator '='"

less :: Stack -> Stack
less (MInt x : MInt y : s) = MBool (y < x):s
less _ = machineError "int and int expected for binary operator '<'"

exec :: Inst -> [Frame] -> Stack -> [Environ] -> ([Frame], Stack, [Environ])
exec IAdd frms stack env         = (frms, add stack, env)
exec IMult frms stack env        = (frms, mult stack, env)
exec ISub frms stack env         = (frms, sub stack, env)
exec IEqual frms stack env       = (frms, equal stack, env)
exec ILess frms stack env        = (frms, less stack, env)
exec (IVar ident) frms stack env = (frms, lookupEnv env ident : stack, env)
exec (IInt n) frms stack env     = (frms, MInt n : stack, env)
exec (IBool b) frms stack env    = (frms, MBool b : stack, env)
exec (IClosure ident param body) frms stack envs =
    case envs of
        env:_ ->
            let c = MClosure param body ((ident,c):env)
             in (frms, c:stack, envs)
        _    -> machineError "No environment for closure"


run :: Frame -> Environ -> MValue
run frm env =
    let loop args = case args of
                        ([], [v], _) -> v
                        ((i:is):frms, stack, envs) -> loop (exec i (is:frms) stack envs)
                        ([]:frms, stack, envs) -> loop (frms, stack, envs)
                        _ -> machineError "Illegal end of program"
     in
        loop ([frm], [], [env])
