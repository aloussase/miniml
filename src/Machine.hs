module Machine where

type Frame = [Inst]

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
