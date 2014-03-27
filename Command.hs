module Command where

import Parser ( BinOp )

data Command = Operator BinOp
             | Push Integer
             | Lvalue String
             | Rvalue String
             | Pop
             | Assign
             | Copy
             | PrintCmd
             | Label String
             | Goto String
             | GoTrue String
             | GoFalse String
             | Halt
    deriving (Eq, Show)
