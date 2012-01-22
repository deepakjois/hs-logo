module Logo.Types where


data LogoToken = Identifier String -- ^ Identifier
               | StrLiteral String -- ^ String Literal, e.g @"word@
               | VarLiteral String -- ^ Variable, e.g @:size@
               | NumLiteral Double -- ^ Number
               | OperLiteral String  -- ^ Operator
               | List [LogoToken] -- ^ Input definition/variable reference

               deriving Show




