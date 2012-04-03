module Logo.Builtins.Arithmetic (arithmeticBuiltins) where

import qualified Data.Map as M

import Logo.Types

sin_, cos_, tan_, arctan, sqrt_ :: [LogoToken] -> LogoEvaluator LogoToken

arithmeticBuiltins :: M.Map String LogoFunctionDef
arithmeticBuiltins = M.fromList
  [ ("sin",      LogoFunctionDef 1 sin_)
  , ("cos",      LogoFunctionDef 1 cos_)
  , ("tan",      LogoFunctionDef 1 tan_)
  , ("arctan",   LogoFunctionDef 1 arctan)
  , ("sqrt",     LogoFunctionDef 1 sqrt_)
  ]

sin_ [NumLiteral n] = return $ NumLiteral (sin $ fromDegrees n)
sin_ _ = error "Invalid arguments for sin"

cos_ [NumLiteral n] = return $ NumLiteral (cos $ fromDegrees n)
cos_ _ = error "Invalid arguments for cos"

tan_ [NumLiteral n] = return $ NumLiteral (tan $ fromDegrees n)
tan_ _ = error "Invalid arguments for cos"

arctan [NumLiteral n] = return $ NumLiteral (atan $ fromDegrees n)
arctan _ = error "Invalid arguments for cos"

fromDegrees :: Double -> Double
fromDegrees n = n * (pi/180)

sqrt_ [NumLiteral n] = return . NumLiteral . sqrt $ n
sqrt_ _ = error "Invalid arguments to sqrt"
