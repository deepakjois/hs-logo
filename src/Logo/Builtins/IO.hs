module Logo.Builtins.IO (ioBuiltins) where

import qualified Data.Map as M

import Control.Applicative ((<$>))
import Control.Monad.Trans (lift, liftIO)

import System.Random (randomIO)

import Logo.Types

turtleIO :: IO a -> LogoEvaluator a
turtleIO = lift . liftIO

pr, random :: [LogoToken] -> LogoEvaluator LogoToken

ioBuiltins :: M.Map String LogoFunctionDef
ioBuiltins = M.fromList
  [ ("pr",       LogoFunctionDef 1 pr)
  , ("random",   LogoFunctionDef 1 random)
  ]

pr [t] = turtleIO $ do
  putStrLn (show t)
  return $ StrLiteral ""

pr _ = error "Invalid arguments to pr"

random [NumLiteral n] = turtleIO $ (NumLiteral . fromIntegral . (round :: Double -> Integer) . (* n) <$> randomIO)

random _ = error "Invalid arguments to random"
