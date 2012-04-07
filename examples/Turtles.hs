-- | Examples of diagrams using the Turtle data type
import Diagrams.Prelude
import Diagrams.TwoD.Path.Turtle.Internal
import Diagrams.Backend.SVG
import Diagrams.Backend.SVG.CmdLine (defaultMain)



instance Show Turtle where
  show t@(Turtle a b c _ _ _) = show (a,b,c)

squareTurtle :: Diagram SVG R2
squareTurtle = getTurtleDiagram $
 startTurtleState #
  forward x # right turn # setPenWidth 2.0 # setPenColour blue  #
  forward x # right turn # setPenWidth 3.0 # setPenColour red   #
  forward x # right turn # setPenWidth 0.5 # setPenColour green #
  forward x
 where
  x = 2.0
  turn = 90


main = defaultMain (squareTurtle # centerXY # pad 1.1)
