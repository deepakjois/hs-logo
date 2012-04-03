-- | Examples of diagrams using the Turtle data type

import Diagrams.Prelude
import Diagrams.TwoD.Path.Turtle.Internal (startTurtleState, forward, right, getTurtleDiagram)
import Diagrams.Backend.SVG.CmdLine (defaultMain)

squareTurtle = getTurtleDiagram $
 startTurtleState #
  forward x # right turn #
  forward x # right turn #
  forward x # right turn #
  forward x
 where
  x = 2.0
  turn = 90

main = defaultMain (squareTurtle # centerXY # pad 1.1)
