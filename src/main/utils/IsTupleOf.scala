package utils


transparent inline def checkTupleOf[S](t: Tuple): Unit = inline t match
    case EmptyTuple => ()
    case t: (S *: xs) => checkTupleOf[S](t.tail)
    case t: (x *: xs) => compiletime.error("t is not a tuple of S!")
