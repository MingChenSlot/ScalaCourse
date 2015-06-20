package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions map {
      case (name, expr) => (name, Signal(eval(expr(), namedExpressions)))
    }
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    // calculating infinite loop
    def detectInfiniteLoop(rightExpr: Expr, visited: Set[Expr]): Boolean = {
      rightExpr match {
        case (l: Literal) => false
        case (r: Ref) =>
          val exprRef = getReferenceExpr(r.name, references)
          if (visited contains exprRef) return true
          else
            detectInfiniteLoop(exprRef, visited + exprRef)
        case (o: Plus) =>
          (detectInfiniteLoop(o.a, visited) | detectInfiniteLoop(o.b, visited))
        case (o: Minus) =>
          (detectInfiniteLoop(o.a, visited) | detectInfiniteLoop(o.b, visited))
        case (o: Times) =>
          (detectInfiniteLoop(o.a, visited) | detectInfiniteLoop(o.b, visited))
        case (o: Divide) =>
          (detectInfiniteLoop(o.a, visited) | detectInfiniteLoop(o.b, visited))
      }
    }

    if (detectInfiniteLoop(expr, Set(expr))) Double.NaN
    else {
      expr match {
        case (l: Literal) => l.v
        case (r: Ref) => val ref = references.get(r.name)
          ref match {
            case Some(v) => eval(getReferenceExpr(r.name, references), references)
            case None => Double.NaN
          }
        case (p: Plus) => eval(p.a, references) + eval(p.b, references)
        case (m: Minus) => eval(m.a, references) - eval(m.b, references)
        case (t: Times) => eval(t.a, references) * eval(t.b, references)
        case (d: Divide) => eval(d.a, references) / eval(d.b, references)
      }
    }
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
