package calculator

enum Expr:
  case Literal(v: Double)
  case Ref(name: String)
  case Plus(a: Expr, b: Expr)
  case Minus(a: Expr, b: Expr)
  case Times(a: Expr, b: Expr)
  case Divide(a: Expr, b: Expr)

object Calculator extends CalculatorInterface:
 import Expr.*

  extension (expr: Expr)
    private def referencesItself: Boolean =
      def containsItself(a: Expr, b: Expr): Boolean = if a == expr || b == expr then true else false

      expr match
        case Plus(a, b) => containsItself(a, b)
        case Minus(a, b) => containsItself(a, b)
        case Times(a, b) => containsItself(a, b)
        case Divide(a, b) => containsItself(a, b)
        case _ => false

  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] =
    namedExpressions.view.mapValues(e => Signal(eval(e(), namedExpressions))).toMap

  def eval(expr: Expr, references: Map[String, Signal[Expr]])(using Signal.Caller): Double =
    if expr.referencesItself then Double.NaN
    else expr match
      case Literal(v) => v
      case Ref(name) => eval(getReferenceExpr(name, references), references.removed(name))
      case Plus(a, b) => eval(a, references) + eval(b, references)
      case Minus(a, b) => eval(a, references) - eval(b, references)
      case Times(a, b) => eval(a, references) * eval(b, references)
      case Divide(a, b) => eval(a, references) / eval(b, references)

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]])(using Signal.Caller): Expr =
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
