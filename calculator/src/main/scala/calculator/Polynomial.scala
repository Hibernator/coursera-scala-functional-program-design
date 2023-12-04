package calculator

object Polynomial extends PolynomialInterface:
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] =
    Signal {
      val bValue = b()
      bValue * bValue - 4 * a() * c()
    }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] =
    Signal {
      val discriminant = computeDelta(a, b, c)()
      if discriminant < 0 then
        Set[Double]()
      else
        val bValue = b()
        val aValue = a()
        val result1 = (-bValue + Math.sqrt(discriminant)) / (2 * aValue)
        val result2 = (-bValue - Math.sqrt(discriminant)) / (2 * aValue)
        Set(result1, result2)
    }
