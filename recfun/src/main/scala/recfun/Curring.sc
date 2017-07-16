object Curring {
  def product(f: Int => Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, f(a) * acc)
    }
    loop(a, 1)
  }

  product(x => x)(1, 5)

  def factorial(n: Int): Int = {
    product(x => x)(1, n)
  }

  factorial(6)

  def range(f: Int => Int, op: (Int, Int) => Int, unit: Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, op(f(a), acc))
    }
    loop(a, unit)
  }

  def prod(f: Int => Int, a: Int, b: Int): Int =
    range(f, (x, y) => x * y, 1)(a, b)

  prod(x => x, 1, 5)
}