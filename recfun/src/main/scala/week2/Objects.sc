object rationals {
  val x = new Rational(1, 2)
  x.denom

  x

  class Rational(x: Int, y: Int) {
    def numer = x
    def denom = y

    override def toString = s"$numer/$denom"
  }
}

