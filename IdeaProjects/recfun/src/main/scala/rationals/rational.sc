object rat {


  class Rational (x: Int, y: Int){
    def fx = x
    def fy = y

    override def toString = x + "/" + y

    def sub(that: Rational): Rational = {

      new Rational(
        that.fy * this.fx - this.fy * that.fx,
        that.fy * this.fy
      )

    }

  }

  val ns = (new Rational(1, 3)).sub(new Rational(5, 7)).sub(new Rational(3, 2))
  println(ns)


  def sum(f: Int => Int):((Int, Int) => Int) = {
    def sumf(a: Int, b: Int): Int = {
      if(a > b) 0
      else f(a) + sumf(a+1, b)
    }
    sumf
  }

  def sumOfSq = sum(x => x * x)
  sumOfSq(1,2)

  def sum2(f: Int => Int)(a: Int, b: Int): Int = {
      if(a > b) 0
      else f(a) + sum2(f)(a+1, b)
  }

  def sumOfSq2 = sum2(x => x * x)_
  sumOfSq2(1,2)

  sum2(x => x * x)(1,2)

}