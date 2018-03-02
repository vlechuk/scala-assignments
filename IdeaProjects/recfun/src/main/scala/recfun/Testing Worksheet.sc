7.+(2)
7 + 2
val e = "s" toLowerCase
val e2 = "s".toLowerCase
//
//  def sum(f: Int => Int, a: Int, b: Int): Int = {
//    def loop(a: Int, acc: Int): Int =
//      if (a > b) acc
//      else loop(a + 1, f(a) + acc)
//    loop(a, 0)
//  }
//
//  sum(x => x * x, 3, 4)
//  sum(x => x * x, 3, 3)


def fun(f: Int => Int)(a: Int, b: Int):Int =
{
  def fun_inner(aa: Int, bb: Int) = {
    f(aa) + f(bb)
  }
  fun_inner(a,b)
}

def product(f: Int => Int, a: Int, b:Int): Int =
  if(a > b) 1
  else f(a) * product(f, a + 1, b)

def summa(f: Int => Int, a: Int, b:Int): Int =
  if(a > b) 0
  else f(a) + summa(f, a + 1, b)


product(x=>2+x,1,2)

def fact(n: Int) = product(x => x,1,n)

fact(4)
summa(x=>x,1,3)


def mapReduce(f: (Int) => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int =
{
  if (a > b) zero
  else
    combine(f(a), mapReduce(f, combine, zero)(a + 1, b))
}

def f(n: Int): Int = {

  def fm(mult: Int, n: Int): Int = {
    if (n > 1) fm(n * mult, n - 1)
    else mult
  }

  if (n == 0) 1
  else fm(1, n)
}

f(1)

def sum(f: Int => Int, a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc
    else loop(a + 1, acc + f(a))
  }
  loop(a, 0)
}


sum(x => x + x, 1, 3)

def test(a: Int, b: Int) =
  a + b

def test2(a: Int)(b: Int) =
  a + b

test2 (1)(2)
test (1,2)















