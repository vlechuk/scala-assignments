  /**
    * We represent a set by its characteristic function, i.e.
    * its `contains` predicate.
    */
  type Set = Int => Boolean

  def contains(s: Set, elem: Int): Boolean = s(elem)
  def singletonSet(elem: Int): Set = y => y == elem
  def union(s: Set, t: Set): Set = elem => s(elem) || t(elem)
  def intersect(s: Set, t: Set): Set = elem => s(elem) && t(elem)
  def diff(s: Set, t: Set): Set = elem => s(elem) && !t(elem)
  def filter(s: Set, p: Int => Boolean): Set = intersect(s, p)
  val bound = 1000

  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a > bound) true
      else if (contains(s, a) && !p(a)) false
      else iter(a + 1)
    }
    iter(-bound)
  }

  def map(s: Set, f: Int => Int): Set = {
    def iter(a: Int, finalSet: Set): Set = {
      if (a > bound) finalSet
      else if (contains(s, a)) iter(a + 1, union(finalSet, singletonSet(f(a))))
      else iter(a + 1, finalSet)
    }
    iter(-bound, x => x != x)
  }

  def exists(s: Set, p: Int => Boolean): Boolean = !forall(s, x => !p(x))

  def toStringNew(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
    * Prints the contents of a set on the console.
    */
  def printSet(s: Set) {
    println(toStringNew(s))
  }



  def a: Set = x => (1 <= x) && (x <= 15)
  def b: Set = x => (10 <= x) && (x <= 20)
  def c: Set = union(a, b)
  def e: Set = x => x != x
  def d: Set = x => (1 <= x) && (x <= 5)


  contains(c, 1)

  printSet(diff(a,b))

  printSet(c)

  printSet(a)

  printSet(filter(a, x => x * 2 < 10))

  printSet(e)

  printSet(union(a,e))

  printSet(d)

  printSet(map(d, x => x + 1))

  forall(d, x => x < 4)

  exists(d, x => x >= 5)


