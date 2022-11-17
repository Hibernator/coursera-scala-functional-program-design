import java.util.Random

trait Generator[+T]:
  def generate(): T

extension [T, S](g: Generator[T])
  def map(f: T => S) = new Generator[S]:
    override def generate(): S = f(g.generate())

  def flatMap(f: T => Generator[S]) = new Generator[S]:
    override def generate(): S = f(g.generate()).generate()

val integers: Generator[Int] = new Generator[Int]:
  val rand: Random = java.util.Random()
  def generate(): Int = rand.nextInt()

val booleans = new Generator[Boolean]:
  override def generate(): Boolean = integers.generate() > 0

val booleans2 = for x <- integers yield x > 0

val pairs = new Generator[(Int, Int)]:
  override def generate(): (Int, Int) =
    (integers.generate(), integers.generate())

def pairs2[T, U](t: Generator[T], u: Generator[U]) =
  for
    x <- t
    y <- u
  yield (x, y)

pairs.generate()

def single[T](x: T): Generator[T] =
  new Generator[T]:
    override def generate() = x

def range(lo: Int, hi: Int): Generator[Int] =
  for x <- integers yield lo + x.abs % (hi - lo)

def oneOf[T](xs: T*): Generator[T] =
  for idx <- range(0, xs.length) yield xs(idx)

def lists: Generator[List[Int]] =
  for
    kind <- range(0, 5)
    list <- if kind == 0 then emptyLists else nonEmptyLists
  yield list

def emptyLists = single(Nil)

def nonEmptyLists =
  for
    head <- integers
    tail <- lists
  yield head :: tail

lists.generate()

enum Tree:
  case Inner(left: Tree, right: Tree)
  case Leaf(x: Int)

def leaves: Generator[Tree] = for integer <- integers yield Tree.Leaf(integer)

def inners =
  for
    x <- trees
    y <- trees
    yield Tree.Inner(x, y)

def trees: Generator[Tree] =
  for
    isLeaf <- booleans2
    tree <- if isLeaf then leaves else inners
  yield tree
