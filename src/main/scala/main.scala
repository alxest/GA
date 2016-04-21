import scala.io.Source
import better.files._, Cmds._
import java.io.{File => JFile}
import scala.annotation.tailrec
import scala.util.Random
// http://stackoverflow.com/questions/15639078/scala-class-constructor-parameters
// If you prefix parameters with val, var they will be visible from outside of class, otherwise, they will be private, as you can see in code above.

class Graph(val size: Int, val edges: Map[(Int, Int), Int]) {

  def this(size: Int) = this(size, Map())

  override def toString = edges.foldLeft("")(
    (s: String, x) => x._1._1 + " " + x._1._2 + " " + x._2 + "\n" + s)

  def random_node = Random.nextInt(size) + 1

  def spread_edges(m: Int, weight: (Int, Int), forbidden: Set[Range]) = {
    val (from, to) = weight
    def random_weight = Random.nextInt(to-from) + from

    var e = edges
    while(e.size < m) {
      val a = random_node
      val b = random_node
      if(forbidden.forall(x => ! (x.contains(a) && x.contains(b))))
        e += new Graph.Edge((a, b), random_weight)
    }
    new Graph(size, e)
  }
  def valuation(a: BinaryChromosome.BC): Double = {
    val t = for {
      ((from, to), weight) <- this.edges
      if(a(from-1) != a(to-1))
    } yield weight
    t.foldLeft(0.0)(_ + _)
  }
}

object Graph {
  type Edge = ((Int, Int), Int)
}

class GA[A](
  val pool: List[A],
  val crossover: (A, A) => A,
  val mutation: A => A,
  val valuation: A => Double,
  val find_parent: List[Double] => (Int, Int),
  val selection: (List[Double], Int) => Set[Int]) {

  lazy val current_value = pool.map(x => valuation(x))

  def get_sibling = {
    val (mama, papa): (Int, Int) = find_parent(current_value)
    mutation(crossover(pool(mama), pool(papa)))
  }

  lazy val next: GA[A] = {
    val siblings = for {
      i <- (1 to GA.k_size)
    } yield get_sibling
    val replaced = selection(current_value, GA.k_size)
    assert(replaced.size == siblings.size)

    val next_pool =
      pool.zipWithIndex.filterNot(x => replaced.contains(x._2)).map(_._1) ++ siblings

    println(s"Current sum of valuation : ${current_value.foldLeft(0.0)(_ + _)} ${get_best._2}")
    new GA[A](next_pool, crossover, mutation, valuation, find_parent, selection)
  }

  def progress(n: Int): GA[A] = {
    println(n)
    if(n > 0) this.next.progress(n-1)
    else this
  }

  def get_best: (A, Double) = {
    val (sol, idx) = current_value.zipWithIndex.sortWith(_._1 > _._1).head
    (pool(idx), sol)
  }
}

object GA {
  val pool_size = 200
  val k_size = pool_size / 2
}

class BinaryChromosome(length: Int) { // extends Chromosome {
  val maxval = 2
  def random_binary: BinaryChromosome.BC =
    (1 to length).map(_ => Random.nextInt(maxval)).toList
  def crossover(a: BinaryChromosome.BC, b: BinaryChromosome.BC): BinaryChromosome.BC = {
    assert(a.size == b.size)
    assert(b.size == length)
    val l = a.size
    val k = Random.nextInt(l)
    a.slice(0, k) ++ b.slice(k, l)
  }
  def mutation(a: BinaryChromosome.BC): BinaryChromosome.BC = {
    assert(a.size == length)
    def go(n: Int): BinaryChromosome.BC =
      if(n > 0) {
        val k = Random.nextInt(a.size)
        go(n-1).updated(k, Random.nextInt(maxval))
      }
      else a
    go(1)
  }
}

object BinaryChromosome {
  type BC = List[Int]
}

object BasicSelection{

  def find_parent(a: List[Double]): (Int, Int) = {
    val x = a.zipWithIndex.sortWith(_._1 > _._1)
    (x(0 + Random.nextInt(3))._2, x(1 + Random.nextInt(3))._2)
  }

  def selection(a: List[Double], replaced: Int): Set[Int] = {
    val b = a.zipWithIndex
    val c = b.sortWith((x, y) => x._1 < y._1)
    val mark_with_false: List[Int] = c.take(replaced).map(_._2)
    val res = mark_with_false.toSet
    assert(res.size == replaced)
    res
  }
}

object main extends Application {
  val dir = File(System.getProperty("user.dir"))
  val matches: Iterator[File] = dir.glob("**/500_5000_pos_2coclique.{in}")
  // val matches: Iterator[File] = dir.glob("**/1000_5000_pos_2coclique.{in}")
  // val matches: Iterator[File] = dir.glob("**/*_pos_2coclique.{in}")
  matches.foreach { f =>
    println(f)
    val lines: Array[String] = f.lines.toArray
    val (n, m) = {
      val t = lines(0).split(" ")
      (t(0).toInt, t(1).toInt)
    }
    val edge_data = lines.drop(1).filterNot(_.trim == "").map(_.split(" ")).map(
      x => {
        assert(x.length == 3, s"${x} length not 3, but ${x.length}")
        new Graph.Edge((x(0).toInt, x(1).toInt), x(2).toInt)
      }
    ).toMap
    assert(edge_data.size == m, s"${m} is not ${edge_data.size}")
    val g = new Graph(n, edge_data)
    val LIC = new BinaryChromosome(n)
    val ga = new GA[List[Int]](
      List.fill(GA.pool_size)(LIC.random_binary),
      LIC.crossover,
      LIC.mutation,
      g.valuation,
      BasicSelection.find_parent,
      BasicSelection.selection
    )
    val ga_ = ga.progress(50)
    println(ga_.get_best)
    println(s"${n} ${m}")
  }
}
