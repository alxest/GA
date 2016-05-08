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
    var res = 0.0
    this.edges.foreach(x => {
      val ((from, to), weight) = x
      if(a(from-1)._1 != a(to-1)._1) res += weight})
    //removing {} will cause error...
    res
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
  val selection: (List[Double], Int) => Set[Int],
  val value_cache: Map[A, (Double, Int)],
  val one_distances: A => List[A]) {

  //mutation 1

  lazy val new_value_cache: Map[A, (Double, Int)] = {
    var vc: Map[A, (Double, Int)] = value_cache
    pool.foreach{x =>
      if(! vc.keySet.contains(x))
        vc = vc.updated(x, (valuation(x), 10))
      else {
        val (value, old_age) = vc(x)
        vc = vc.updated(x, (value, old_age+10))
      }
      assert(vc.keySet.contains(x))
    }

    vc = vc.map{x =>
      val (key, (value, age)) = x
      (key, (value, age-1))
    }
    vc = vc.filter(_._2._2 != 0)
    println("vc size : " + vc.size)
    vc
  }

  lazy val current_value = pool.map(x =>
      new_value_cache(x)._1)

  def get_sibling = {
    val (mama, papa): (Int, Int) = find_parent(current_value)
    mutation(crossover(pool(mama), pool(papa)))
  }

  lazy val next: GA[A] = {
    // println("get_sibling")
    val siblings = List.fill(GA.k_size)(get_sibling)
    // println("get_replaced")
    val replaced = selection(current_value, GA.k_size)
    assert(replaced.size == siblings.size)

    // println("calculate next_pool")
    val next_pool =
      pool.zipWithIndex.filterNot(x => replaced.contains(x._2)).map(_._1) ++ siblings

    // if(avg >= get_best._2 * 0.99)
    // if(get_best._2 >= 3220)
    //   println(s"Best's 1 distance value changes: ${one_distances(get_best._1).map(x =>
    //     if(new_value_cache.keySet.contains(x))
    //       new_value_cache(x)._1
    //     else
    //       valuation(x)).max}")
    new GA[A](next_pool, crossover, mutation, valuation, find_parent, selection, new_value_cache, one_distances)
  }
}

object GA {
  val pool_size = 15
  val k_size = (pool_size / 1.1).toInt
}

class BinaryChromosome(length: Int) { // extends Chromosome {
  val maxval = 2
  val default_age = 1.0E-7
  val aging = 1.04 // 1.04
  val mutated_new_age = 1.0E-7
  def random_binary: BinaryChromosome.BC =
    (1 to length).map(_ => (Random.nextInt(maxval), default_age)).toList
  def point_crossover(n: Int)(a: BinaryChromosome.BC, b: BinaryChromosome.BC): (BinaryChromosome.BC, BinaryChromosome.BC) = {
    if(n == 0) {
      (a, b)
    }
    else if(n >= 1){
      assert(a.size == b.size)
      assert(b.size == length)
      val k = Random.nextInt(length)
      val (a_, b_) = (a.slice(0, k) ++ b.slice(k, length),
        a.slice(k, length) ++ b.slice(0, k))
      point_crossover(n-1)(a_, b_)
    }
    else ???
  }
  def equal_crossover = ???
  def mutate(a: (Int, Double)): (Int, Double) = {
    (1-a._1, mutated_new_age) //List(0, a._2 - default_age).max)
  }

  final def sums(a: List[Double]): List[Double] =
    a.scanLeft(0.0)(_ + _).tail
    // a match {
    //   case Nil => Nil
    //   case h :: t => h :: (sums(t).map(_ + h))
    // }

  def mutation(a: BinaryChromosome.BC): BinaryChromosome.BC = {
    if(Random.nextInt(5) == 0) return a
    // println("mutation start")
    assert(a.size == length)
    @tailrec
    def go(a: BinaryChromosome.BC)(n: Int): BinaryChromosome.BC =
      if(n > 0) {
        val s = sums(a.map(_._2))
        assert(sums(List(1,2,3,4)) == List(1,3,6,10))
        assert(s.size == a.size)
        // val k_ = Random.nextInt(s.last.toInt).toDouble
        val k_ = Random.nextDouble() * s.last
        import scala.collection.Searching._
        val k = s.search(k_) match {
          case Found(i) => i
          case InsertionPoint(i) => i
        }
        assert(0 <= k && k < a.size)
        // a.map(x => assert(x._2 == 1))
        // println(a.map(_._1))
        // println(sums(a.map(_._1)))
        // println(s.last)
        // println(k)
        // val k = Random.nextInt(a.size)
        go(a.updated(k, mutate(a(k))))(n-1)
        // val g = go(a)(n-1)
        // g.updated(k, mutate(g(k)))
      }
      else a
    val res = go(a)(1).map(x => (x._1, x._2)) // * aging))
    // println("mutation end")
    res
  }

  def distance(a: BinaryChromosome.BC, b: BinaryChromosome.BC): Int =
    a.zip(b).filter(x => x._1 != x._2).size

  def one_distances(a: BinaryChromosome.BC): List[BinaryChromosome.BC] = {
    val b = for{
      i <- (0 until a.size)
    } yield (a.updated(i, mutate(a(i))))
    b.toList
  }
}

object BinaryChromosome {
  type BC = List[(Int, Double)]
}

object BasicSelection{

  def find_parent(a: List[Double]): (Int, Int) = {
    // println("find_parent start")
    val x = a.zipWithIndex.sortWith(_._1 > _._1)
    val res = (x(0 + Random.nextInt(1))._2, x(0 + Random.nextInt(1))._2)
    // println("find_parent end")
    res
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

object main extends App {
  val dir = File(System.getProperty("user.dir"))
  // val matches: Iterator[File] = dir.glob("**/100_5000_pos_2coclique.{in}")
  // val matches: Iterator[File] = dir.glob("**/200_20000_pos_2coclique.{in}")
  // val matches: Iterator[File] = dir.glob("**/1000_10000_pos_2coclique.{in}")
  val matches: Iterator[File] = dir.glob("**/given_500.in")
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
    val BC = new BinaryChromosome(n)
    val ga = new GA[BinaryChromosome.BC](
      List.fill(GA.pool_size)(BC.random_binary),
      ((x, y) => BC.point_crossover(1)(x, y)._1),
      BC.mutation,
      g.valuation,
      BasicSelection.find_parent,
      BasicSelection.selection,
      Map(),
      BC.one_distances
    )
    var ga_ = ga
    (0 until 7000).foreach { _ =>
      ga_ = ga_.next
      // println(ga_.get_best)
    }
    // println(ga_.pool.map(BC.distance(bc, _)).sorted)
    println(s"${n} ${m}")
  }
}
