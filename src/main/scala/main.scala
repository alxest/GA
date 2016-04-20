import scala.io.Source
import better.files._, Cmds._
import java.io.{File => JFile}
import scala.annotation.tailrec
import scala.util.Random
// http://stackoverflow.com/questions/15639078/scala-class-constructor-parameters
// If you prefix parameters with val, var they will be visible from outside of class, otherwise, they will be private, as you can see in code above.

// class Edge(val from: Int, val to: Int, val weight: Int) {
//   override def toString =
//     from.toString + " " + to.toString + " " + weight.toString
//   override def equals(o: Any) = o match {
//     case (that: Edge) =>
//       this.from == that.from && this.to == that.to
//     case _ => false
//   }
//   override def hashCode = (from + to).hashCode
// }

class Graph(val size: Int, val edges: Map[(Int, Int), Int]) {
  def this(size: Int) = this(size, Map())
  override def toString = edges.foldLeft("")(
    (s: String, x) => x._1._1 + " " + x._1._2 + " " + x._2 + "\n" + s)
  def random_node = Random.nextInt(size) + 1
  def spread_edges(m: Int, forbidden: Set[Range]) = {
//     def stream: Stream[Graph.Edge] =
//       (new Graph.Edge((random_node, random_node), Graph.random_weight) #:: stream)
// //      (new Tuple2(new Tuple2(random_node, random_node), Graph.random_weight) #:: stream)

//     // from Stream.scala
//     // This should use max memory proportional to N, whereas
//     // recursively calling distinct on the tail is N^2.
//     val bb: Set[Graph.Edge] = stream.distinct.filter(x =>
//       forbidden.forall(! _.contains(x))).take(m).toSet
//     //distinct does not work well now, because
//     //it compares (from,to,wegiht), not (from,to)
//     val e = edges ++ bb

    var e = edges
    while(e.size < m) {
      e += new Graph.Edge((random_node, random_node), Graph.random_weight)
    }
    new Graph(size, e)
  }
  def valuation(a: BinaryChromosome.BC): Double = {
    println("inside valuation")
    val t = for (
      //to: [a,b]
      //until: [a,b)
      from <- (0 until size) ;
      to <- ((from + 1) until size) ;
      if(a(from) != a(to)) ;
      weight = edges.getOrElse((from+1,to+1), 0)
    ) yield weight
    //BC: [0,n)
    //node: [1,n]
    t.foldLeft(0.0)(_ + _)
  }
}

object Graph {
  type Edge = ((Int, Int), Int)
  def random_weight = Random.nextInt(2000) - 1000
  // import scala.language.implicitConversions
  // implicit def GraphToString(g: Graph): String = ""
}

class GA[A](
  val pool: List[A],
  val crossover: (A, A) => A,
  val mutation: A => A,
  val valuation: A => Double,
  val selection: List[Double] => List[Double]) {
  lazy val next: GA[A] = {
    println("inside next")
    val next_pool: List[A] = pool.map(x => (x, valuation(x))).map(x => x._1)
    println("inside next 2")
    new GA[A](next_pool, crossover, mutation, valuation, selection)
  }
  def progress(n: Int): GA[A] = {
    println(n)
    if(n > 0) this.next.progress(n-1)
    else this
  }
}

object GA {
  val pool_size = 250
}

class BinaryChromosome(length: Int) { // extends Chromosome {
  val maxval = 2
  def random_binary: BinaryChromosome.BC = {
    // val t: IndexedSeq[String] = (0 to n).map(_ =>
    //   (scala.util.Random.nextInt % 2).toString)
    // t.foldLeft("")(_ + _)
    (1 to length).map(_ => Random.nextInt(maxval)).toList
  }
  def crossover(a: BinaryChromosome.BC, b: BinaryChromosome.BC): BinaryChromosome.BC = {
    assert(a.size == b.size)
    assert(b.size == length)
    val l = a.size
    val k = Random.nextInt(l)
    a.slice(0, k) ++ b.slice(k, l)
  }
  def mutation(a: BinaryChromosome.BC): BinaryChromosome.BC = {
    assert(a.size == length)
    val k = Random.nextInt(a.size)
    a.updated(k, Random.nextInt(maxval))
  }
}

object BinaryChromosome {
  type BC = List[Int]
}

object SelectionFunction {
  def basic(a: List[Double]): List[Double] = ???
}

object main extends Application {
  val dir = File(System.getProperty("user.dir"))
  val matches: Iterator[File] = dir.glob("**/*.{in}")
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
      SelectionFunction.basic
    )
    println(ga.progress(15))
    println(s"${n} ${m}")
  }
  // val dir: better.files.File = pwd / cwd
  // val matches: Iterator[File] = dir.glob("**/*.{java,scala}")
}
