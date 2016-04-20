import scala.io.Source
import better.files._, Cmds._
import java.io.{File => JFile}
import scala.annotation.tailrec
import scala.util.Random
// http://stackoverflow.com/questions/15639078/scala-class-constructor-parameters
// If you prefix parameters with val, var they will be visible from outside of class, otherwise, they will be private, as you can see in code above.
class Edge(val from: Int, val to: Int, val weight: Int) {
  override def toString =
    from.toString + " " + to.toString + " " + weight.toString
  // where did this "weight" come from??
  override def equals(o: Any) = o match {
    case (that: Edge) =>
      this.from == that.from && this.to == that.to
      // val r = this.from == that.from && this.to == that.to
      // println(r)
      // r
    case _ => false
  }
  override def hashCode = (from + to).hashCode
}

class Graph(val size: Int, val edges: Set[Edge]) {
  def this(size: Int) = this(size, Set())
  override def toString = edges.foldLeft("")(
    (s: String, x: Edge) => x.toString + "\n" + s)
  def random_node = Random.nextInt(size) + 1
  def spread_edges(m: Int, forbidden: Set[Range]) = {
    // var counter = 0
    // @tailrec
    def stream: Stream[Edge] = {
      // println(counter)
      // counter += 1
      (new Edge(random_node, random_node, Graph.random_weight) #:: stream)
      // (new Edge(random_node, random_node, Graph.random_weight) #:: stream).distinct
    }
    // println("-----------------------------------------------------------")
    // println(stream.take(10000))
    // println("-----------------------------------------------------------")
    // println(stream.take(100).toList)
    // println("-----------------------------------------------------------")
    // println(stream.take(1000).toList)
    // println("-----------------------------------------------------------")
    // println(stream.take(1000).toList)
    // println("-----------------------------------------------------------")

    val bb: Set[Edge] = stream.distinct.take(m).filter(x =>
    forbidden.forall(! _.contains(x))).toSet.take(m)
    // println(bb)
    val e = edges ++ bb
    // println(e)
    //     override def distinct: Stream[A] = {
  //   // This should use max memory proportional to N, whereas
  //   // recursively calling distinct on the tail is N^2.
  //   def loop(seen: Set[A], rest: Stream[A]): Stream[A] = {
  //     if (rest.isEmpty) rest
  //     else if (seen(rest.head)) loop(seen, rest.tail)
  //     else cons(rest.head, loop(seen + rest.head, rest.tail))
  //   }
  //   loop(Set(), this)
  // }

    
      // (1 to m).map(_ =>
      //   new Edge(random_node, random_node, Graph.random_weight))
    new Graph(size, e)
  }

  // implicit override def toString = ""
  // override def abc = ""
  // method abc overrides nothing
}

object Graph {
  def random_weight = Random.nextInt(2000) - 1000
  // import scala.language.implicitConversions
  // implicit def GraphToString(g: Graph): String = ""
}

class GA[A](
  val pool: List[A],
  val crossover: (A, A) => A,
  val mutation: A => A,
  val valuation: A => Float,
  val selection: List[Float] => List[Float]) {
  val progress: GA[A] = {
    val next_pool: List[A] = pool.map(x => (x, valuation(x))).map(x => x._1)
    new GA[A](next_pool, crossover, mutation, valuation, selection)
  }
}

object GA {
  val pool_size = 250
}

class ListIntChromosome(length: Int, maxval: Int) { // extends Chromosome {
  type A = List[Int]
  def random_binary: A = {
    // val t: IndexedSeq[String] = (0 to n).map(_ =>
    //   (scala.util.Random.nextInt % 2).toString)
    // t.foldLeft("")(_ + _)
    (0 to length).map(_ => Random.nextInt(maxval)).toList
  }
  def crossover(a: A, b: A): A = {
    assert(a.size == b.size)
    assert(b.size == length)
    val l = a.size
    val k = Random.nextInt(l)
    a.slice(0, k) ++ b.slice(k, l)
  }
  def mutation(a: A): A = {
    assert(a.size == length)
    val k = Random.nextInt(a.size)
    a.updated(k, Random.nextInt(maxval))
  }
}

object SelectionFunction {
  def basic(a: List[Float]): List[Float] = ???
}

object GARunner extends Application {
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
        new Edge(x(0).toInt, x(1).toInt, x(2).toInt)
      }
    ).toSet
    assert(edge_data.size == m)
    val g = new Graph(n, edge_data)
    val LIC = new ListIntChromosome(n, 2)
    val ga = new GA[List[Int]](
      List.fill(GA.pool_size)(LIC.random_binary),
      LIC.crossover,
      LIC.mutation,
      ???,
      SelectionFunction.basic
    )
    println(s"${n} ${m}")
  }
  // val dir: better.files.File = pwd / cwd
  // val matches: Iterator[File] = dir.glob("**/*.{java,scala}")
}
