import scala.io.Source
import better.files._, Cmds._
import java.io.{File => JFile}
import scala.annotation.tailrec
import scala.util.Random

object DataGenerator extends App {
  val n_cand : Set[Int] = Set(100, 200, 500, 1000) //, 2500)
  val m_cand : Set[Int] = Set(0, 1000, 5000, 10000, 20000) //, 25000)
  val w_cand : Set[(String, (Int, Int))] = Set(
    ("both", (-100, 100)),
    ("pos", (100, 200)),
    ("neg", (-200, -100)))

  for{
    n <- n_cand ;
    m <- m_cand ;
    w <- w_cand
  } {
    if(n*n >= m) {
      println(s"${n} ${m}")
      def write_graph(file_name: String, g: Graph) = {
        val f = File(file_name)
        // f.delete(true)
        f < ""
        f << s"${n} ${m}"
        f << g.toString
      }

      write_graph(s"${n}_${m}_${w._1}_normal.in",
        new Graph(n).spread_edges(m, w._2, Set()))

      val x: Int = (n/2).toInt
      val y: Int = x*2
      // println(n + " " + x + " " + m)
      // println(n*n - x*x - x*x >= m)
      if(n*n - x*x - x*x >= m) {
        write_graph(s"${n}_${m}_${w._1}_2coclique.in", {
          new Graph(n).spread_edges(m, w._2, Set((0 until x), (x until y)))
        })
      }
      // write_graph(s"${n}_${m}_clique.in", new Graph(n).spread_edges(m, Set()))
      // write_graph(s"${n}_${m}_2clique.in", new Graph(n).spread_edges(m, Set()))
      // write_graph(s"${n}_${m}_coclique.in", new Graph(n).spread_edges(m, Set()))
      // write_graph(s"${n}_${m}_2coclique.in", new Graph(n).spread_edges(m, Set()))
      // val f_clique1 = File(s"${n}_${m}_clique1.in")
      // f < ""
      // f << s"${n}"
      // f << s"${m}"
      // val g = new Graph(n, Set()).spread_edges(m, Set())
      // f << g.toString

    }
  }
}

