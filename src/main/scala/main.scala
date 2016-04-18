import scala.io.Source

class graph {
  val size: Int
  val edges: Set[(Int, Int, Int)]
}

object main extends Application {
  val n_cand : Set[Int] = Set(100, 500, 1000)
  val m_cand : Set[Int] = Set(0, 10, 100, 1000, 10000)
  for(i <- n_cand ;
    j <- m_cand
  ) {
  }
}
