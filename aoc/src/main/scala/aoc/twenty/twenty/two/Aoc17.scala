package aoc.twenty.twenty.two

object Aoc17 extends App {
  println("Trick Shot")

  case class State(x: Int, y: Int, xv: Int, yv: Int, max: Int) {

    // target area: x=57..116, y=-198..-148
    private val xr = 57 to 116
    private val yr = -198 to -148

    def inTargetArea: Boolean = {
      xr.contains(x) && yr.contains(y)
    }

    def lost: Boolean = {
      y < yr.min && x > xr.max
    }
  }

  class Test(zero: State) {
    lazy val stream: LazyList[State] = {
      def loop(zero: State): LazyList[State] = zero #:: loop(next(zero))

      loop(zero)
    }
  }

  def next(state: State): State = {
    val yPrime = state.y + state.yv
    val statePrime = state.copy(
      x = state.x + state.xv,
      y = yPrime,
      xv = state.xv match {
        case v if v > 0 => state.xv - 1
        case v if v < 0 => state.xv + 1
        case _ => state.xv
      },
      yv = state.yv - 1,
      max = Math.max(state.max, yPrime)
    )
    statePrime
  }

  // The power of brute force
  val candidates = for {
    ixv <- -1000 to 1000
    iyv <- -198 to 1000
  } yield State(0, 0, ixv, iyv, 0)

  val streams = candidates.map { sts =>
    new Test(sts).stream.takeWhile(!_.lost).find(s => s.inTargetArea)
  }

  val inTargetStreams = streams.filter(_.isDefined).map(_.get).sortBy(_.max)

  println("Search...")
  inTargetStreams.foreach(println)
  println(inTargetStreams.length)

}
