package aoc.twenty.twenty.two

object Aoc11 extends App {
  println("Dumbo Octopus")

  val my = "8826876714;3127787238;8182852861;4655371483;3864551365;1878253581;8317422437;1517254266;2621124761;3473331514"

  val grid = for {
    row <- my.split(';').zipWithIndex
    levelIdx <- row._1.toCharArray.zipWithIndex
    column = (levelIdx._2, row._2) -> levelIdx._1.toString.toInt
  } yield column

  val gridMap = grid.toMap

  val adjacentOffsets = for {
    x <- -1 to 1
    y <- -1 to 1
  } yield (x, y)

  val adjacent = gridMap.map {
    case ((x, y), _) => ((x, y), adjacentOffsets.map {
      case (ox, oy) => (x + ox, y + oy)
    }.filter(gridMap.contains))
  }

  val zero = Octopus(gridMap, 0, 0, 0)

  lazy val stream = {
    def loop(current: Octopus): LazyList[Octopus] = current #:: loop(next(current))

    loop(zero)
  }

  run(stream.iterator)

  def calc(pos: (Int, Int), levels: Map[(Int, Int), Int]) = {
    adjacent(pos).count(p => levels(p) == 9)
  }

  def nextLevels(ready: Int, levels: Map[(Int, Int), Int])(level: ((Int, Int), Int)) = level match {
    case (_, -1) if ready == 0 => level
    case (pos, 9) => (pos, -1)
    case (pos, l) => (pos, Math.min(l + ready + calc(pos, levels), 9))
  }

  def next(state: Octopus) = Octopus(
    state.levels.map(l => nextLevels(if (state.flashExists) 0 else 1, state.levels)(l)),
    state.numberOfFlashes,
    state.macroFlashes + state.numberOfFlashes,
    if (!state.flashExists) state.steps + 1 else state.steps)

  def run(it: Iterator[Octopus]): Unit = {
    while (it.hasNext) {
      val state = it.next
      if (state.levels.values.forall(_ == 0)) {
        require(788 == state.steps)
        return
      }
    }
  }

  case class Octopus(levels: Map[(Int, Int), Int], microFlashes: Int, macroFlashes: Int, steps: Int) {
    def numberOfFlashes: Int = levels.values.count(_ == 9)

    def flashExists: Boolean = numberOfFlashes != 0
  }
}
