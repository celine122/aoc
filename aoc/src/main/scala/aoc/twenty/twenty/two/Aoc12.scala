package aoc.twenty.twenty.two

object Aoc12 extends App {
  println("Passage Pathing")

  val my = "qi-UD;jt-br;wb-TF;VO-aa;UD-aa;br-end;end-HA;qi-br;br-HA;UD-start;TF-qi;br-hf;VO-hf;start-qi;end-aa;hf-HA;hf-UD;aa-hf;TF-hf;VO-start;wb-aa;UD-wb;KX-wb;qi-VO;br-TF"

  val rules = for {
    line <- my.split(';')
    rule = line.split('-')
  } yield Transition(rule(0), rule(1))

  val ruleSet = rules.toSet

  val map = (ruleSet ++ ruleSet.map(flip)).groupBy(_.from)

  val ps = walk(map, Nil)("start")

  require(ps.size == 116692, ps.size)

  def walk(nextMap: Map[String, Set[Transition]], rest: List[String])(node: String): Set[List[String]] = {
    val path = node :: rest
    if (node == "end") {
      Set(path)
    } else {
      val visitedSmall = path.toSet.filter(small)
      val visitedSmallTwice = visitedSmall.exists(v => path.count(_ == v) > 1)
      val skip = if (visitedSmallTwice) visitedSmall else Set("start")
      val next = nextMap(node).map(_.to) -- skip
      next.flatMap(walk(nextMap, path))
    }
  }

  def small(sym: String) = sym.forall(_.isLower)

  def flip(t: Transition) = Transition(t.to, t.from)

  case class Transition(from: String, to: String)
}
