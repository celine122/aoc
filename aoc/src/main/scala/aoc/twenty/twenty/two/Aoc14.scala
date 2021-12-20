package aoc.twenty.twenty.two

import scala.language.postfixOps

object Aoc14 extends App {
  println("Extended Polymerization")

  val myTemplate = "PHVCVBFHCVPFKBNHKNBO"
  val myRules = "HK,F;VN,S;NB,F;HF,B;CK,N;VP,B;HO,P;NH,N;CC,N;FC,P;OK,S;OO,P;ON,C;VF,B;NN,O;KS,P;FK,K;HB,V;SH,O;OB,K;PB,V;BO,O;NV,K;CV,H;PH,H;KO,B;BC,B;KC,B;SO,P;CF,V;VS,F;OV,N;NS,K;KV,O;OP,O;HH,C;FB,S;CO,K;SB,K;SN,V;OF,F;BN,F;CP,C;NC,H;VH,S;HV,V;NF,B;SS,K;FO,F;VO,H;KK,C;PF,V;OS,F;OC,H;SK,V;FF,H;PK,N;PC,O;SP,B;CB,B;CH,H;FN,V;SV,O;SC,P;NP,B;BB,S;PV,S;VB,P;SF,H;VC,O;HN,V;BF,O;NO,O;HP,N;VV,K;HS,P;FH,N;KB,F;KF,B;PN,K;KH,K;CN,S;PP,O;BP,O;OH,B;FS,O;BK,B;PO,V;CS,C;BV,N;KP,O;KN,B;VK,F;HC,O;BH,B;FP,H;NK,V;BS,C;FV,F;PS,P"

  type Key = (String, String)

  val rules = for {
    line <- myRules.split(';')
    rule = line.split(',')
  } yield rule(0).map(_.toString).toList -> rule(1)

  val ruleMap = rules.toMap.map {
    case (List(left, right), v) => ((left, right), v)
  }

  val template = myTemplate.map(_.toString).toList
  val pairs = template.reverse.foldLeft(List.empty[Pair])(fold)

  val zeroCounter = template.map(t => (t, 1L)).foldLeft[Map[String, Long]](Map.empty)(update)
  val zeroCounterPairs = pairs.map(p => (p.key, 1L)).foldLeft[Map[Key, Long]](Map.empty)(update)
  val zero = State(pairs.toSet, zeroCounter, zeroCounterPairs)

  lazy val stream = {
    def loop(current: State): LazyList[State] = current #:: loop(next(current))

    loop(zero)
  }

  run(stream.iterator)

  def nextPair(state: State, p: Pair) = {
    ruleMap.get(p.key) match {
      case None => State(state.pairs + p, state.counter, state.counterPairs)
      case Some(symbol) =>
        val numberOfEqualPairs = state.counterPairs(p.key)
        val pairPrime = p.insert(symbol, numberOfEqualPairs)
        State(state.pairs + pairPrime, update(state.counter, (symbol, numberOfEqualPairs)), state.counterPairs)
    }
  }

  def splitPair(state: State, p: Pair) = {
    if (p.hasInsert) {
      val changes = List((p.key, -p.middle._2), ((p.lhs, p.middle._1), p.middle._2), ((p.middle._1, p.rhs), p.middle._2))
      val counterPairsPrime = changes.foldLeft[Map[Key, Long]](state.counterPairs)(update)
      state.copy(pairs = state.pairs ++ p.split, counterPairs = counterPairsPrime)
    } else {
      state.copy(pairs = state.pairs + p)
    }
  }

  def next(state: State) = {
    val pre = state.pairs.foldLeft(state.emptyPairs)(nextPair)
    pre.pairs.foldLeft(pre.emptyPairs)(splitPair)
  }

  def run(it: Iterator[State]): Unit = {
    var step = 0
    while (it.hasNext) {
      val state = it.next
      step = step + 1
      if (step > 40) {
        val sorted = state.counter.toList.sortBy(_._2).reverse
        require(4439442043739L == (sorted.head._2 - sorted.last._2))
        return
      }
    }
  }

  def fold(list: List[Pair], symbol: String) = {
    val lhsPrime = if (list.isEmpty) "X" else list.head.lhs
    newPair(symbol, lhsPrime) :: list
  }

  def newPair(lhs: String, rhs: String) = Pair(lhs, ("X", 0), rhs)

  case class Pair(lhs: String, middle: (String, Long), rhs: String) {
    def key: Key = (lhs, rhs)

    def insert(symbol: String, number: Long): Pair = this.copy(middle = (symbol, number))

    def hasInsert: Boolean = middle._1 != "X"

    def split = Set(newPair(lhs, middle._1), newPair(middle._1, rhs))
  }

  case class State(pairs: Set[Pair], counter: Map[String, Long], counterPairs: Map[Key, Long]) {
    def emptyPairs: State = this.copy(pairs = Set.empty)
  }

  def update[T](map: Map[T, Long], change: (T, Long)) = {
    map get change._1 match {
      case None => map + (change._1 -> change._2)
      case Some(value) => map + (change._1 -> (change._2 + value))
    }
  }
}
