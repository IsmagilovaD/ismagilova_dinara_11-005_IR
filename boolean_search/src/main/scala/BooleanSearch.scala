import InvertedIndex.downloadInvertedIndex

import scala.collection.mutable
import scala.io.StdIn
import scala.util.matching.Regex

object BooleanSearch {
  val getToken: Regex = raw"\s*([A-Za-z01]+|[(|)])\s*(.*)\s*".r
  val pages: List[Int] = (0 until 99).toList


  def main(args: Array[String]): Unit = {
    val invertedIndex = downloadInvertedIndex()

    val query = StdIn.readLine()

    val res = booleanSearch(query, invertedIndex)

    println(res)
  }


  def booleanSearch(query: String, invertedIndex: Map[String, List[Int]]): List[Int] = {
    val expression = toRPN(query)

    val stack: mutable.Stack[List[Int]] = mutable.Stack()

    for (token <- expression) {
      token match {
        case "AND" =>
          val list1 = stack.pop()
          val list2 = stack.pop()
          stack.push(and(list1, list2))
        case "OR" =>
          val list1 = stack.pop()
          val list2 = stack.pop()
          stack.push(or(list1, list2))
        case "NOT" =>
          val list1 = stack.pop()
          stack.push(not(list1))
        case _ => stack.push(invertedIndex.getOrElse(token, List.empty[Int]))
      }
    }
    stack.pop()
  }

  def and(list1: List[Int], list2: List[Int]): List[Int] = {
    list1.intersect(list2)
  }

  def or(list1: List[Int], list2: List[Int]): List[Int] = {
    (list1 ++ list2).distinct
  }

  def not(list: List[Int]): List[Int] = {
    pages.filterNot(list.contains)
  }

  def toRPN(s: String, ops: Seq[String] = Seq()): Seq[String] = s match {
    case "" => ops
    case getToken("(", rest) => //start of paren expression
      val spltAt = rest.iterator.scanLeft(1) { case (lvl, c) =>
        if (c == '(') lvl + 1 else if (c == ')') lvl - 1 else lvl
      }.drop(1).indexOf(0)
      val (paren, str) = rest.splitAt(spltAt)
      toRPN(paren) ++ toRPN(str.tail, ops)

    case getToken(tok@("NOT"), rest) => //higher precedence op
      toRPN(rest, tok +: ops)

    case getToken(tok@("AND" | "OR"), rest) => //operators
      ops.headOption.fold(toRPN(rest, tok +: ops)) {
        case "AND" | "OR" => toRPN(rest, tok +: ops)
        case _ => ops.head +: toRPN(rest, tok +: ops.tail)
      }


    case getToken(num, rest) => num +: toRPN(rest, ops) //boolean values
    case _ => throw new Error(s"can't parse: $s")
  }

}
