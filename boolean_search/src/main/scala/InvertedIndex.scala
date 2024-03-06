import edu.stanford.nlp.simple.{Document, Token}
import org.jsoup.Jsoup

import scala.collection.convert.ImplicitConversions.`collection AsScalaIterable`
import java.io.PrintWriter
import scala.collection.mutable.ListBuffer
import scala.io.Source

object InvertedIndex {
  private val forbiddenTags = Seq("DT", "CC", "CD", "IN", "TO")

  def main(args: Array[String]): Unit = {
    val texts = downloadTexts()

    val invertedIndex = getInvertedIndex(texts)

    writeInvertedIndexToFiles(invertedIndex)
  }

  def downloadTexts(): Map[Int, String] = {
    val pages = ListBuffer[(Int, String)]()
    for (i <- 0 to 99) {
      val filename: String = "%03d".format(i)
      val source = Source.fromFile(s"src/main/resources/Downloads/$filename.txt", "UTF-8")
      val htmlContent = source.mkString
      source.close()
      val page = (i, extractTextFromHtml(htmlContent))
      pages += page
    }
    pages.toMap
  }

  def extractTextFromHtml(html: String): String = {
    val doc = Jsoup.parse(html)
    doc.text()
  }

  def getInvertedIndex(texts: Map[Int, String]): Map[String, List[Int]] = {
    texts.toList.flatMap { case (pageNumber, text) => getLemmas(pageNumber, text) }
      .groupBy(_._1.toLowerCase)

      .map { case (k, v) => (k, v.map(_._2).distinct) }
  }

  def getLemmas(pageNumber: Int, text: String): List[(String, Int)] = {
    val doc = new Document(text)
    val tokens = doc.sentences().flatMap(_.tokens()).toList

    val unique = tokens.distinctBy(token => token.word().toLowerCase)
    filterTokens(unique).map(token => (token.lemma(), pageNumber))
  }

  def filterTokens(tokens: List[Token]): List[Token] = {
    val digitRegex = """\d""".r
    val punctuationRegex = """[‐+<>/|.,!?;:()"'`-]""".r
    tokens
      .filterNot(token =>
        forbiddenTags.contains(token.tag())
          || token.word().length < 2
          || digitRegex.findFirstIn(token.word()).isDefined
          || punctuationRegex.findFirstIn(token.word()).isDefined
          || token.word().contains("\\")
      )
  }


  def writeInvertedIndexToFiles(invertedIndex: Map[String, List[Int]]) = {
    val writer = new PrintWriter("inverted_index.txt")
    try {
      invertedIndex.foreach { case (lemma, pages) =>
        writer.println(s"$lemma ${pages.mkString(" ")}")
      }
    } finally {
      writer.close()
    }

    val writer2 = new PrintWriter("inverted_index_2.txt")
    try {
      invertedIndex.foreach { case (lemma, pages) =>
        writer2.println(
          s"""{"count":${pages.size},"inverted_array":${pages.mkString("[", ", ", "]")},"word":"$lemma"}"""
        )
      }
    } finally {
      writer2.close()
    }
  }

  def downloadInvertedIndex(): Map[String, List[Int]] = {
    val source = Source.fromFile(s"inverted_index.txt")
    val sourceLines = source.getLines().toList
    source.close()


    sourceLines.map { line =>
      val parts = line.split(" ")
      val lemma = parts.head
      val pages = parts.tail.map(_.toInt).toList
      lemma -> pages
    }.toMap
  }

}
