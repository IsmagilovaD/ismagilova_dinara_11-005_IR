import edu.stanford.nlp.simple._
import org.jsoup.Jsoup

import java.io.PrintWriter
import scala.collection.convert.ImplicitConversions.`collection AsScalaIterable`
import scala.collection.mutable.ListBuffer
import scala.io.Source


object TokenizerAndLemmatizer {
  private val forbiddenTags = Seq("DT", "CC", "CD", "IN", "TO")

  def main(args: Array[String]): Unit = {
    val htmls = downloadHtmls()
    val texts = extractTextFromHtml(htmls)

    val tokens = getTokens(texts)
    val lemmas = getLemmas(tokens)

    writeTokensToFile(tokens.map(_.word()))
    writeLemmasToFile(lemmas)
  }

  def downloadHtmls(): List[String] = {
    val htmlFiles = ListBuffer[String]()
    for (i <- 0 to 99) {
      val filename: String = "%03d".format(i)
      val source = Source.fromFile(s"src/main/resources/Downloads/$filename.txt", "UTF-8")
      val htmlContent = source.mkString
      source.close()
      htmlFiles += htmlContent
    }
    htmlFiles.toList
  }

  def extractTextFromHtml(htmls: List[String]): List[String] = {
    htmls.map { html =>
      val doc = Jsoup.parse(html)
      doc.text()
    }
  }

  def getTokens(texts: List[String]): List[Token] = {
    val tokens = for {
      text <- texts
      doc = new Document(text)
      sentence <- doc.sentences()
      tokens <- sentence.tokens()
    } yield tokens

    val unique = tokens.distinctBy(token => token.word().toLowerCase)
    filterTokens(unique)
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

  def getLemmas(tokens: List[Token]): Map[String, List[String]] = {
    tokens.groupBy(token => token.lemma())
      .map { case (k, v) => (k, v.map(_.word())) }
  }

  def writeTokensToFile(tokens: List[String]) = {
    val writer = new PrintWriter("tokens.txt")
    try {
      tokens.foreach(writer.println)
    } finally {
      writer.close()
    }
  }

  def writeLemmasToFile(lemmas: Map[String, List[String]]) = {
    val writer = new PrintWriter("lemmas.txt")
    try {
      lemmas.foreach { case (k, v) => writer.println(s"$k: ${v.mkString(" ")}") }
    } finally {
      writer.close()
    }
  }
}