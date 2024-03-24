import edu.stanford.nlp.simple._
import org.jsoup.Jsoup

import java.io.{File, PrintWriter}
import java.nio.file.{Files, Paths}
import scala.collection.convert.ImplicitConversions.`collection AsScalaIterable`
import scala.io.Source
import scala.math.log


object TFIDF {
  private val forbiddenTags = Seq("DT", "CC", "CD", "IN", "TO")

  def main(args: Array[String]): Unit = {
    val htmls = downloadHtmls()
    val texts = extractTextFromHtml(htmls)

    val (idfTokens, tfIdfTokens) = countTfIdf("tokens", texts)
    writingFiles(idfTokens, tfIdfTokens, "tokens")

    val (idfLemmas, tfIdfLemmas) = countTfIdf("lemmas", texts)
    writingFiles(idfLemmas, tfIdfLemmas, "lemmas")
  }


  def downloadHtmls(): List[String] = {
    val htmlFiles = (0 to 99).toList.map { i =>
      val filename: String = "%03d".format(i)
      val file = Source.fromFile(s"src/main/resources/Downloads/$filename.txt", "UTF-8")
      val htmlContent = try file.mkString finally file.close()
      htmlContent
    }
    htmlFiles
  }

  def extractTextFromHtml(htmls: List[String]): List[String] = {
    htmls.map { html =>
      val doc = Jsoup.parse(html)
      doc.text()
    }
  }

  def countTfIdf(`type`: String, texts: List[String]): (Map[String, Double], List[Map[String, Double]]) = {
    var allWords = List[String]()
    var wordsInDocuments = List[List[String]]()
    var tf = List[Map[String, Double]]()

    for (text <- texts) {
      val words =
        `type` match {
          case "lemmas" => getTokens(text).map(_.lemma().toLowerCase)
          case "tokens" => getTokens(text).map(_.word().toLowerCase)
        }
      allWords = allWords ::: words
      tf = tf :+ countTF(words)
      wordsInDocuments = wordsInDocuments :+ words
    }
    val idf = countIDF(allWords, wordsInDocuments)
    val tfIdf = tf.map(countTFIDF(_, idf))

    (idf, tfIdf)
  }

  def getTokens(text: String): List[Token] = {
    val doc = new Document(text)
    val sentences = doc.sentences()
    val tokens = sentences.flatMap(_.tokens()).toList

    filterTokens(tokens)
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

  def countTF(tokens: List[String]): Map[String, Double] = {
    val totalTokens = tokens.length.toDouble
    tokens.groupBy(identity).map {
      case (token, tokens) => (token, tokens.length / totalTokens)
    }
  }

  def countIDF(tokens: List[String], tokensInDocuments: List[List[String]]): Map[String, Double] = {
    tokens.distinct.map { token =>
      val countDocWithToken = tokensInDocuments.count(_.contains(token))
      token -> log(100.toDouble / countDocWithToken)
    }.toMap
  }

  def countTFIDF(tf: Map[String, Double], idf: Map[String, Double]): Map[String, Double] = {
    tf.map { case (term, tfValue) =>
      term -> tfValue * idf.getOrElse(term, 0.0)
    }
  }

  def writingFiles(idfWords: Map[String, Double], tfIdf: List[Map[String, Double]],  kind: String): Unit = {
    val directory = s"tf_idf_$kind"
    Files.createDirectories( Paths.get(directory))
    tfIdf.zipWithIndex.foreach { case (tfIDFDict, i) =>
      val fileName = f"/${directory}_$i%03d.txt"
      val pathResult = s"$directory$fileName"
      val writer = new PrintWriter(new File(pathResult))
      try {
        tfIDFDict.foreach { case (word, tfidf) =>
          val idf = idfWords.getOrElse(word, 0.0)
          writer.write(s"$word $idf $tfidf\n")
        }
      } finally {
        writer.close()
      }
    }
  }
}