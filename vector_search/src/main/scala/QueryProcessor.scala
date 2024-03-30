import VectorSearch.lemmas
import edu.stanford.nlp.simple.{Document, Token}
import org.apache.lucene.analysis.en.EnglishAnalyzer

import scala.collection.convert.ImplicitConversions.`collection AsScalaIterable`

object QueryProcessor {
  private val forbiddenTags = Seq("DT", "CC", "CD", "IN", "TO")
  private val stopwords = {
    val analyzer = new EnglishAnalyzer()
    analyzer.getStopwordSet
  }

  def getQueryVector(query: String): Map[String, Double] = {
    val queryLemmas = getLemmas(query)
    val queryLemmasTF = countTF(queryLemmas)
    queryLemmasTF.map { case (lemma, tf) =>
      (lemma,
        tf * lemmas.getOrElse(lemma, 0.0)
      )
    }
  }

  def countTF(tokens: List[String]): Map[String, Double] = {
    val totalTokens = tokens.length.toDouble
    tokens.groupBy(identity).map {
      case (token, tokens) => (token, tokens.length / totalTokens)
    }
  }

  def getLemmas(query: String): List[String] = {
    val doc = new Document(query)
    val tokens = doc.sentences().flatMap(_.tokens()).toList

    filterTokens(tokens).map(token => token.lemma().toLowerCase)
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
          || stopwords.contains(token.word())
      )
  }

}
