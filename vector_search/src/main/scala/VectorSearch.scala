import edu.stanford.nlp.simple.{Document, Token}
import org.apache.lucene.analysis.CharArraySet
import org.apache.lucene.analysis.en.EnglishAnalyzer

import java.nio.file.{Files, Paths}
import scala.collection.convert.ImplicitConversions.`collection AsScalaIterable`
import scala.collection.mutable
import scala.io.{Source, StdIn}
import scala.math.sqrt

object VectorSearch {
  val lemmas = loadLemmasIDF()

  val TFIDFMatrix = loadTFIDFMatrix()

  def main(args: Array[String]): Unit = {
    val query = StdIn.readLine()
    val results = search(query)
    results.foreach(println)
  }

  def search(query: String): List[(String, Double)] = {
    val queryVector = QueryProcessor.getQueryVector(query)

    getSimilarities(queryVector)
  }

  def calculateCosineSimilarity(queryVector: Map[String, Double], pageVector: Map[String, Double]): Double = {
    val dotProduct = queryVector.map { case (term, qCount) => qCount * pageVector.getOrElse(term, 0.0) }.sum
    val queryNorm = sqrt(queryVector.values.map(x => x * x).sum)
    val pageNorm = sqrt(pageVector.values.map(x => x * x).sum)
    if (queryNorm > 0 && pageNorm > 0)
      dotProduct / (queryNorm * pageNorm)
    else
      0.0
  }

  def getSimilarities(queryVector: Map[String, Double]): List[(String, Double)] = {
    TFIDFMatrix.toList.flatMap { case (page, lemmaTfIdf) =>
      val similarity = calculateCosineSimilarity(queryVector, lemmaTfIdf)
      if (similarity > 0.0) Some(page -> similarity) else None
    }.sortBy(-_._2)
  }

  def loadLemmasIDF(): Map[String, Double] = {
    val source = Source.fromFile("src/main/resources/lemmas_idf.txt")
    val lemmas = source.getLines.map { line =>
      val words = line.split(" ")
      (words(0), words(1).toDouble)
    }.toMap
    source.close()
    lemmas
  }

  def loadTFIDFMatrix(): Map[String, Map[String, Double]] = {
    val matrix = mutable.HashMap[String, Map[String, Double]]()
    val dir = Paths.get("src/main/resources/tf_idf_lemmas")
    Files.walk(dir).forEach { filePath =>
      if (!Files.isDirectory(filePath)) {
        val pageNumber = filePath.getFileName.toString.split("_").last
        val source = Source.fromFile(filePath.toString)
        val tfIdfMap = source.getLines
          .map(_.split("\\s+"))
          .map { case Array(lemma, _, tfIdf) => lemma -> tfIdf.toDouble }
          .toMap
        source.close()
        matrix += (pageNumber -> lemmas.map(lemma => lemma._1 -> tfIdfMap.getOrElse(lemma._1, 0.0)).toMap)
      }
    }
    matrix.toMap
  }
}
