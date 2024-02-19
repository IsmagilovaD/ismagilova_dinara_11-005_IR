
import java.io.{BufferedWriter, FileWriter}
import java.nio.file.{Files, Paths}
import scala.collection.mutable.Map
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success, Try}
import org.jsoup.Jsoup
import org.jsoup.nodes.Document

import scala.collection.mutable

object Main {

  // Конфигурационные переменные
  val MainLink: String = "https://www.sciencedaily.com"
  val MainLinkPage: String = "https://www.sciencedaily.com/news/computers_math/quantum_computers"
  val InfoFile: String = "index.txt"

  def main(args: Array[String]): Unit = {
    var indexMap: mutable.Map[Int, String] = Map()
    var infoString: String = ""

    val linksAll = getLinks(s"$MainLinkPage")


    for ((link, index) <- linksAll.zipWithIndex) {
      val htmlText: String = crawl(s"$MainLink$link")
      indexMap(index) = link
      val filename: String = "%03d".format(index)
      infoString += s"$filename.txt $MainLink$link\r\n"
      val resultPath: String = s"Downloads/$filename.txt"
      val dirPath: String = Paths.get(resultPath).getParent.toString
      Try(Files.createDirectories(Paths.get(dirPath))) match {
        case Failure(exception) => println(s"Ошибка создания директории: ${exception.getMessage}")
        case Success(_) =>
          val writer = new BufferedWriter(new FileWriter(resultPath))
          writer.write(htmlText)
          writer.close()
          println(s"$resultPath done")
      }
    }
    Files.write(Paths.get(InfoFile), infoString.getBytes)
  }

  def getLinks(url: String): List[String] = {
    println(url)
    Try(
      Jsoup.connect(url)
        .userAgent("Chrome/4.0.249.0 Safari/532.5")
        .referrer("http://www.google.com")
        .get()) match {
      case Failure(exception) =>
        println(s"Ошибка выполнения GET-запроса [$url]: ${exception.getMessage}")
        List.empty[String]
      case Success(doc) =>
        val listNews = doc.select("div#headlines.tab-pane > ul")
        val links = listNews.select("a").asScala.map(_.attr("href")).filterNot(link => link.contains(".js") || link.contains(".css")).toList
        links
    }
  }

  def crawl(url: String): String = {
    Try(
      Jsoup.connect(url)
        .userAgent("Chrome/4.0.249.0 Safari/532.5")
        .referrer("http://www.google.com")
        .get()
    ) match {
      case Failure(exception) =>
        println(s"Ошибка выполнения GET-запроса: ${exception.getMessage}")
        ""
      case Success(htmlText) => removeTags(htmlText)
    }
  }

  def removeTags(doc: Document): String = {
    doc.select("script, link[rel='stylesheet'], noscript, img, picture, style, link").remove()
    doc.html()
  }
}