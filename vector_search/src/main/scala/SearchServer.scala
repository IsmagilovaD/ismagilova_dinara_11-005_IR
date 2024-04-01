import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import spray.json.{DefaultJsonProtocol, RootJsonFormat}

import scala.concurrent.Future
import scala.util.{Failure, Success}

object SearchServer extends App with JsonSupport {

  implicit val system = ActorSystem("vector-search")
  implicit val executionContext = system.dispatcher

  // Функция для выполнения векторного поиска
  def performVectorSearch(query: String): Future[SearchResult] = {
    val res = VectorSearch.searchTopPages(query)
      .map{ case (pageNumber, pageUrl) => Page(pageNumber, pageUrl)}
    Future.successful(SearchResult(res))
  }

  // Обработчик запросов для поиска
  val searchRoute =
    path("search") {
      get {
        parameters("query") { query =>
          // Выполняем векторный поиск для полученного запроса
          val searchResultFuture = performVectorSearch(query)
          // Отправляем результат обратно пользователю
          onComplete(searchResultFuture) {
            case Success(result) => complete(result)
            case Failure(ex) => complete(StatusCodes.InternalServerError, s"An error occurred: ${ex.getMessage}")
          }
        }
      }
    }

  // Функция для обработки статических файлов
  def htmlPage(): Route = {
    pathEndOrSingleSlash {
      getFromResource("search.html")
    }
  }

  // Комбинированный маршрут
  val routes = searchRoute ~ htmlPage()

  // Запускаем HTTP-сервер
  val bindingFuture = Http().newServerAt("localhost", 8080).bind(routes)

  bindingFuture.onComplete {
    case Success(binding) =>
      println(s"Сервер запущен на http://localhost:8080/")
    case Failure(ex) =>
      println(s"Ошибка при запуске сервера: ${ex.getMessage}")
      system.terminate()
  }

  // Дожидаемся завершения приложения
  scala.io.StdIn.readLine()
  bindingFuture
    .flatMap(_.unbind())
    .onComplete(_ => system.terminate())
}

// Класс для хранения результатов поиска
case class SearchResult(pages: List[Page])
case class Page(pageNumber: String, pageUrl: String)

// Json формат для преобразования SearchResult в JSON и обратно
trait JsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val pageFormat: RootJsonFormat[Page] = jsonFormat2(Page)
  implicit val searchResultFormat: RootJsonFormat[SearchResult] = jsonFormat1(SearchResult)
}