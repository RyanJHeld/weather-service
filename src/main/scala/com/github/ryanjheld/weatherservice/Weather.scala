package com.github.ryanjheld.weatherservice

import cats.effect.Concurrent
import cats.implicits._
import io.circe.{Encoder, Decoder, HCursor}
import io.circe.generic.semiauto._
import org.http4s._
import org.http4s.client.Client
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.circe._
import org.http4s.Method._

trait Weather[F[_]] {
  def get(lat: Double, lon: Double): F[Weather.WeatherReport]
}

object Weather {
  def apply[F[_]](implicit ev: Weather[F]): Weather[F] = ev

  final case class WeatherReport(report: String) extends AnyVal

  object WeatherReport {
    def fromWeatherResponse(weatherResponse: WeatherResponse): WeatherReport = {
      val report =
        getTemperatureReport(weatherResponse.temp) + "\n" +
          getConditionReport(weatherResponse.conditions) + "\n" +
          getAlertReport(weatherResponse.alerts)
      WeatherReport(report)
    }

    private def getTemperatureReport(temp: Double): String = {
      val qualitativeTemp = if (temp < -459.67) "impossibly cold"
      else if (temp < 32) "literally freezing"
      else if (temp < 60) "cold"
      else if (temp < 80) "moderate"
      else if (temp < 212) "hot"
      else "literally boiling"
      s"Temperature: $qualitativeTemp"
    }

    private def getConditionReport(conditions: List[WeatherCondition]): String = {
      if (conditions.isEmpty) ""
      else s"Weather Conditions: ${conditions.map(_.description).mkString(", ")}."
    }

    private def getAlertReport(alerts: List[Alert]): String = {
      val alertToString: Alert => String = alert => s"${alert.eventName}: ${alert.description}"
      val alertsString = if (alerts.isEmpty) "No active weather alerts."
      else "\n" + alerts.map(alertToString).mkString("\n")
      s"Weather alerts: $alertsString"
    }

    implicit val weatherEncoder: Encoder[WeatherReport] = deriveEncoder[WeatherReport]

    implicit def weatherEntityEncoder[F[_]]: EntityEncoder[F, WeatherReport] =
      jsonEncoderOf
  }

  case class WeatherResponse(
    temp: Double,
    conditions: List[WeatherCondition],
    alerts: List[Alert]
  )

  object WeatherResponse {
    implicit val weatherDecoder: Decoder[WeatherResponse] = new Decoder[WeatherResponse] {
      final def apply(c: HCursor): Decoder.Result[WeatherResponse] =
        for {
          temp <- c.downField("current").downField("temp").as[Double]
          conditions <- c.downField("current").downField("weather").as[List[WeatherCondition]]
          alerts <- c.downField("alerts").as[Option[List[Alert]]]
        } yield WeatherResponse(temp, conditions, alerts.getOrElse(List.empty[Alert]))
    }

    implicit def weatherEntityDecoder[F[_] : Concurrent]: EntityDecoder[F, WeatherResponse] =
      jsonOf
  }

  case class WeatherCondition(
    name: String,
    description: String
  )

  object WeatherCondition {
    implicit val conditionDecoder: Decoder[WeatherCondition] = new Decoder[WeatherCondition] {
      final def apply(c: HCursor): Decoder.Result[WeatherCondition] =
        for {
          name <- c.downField("main").as[String]
          description <- c.downField("description").as[String]
        } yield WeatherCondition(name, description)
    }
  }

  case class Alert(
    eventName: String,
    description: String
  )

  object Alert {
    implicit val alertDecoder: Decoder[Alert] = new Decoder[Alert] {
      final def apply(c: HCursor): Decoder.Result[Alert] =
        for {
          name <- c.downField("event").as[String]
          description <- c.downField("description").as[String]
        } yield Alert(name, description)

    }
  }

  final case class WeatherError(e: Throwable) extends RuntimeException

  def impl[F[_] : Concurrent](C: Client[F])(implicit apiKey: String): Weather[F] = new Weather[F] {
    val dsl = new Http4sClientDsl[F] {}
    val M: Concurrent[F] = implicitly

    import dsl._

    def get(lat: Double, lon: Double): F[Weather.WeatherReport] = {
      val uri = s"https://api.openweathermap.org/data/2.5/onecall?lat=$lat&lon=$lon&exclude=minutely,hourly,daily&units=imperial&appid=$apiKey"

      M.fromEither(Uri.fromString(uri)).flatMap(uri =>
        C.expect[WeatherResponse](GET(uri))
          .map(WeatherReport.fromWeatherResponse(_))
          .adaptError { case t => WeatherError(t) }
      )
    }
  }
}
