package com.github.ryanjheld.weatherservice

import cats.effect.Sync
import cats.implicits._
import org.http4s.dsl.io._
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl

object WeatherServiceRoutes {

  object LatitudeQueryParamMatcher extends QueryParamDecoderMatcher[Double]("lat")
  object LongitudeQueryParamMatcher extends QueryParamDecoderMatcher[Double]("lon")

  def weatherRoutes[F[_] : Sync](W: Weather[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F] {}
    import dsl._
    HttpRoutes.of[F] {
      case GET -> Root / "weather" :? LatitudeQueryParamMatcher(lat) +& LongitudeQueryParamMatcher(lon) =>
        for {
          report <- W.get(lat, lon)
          resp <- Ok(report.report)
        } yield resp
    }
  }

}
