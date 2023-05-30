package com.github.ryanjheld.weatherservice

import cats.effect.Async
import com.comcast.ip4s._
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits._
import org.http4s.server.middleware.Logger
import com.typesafe.config.ConfigFactory

object WeatherServiceServer {

  def run[F[_]: Async]: F[Nothing] = {
    val config = ConfigFactory.load()
    val apiKey = config.getString("OpenWeather.key")
    for {
      client <- EmberClientBuilder.default[F].build
      weatherAlg = Weather.impl(client)(implicitly, apiKey = apiKey)

      // Combine Service Routes into an HttpApp.
      // Can also be done via a Router if you
      // want to extract segments not checked
      // in the underlying routes.
      httpApp = (
        WeatherServiceRoutes.weatherRoutes[F](weatherAlg)
      ).orNotFound

      // With Middlewares in place
      finalHttpApp = Logger.httpApp(true, true)(httpApp)

      _ <- 
        EmberServerBuilder.default[F]
          .withHost(ipv4"0.0.0.0")
          .withPort(port"8080")
          .withHttpApp(finalHttpApp)
          .build
    } yield ()
  }.useForever
}
