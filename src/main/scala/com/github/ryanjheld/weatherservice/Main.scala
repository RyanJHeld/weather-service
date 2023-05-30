package com.github.ryanjheld.weatherservice

import cats.effect.{IO, IOApp}

object Main extends IOApp.Simple {
  val run = WeatherServiceServer.run[IO]
}
