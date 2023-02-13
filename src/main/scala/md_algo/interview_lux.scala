import java.nio.file.{Files, Paths}

import cats.effect.{ExitCode, IO, Resource}
import cats.implicits._

import scala.collection.mutable
import scala.io.Source

case class SensorData(sensorId: String, humidity: Double)

object Main {
  def parseFile(file: String): IO[List[SensorData]] =
    Resource
      .fromAutoCloseable(IO(Source.fromFile(file)))
      .use { source =>
        IO {
          source
            .getLines()
            .drop(1)
            .map { line =>
              val Array(sensorId, humidity) = line.split(",")
              SensorData(sensorId, if (humidity == "NaN") Double.NaN else humidity.toDouble)
            }
            .toList
        }
      }

  def processData(
    processedData: mutable.Map[String, (Double, Double, Double, Int)],
    data: SensorData
  ): mutable.Map[String, (Double, Double, Double, Int)] = {
    if (!data.humidity.isNaN) {
      val (min, avg, max, count) = processedData.getOrElse(data.sensorId, (Double.MaxValue, 0.0, Double.MinValue, 0))
      processedData.update(
        data.sensorId,
        (math.min(min, data.humidity), avg + data.humidity, math.max(max, data.humidity), count + 1)
      )
    }
    processedData
  }

  def calculateStats(
    processedData: mutable.Map[String, (Double, Double, Double, Int)]
  ): List[(String, (Double, Double, Double))] =
    processedData.toList.map { case (sensorId, (min, avg, max, count)) => (sensorId, (min, avg / count, max)) }

  def sortData(data: List[(String, (Double, Double, Double))]): List[(String, (Double, Double, Double))] =
    data.sortBy { case (_, (_, avg, _)) => -avg }

  import scala.jdk.CollectionConverters._
  def run(dir: String): IO[Unit] = {
    for {
      filePaths <- IO {
                     val dirStream = Files.newDirectoryStream(Paths.get(dir), "*.csv")
                     dirStream.iterator().asScala.map(_.toString).toList
                   }
      measurements <- filePaths.traverse(parseFile)
      processedData = measurements.flatten.foldLeft(mutable.Map[String, (Double, Double, Double, Int)]())(processData)
      calculatedData = calculateStats(processedData)
      sortedData = sortData(calculatedData)
      _ <- IO(println(s"Num of processed files: ${filePaths.length}"))
      _ <- IO(println(s"Num of processed measurements: ${measurements.map(_.length).sum}"))
      _ <- IO(println(s"Num of failed measurements: ${processedData.values.map(_._4).sum}"))
      _ <- IO(println("\nSensors with highest avg humidity:\n"))
      _ <- IO(println("sensor-id,min,avg,max"))
      _ <- sortedData.traverse {
             case (sensor, (minHumidity, avgHumidity, maxHumidity)) =>
               IO(println(s"$sensor,$minHumidity,$avgHumidity,$maxHumidity"))
           }
    } yield ()
  }
}
