package kamon.datadog

import java.time.Instant

import com.typesafe.config.ConfigFactory
import kamon.Kamon
import kamon.metric.{MeasurementUnit, MetricValue, MetricsSnapshot, PeriodSnapshot}
import okhttp3.mockwebserver.{MockResponse, MockWebServer}
import org.scalatest.{Matchers, WordSpec}

class DatadogAPIReporterSpec extends AbstractHttpReporter with Matchers {



  "the DatadogAPIReporter" should {
    val reporter = new DatadogAPIReporter()
    val now = Instant.ofEpochMilli(1523395554)

    reporter.start()

    "sends counter metrics" in {
      val baseUrl = mockResponse("/test", new MockResponse().setStatus("HTTP/1.1 200 OK"))
      reporter.reconfigure(ConfigFactory.parseString("kamon.datadog.http.api-url = \"" + baseUrl + "\"").withFallback(Kamon.config()))

      reporter.reportPeriodSnapshot(
        PeriodSnapshot.apply(
          now.minusMillis(1000), now, MetricsSnapshot.apply(
            Nil,
            Nil,
            Nil,
            Seq(
              MetricValue.apply("test.counter", Map("tag1" -> "value1"), MeasurementUnit.none, 0)
            )

          )
        )
      )
      val request = server.takeRequest()
      request.getBody().readUtf8() shouldBe """{"series":[{"metric":"test.counter","interval":1,"points":[[1523394,0]],"type":"count","host":"test","tags":["service:kamon-application","env:staging","tag1:value1"]}]}"""
      request.getRequestUrl.toString shouldEqual baseUrl + "?api_key="

    }

    reporter.stop()

  }

}
