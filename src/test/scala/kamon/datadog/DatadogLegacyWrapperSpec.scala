package kamon.datadog

import com.typesafe.config.Config
import kamon.{ Kamon, MetricReporter }
import kamon.metric._
import kamon.testkit.MetricInspection
import org.scalatest.{ Matchers, WordSpec }

class DatadogLegacyWrapperSpec extends WordSpec with Matchers with MetricInspection {

  "the DatadogLegacyWrapper" should {
    "apply known mappings to reported metrics" in {
      report(histogram("akka.actor.processing-time", Map("path" -> "user/test/other"))) { result =>
        val histogram = result.histograms.head
        histogram.name shouldBe "kamon.akka-actor.processing-time"
        histogram.tags should contain(
          "akka-actor" -> "user/test/other"
        )
      }

      report(counter("akka.actor.errors", Map("path" -> "user/test/other"))) { result =>
        val counter = result.counters.head
        counter.name shouldBe "kamon.akka-actor.errors"
        counter.tags should contain(
          "akka-actor" -> "user/test/other"
        )
      }

      report(rangeSampler("akka.actor.mailbox-size", Map("path" -> "user/test/other"))) { result =>
        val rangeSampler = result.rangeSamplers.head
        rangeSampler.name shouldBe "kamon.akka-actor.mailbox-size"
        rangeSampler.tags should contain(
          "akka-actor" -> "user/test/other"
        )
      }
    }

    "treat all metrics without mappings as if they were single metric entities" in {
      report(histogram("my-custom-histogram", Map("some" -> "tag"))) { result =>
        val histogram = result.histograms.head
        histogram.name shouldBe "kamon.histogram.my-custom-histogram"
        histogram.tags should contain(
          "some" -> "tag"
        )
      }

      report(counter("my-custom-counter", Map("some" -> "tag"))) { result =>
        val counter = result.counters.head
        counter.name shouldBe "kamon.counter.my-custom-counter"
        counter.tags should contain(
          "some" -> "tag"
        )
      }

      report(rangeSampler("my-custom-range-sampler", Map("some" -> "tag"))) { result =>
        val rangeSampler = result.rangeSamplers.head
        rangeSampler.name shouldBe "kamon.min-max-counter.my-custom-range-sampler"
        rangeSampler.tags should contain(
          "some" -> "tag"
        )
      }

      report(gauge("my-custom-gauge", Map("some" -> "tag"))) { result =>
        val gauge = result.gauges.head
        gauge.name shouldBe "kamon.gauge.my-custom-gauge"
        gauge.tags should contain(
          "some" -> "tag"
        )
      }
    }
  }

  val reporter = new FakeReporter()
  val wrapper = new DatadogLegacyWrapper(reporter)

  class FakeReporter extends MetricReporter {
    override def start(): Unit = {}
    override def stop(): Unit = {}
    override def reconfigure(config: Config): Unit = {}

    var metrics: MetricsSnapshot = _
    override def reportPeriodSnapshot(snapshot: PeriodSnapshot): Unit = {
      metrics = snapshot.metrics
    }
  }

  def report(periodSnapshot: PeriodSnapshot)(assertions: MetricsSnapshot => Unit): Unit = {
    wrapper.reportPeriodSnapshot(periodSnapshot)
    assertions(reporter.metrics)
  }

  val emptyDistribution = Kamon.histogram("test").distribution()
  val emptyPeriodSnapshot = PeriodSnapshot(Kamon.clock().instant(), Kamon.clock().instant(),
    MetricsSnapshot(Seq.empty, Seq.empty, Seq.empty, Seq.empty))

  def counter(metricName: String, tags: Map[String, String]): PeriodSnapshot = {
    emptyPeriodSnapshot.copy(metrics = emptyPeriodSnapshot.metrics
      .copy(counters = Seq(MetricValue(metricName, tags, MeasurementUnit.none, 1))))
  }

  def gauge(metricName: String, tags: Map[String, String]): PeriodSnapshot = {
    emptyPeriodSnapshot.copy(metrics = emptyPeriodSnapshot.metrics
      .copy(gauges = Seq(MetricValue(metricName, tags, MeasurementUnit.none, 1))))
  }

  def histogram(metricName: String, tags: Map[String, String]): PeriodSnapshot = {
    emptyPeriodSnapshot.copy(metrics = emptyPeriodSnapshot.metrics
      .copy(histograms = Seq(MetricDistribution(metricName, tags, MeasurementUnit.none, DynamicRange.Default, emptyDistribution))))
  }

  def rangeSampler(metricName: String, tags: Map[String, String]): PeriodSnapshot = {
    emptyPeriodSnapshot.copy(metrics = emptyPeriodSnapshot.metrics
      .copy(rangeSamplers = Seq(MetricDistribution(metricName, tags, MeasurementUnit.none, DynamicRange.Default, emptyDistribution))))
  }
}
