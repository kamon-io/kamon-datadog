package kamon
package datadog

import com.typesafe.config.Config
import kamon.datadog.DatadogLegacyWrapper.Mapping
import kamon.metric.{ MetricDistribution, MetricValue, PeriodSnapshot }

class DatadogLegacyWrapper(targetReporter: MetricReporter) extends MetricReporter {
  private var applicationName: String = "kamon"
  private var mappings: Map[String, Mapping] = Map.empty
  updateConfig(Kamon.config())

  override def start(): Unit = targetReporter.start()
  override def stop(): Unit = targetReporter.stop()
  override def reconfigure(config: Config): Unit = {
    updateConfig(config)
    targetReporter.reconfigure(config)
  }

  override def reportPeriodSnapshot(snapshot: PeriodSnapshot): Unit = {
    val translatedMetrics = snapshot.metrics.copy(
      histograms = snapshot.metrics.histograms.map(translateDistribution(_, "histogram")),
      rangeSamplers = snapshot.metrics.rangeSamplers.map(translateDistribution(_, "min-max-counter")),
      gauges = snapshot.metrics.gauges.map(translateValue(_, "gauge")),
      counters = snapshot.metrics.counters.map(translateValue(_, "counter"))
    )

    targetReporter.reportPeriodSnapshot(snapshot.copy(metrics = translatedMetrics))
  }

  private def translateValue(metricValue: MetricValue, instrumentType: String): MetricValue = {
    val mapping = mappings.getOrElse(metricValue.name, Mapping(instrumentType, metricValue.name, None))

    metricValue.copy(
      name = legacyMetricName(mapping),
      tags = legacyTags(mapping, metricValue.tags)
    )
  }

  private def translateDistribution(metricDistribution: MetricDistribution, instrumentType: String): MetricDistribution = {
    val mapping = mappings.getOrElse(metricDistribution.name, Mapping(instrumentType, metricDistribution.name, None))

    metricDistribution.copy(
      name = legacyMetricName(mapping),
      tags = legacyTags(mapping, metricDistribution.tags)
    )
  }

  private def legacyMetricName(mapping: Mapping): String =
    applicationName + "." + mapping.legacyCategory + "." + mapping.legacyMetricName

  private def legacyTags(mapping: Mapping, tags: Map[String, String]): Map[String, String] =
    mapping.nameTag
      .flatMap(nameTag => tags.get(nameTag).map(name => tags.updated(mapping.legacyCategory, name)))
      .getOrElse(tags)

  private def updateConfig(config: Config): Unit = {
    val legacyConfig = config.getConfig("kamon.datadog.legacy")
    val mappingsConfig = legacyConfig.getConfig("mappings")

    val mappingKeys = mappingsConfig.configurations.map {
      case (key, config) =>
        (key, Mapping(
          legacyCategory = config.getString("category"),
          legacyMetricName = config.getString("metric-name"),
          nameTag = Some(config.getString("name-tag"))
        ))
    }

    applicationName = config.getString("kamon.datadog.application-name")
    mappings = mappingKeys
  }

}

object DatadogLegacyWrapper {
  case class Mapping(
    legacyCategory:   String,
    legacyMetricName: String,
    nameTag:          Option[String]
  )
}

