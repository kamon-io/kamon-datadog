package kamon.datadog

import java.time.Duration

import play.api.libs.json._

case class DdSpan(
  traceId:  BigInt,
  spanId:   BigInt,
  parentId: Option[BigInt],
  name:     String,
  resource: String,
  service:  String,
  spanType: String,
  start:    Long,
  duration: Duration,
  meta:     Map[String, Any],
  error:    Boolean) {

  implicit val anyValWriter = Writes[Any](a => a match {
    case v: String  => JsString(v)
    case v: Int     => JsNumber(v)
    case v: Long    => JsNumber(v)
    case v: Boolean => JsBoolean(v)
    case v: Any     => Json.toJson(v.toString)
  })

  def toJson(): JsObject = {
    val json = JsObject(Map(
      "trace_id" -> JsNumber(BigDecimal(traceId)),
      "span_id" -> JsNumber(BigDecimal(spanId)),
      "name" -> JsString(name),
      "type" -> JsString(spanType),
      "resource" -> JsString(resource),
      "service" -> JsString(service),
      "start" -> JsNumber(BigDecimal(start)),
      "duration" -> JsNumber(BigDecimal(duration.toNanos)),
      "meta" -> JsObject(meta.map(m => m._1 -> Json.toJson(m._2))),
      "error" -> JsBoolean(error)
    ))
    if (parentId.nonEmpty) {
      json + ("parent_id", JsNumber(BigDecimal(parentId.get)))
    } else {
      json
    }
  }
}
