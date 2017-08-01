package com.viglink.diffbotsequencemodel


case class Tag(tokens: Vector[Array[String]], index:Int)

case class Prediction(tokenPredictions: List[(String, String)],
                      predictionAsString: Vector[String],
                      confidence: Double,
                      tagIndex:Int)

case class ParseResult(title: Option[Prediction],
                       price: Option[Prediction],
                       image: Option[Prediction],
                       sku: Option[Prediction],
                       availability: Option[Prediction],
                       canonicalUrl: Option[Prediction],
                       mpn: Option[Prediction])