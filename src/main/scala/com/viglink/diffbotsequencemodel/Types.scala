package com.viglink.diffbotsequencemodel


case class Tag(tokens: Vector[Array[String]])

case class Prediction(tokenPredictions: List[(String, String)], confidence: Double)

case class ParseResult(title: Option[Prediction], price: Option[Prediction], image: Option[Prediction])