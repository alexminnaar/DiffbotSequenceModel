import java.util.regex.Pattern

import org.scalatest.{Matchers, WordSpec}

class PriceFeatureSpec extends WordSpec with Matchers {

  "Price feature" should {

    "find the correct prices" in {


      val isPrice1 = "5.00"
      val isPrice2 = "$72.99"
      val isPrice3 = "52"

      val notPrice1 = "7.222"
      val notPrice2 = "63.9"
      val notPrice3 = "849483938573"

      val result = Pattern.matches("(USD|EUR|€|\\$)\\s?(\\d{1,3}(?:[.,]\\d{3})*(?:[.,]\\d{2}))|(\\d{1,3}(?:[.,]\\d{3})*(?:[.,]\\d{2})?)\\s?(USD|EUR|€|\\$)",isPrice3)
      println(result)




    }


  }


}
