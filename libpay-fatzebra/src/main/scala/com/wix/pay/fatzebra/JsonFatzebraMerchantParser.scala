package com.wix.pay.fatzebra

import org.json4s.DefaultFormats
import org.json4s.native.Serialization

class JsonFatzebraMerchantParser() extends FatzebraMerchantParser {
  private implicit val formats = DefaultFormats

  override def parse(merchantKey: String): FatzebraMerchant = {
    Serialization.read[FatzebraMerchant](merchantKey)
  }

  override def stringify(merchant: FatzebraMerchant): String = {
    Serialization.write(merchant)
  }
}
