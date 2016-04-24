package com.wix.pay.fatzebra

trait FatzebraMerchantParser {
  def parse(merchantKey: String): FatzebraMerchant
  def stringify(merchant: FatzebraMerchant): String
}
