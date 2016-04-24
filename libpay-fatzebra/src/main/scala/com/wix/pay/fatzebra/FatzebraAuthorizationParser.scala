package com.wix.pay.fatzebra

trait FatzebraAuthorizationParser {
  def parse(authorizationKey: String): FatzebraAuthorization
  def stringify(authorization: FatzebraAuthorization): String
}
