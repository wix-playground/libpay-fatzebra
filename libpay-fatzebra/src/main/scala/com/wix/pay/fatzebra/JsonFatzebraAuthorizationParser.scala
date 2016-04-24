package com.wix.pay.fatzebra

import org.json4s.DefaultFormats
import org.json4s.native.Serialization

class JsonFatzebraAuthorizationParser() extends FatzebraAuthorizationParser {
  private implicit val formats = DefaultFormats

  override def parse(authorizationKey: String): FatzebraAuthorization = {
    Serialization.read[FatzebraAuthorization](authorizationKey)
  }

  override def stringify(authorization: FatzebraAuthorization): String = {
    Serialization.write(authorization)
  }
}
