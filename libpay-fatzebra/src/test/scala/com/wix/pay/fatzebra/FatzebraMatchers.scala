package com.wix.pay.fatzebra

import org.specs2.matcher.{AlwaysMatcher, Matcher, Matchers}

trait FatzebraMatchers extends Matchers {
  def authorizationParser: FatzebraAuthorizationParser

  def beAuthorization(purchaseId: Matcher[String] = AlwaysMatcher()): Matcher[FatzebraAuthorization] = {
    purchaseId ^^ { (_: FatzebraAuthorization).purchaseId aka "purchaseId" }
  }

  def beAuthorizationKey(authorization: Matcher[FatzebraAuthorization]): Matcher[String] = {
    authorization ^^ { authorizationParser.parse(_: String) aka "parsed authorization"}
  }

  def beMerchant(username: Matcher[String] = AlwaysMatcher(),
                 password: Matcher[String] = AlwaysMatcher()): Matcher[FatzebraMerchant] = {
    username ^^ { (_: FatzebraMerchant).username aka "username" } and
      password ^^ { (_: FatzebraMerchant).password aka "password" }
  }
}

object FatzebraMatchers extends FatzebraMatchers {
  override val authorizationParser = new JsonFatzebraAuthorizationParser()
}