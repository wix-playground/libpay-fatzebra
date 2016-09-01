package com.wix.pay.fatzebra


import com.wix.pay.fatzebra.model.{Purchase, Response}
import org.specs2.mutable.SpecWithJUnit
import org.specs2.specification.Scope


class PurchaseResponseParserTest extends SpecWithJUnit {
  trait Ctx extends Scope {}

  "stringify and then parse" should {
    "yield an object similar to the original one" in new Ctx {
      val someObj = new Response[Purchase](Some(Purchase(
          authorization = Some("some authorization")
      )))

      val str = PurchaseResponseParser.stringify(someObj)
      PurchaseResponseParser.parse(str) must beEqualTo(someObj)
    }
  }
}
