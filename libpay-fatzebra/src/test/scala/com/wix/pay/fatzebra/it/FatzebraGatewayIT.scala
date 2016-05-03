package com.wix.pay.fatzebra.it


import com.google.api.client.http.javanet.NetHttpTransport
import com.wix.pay.creditcard.{CreditCard, CreditCardOptionalFields, YearMonth}
import com.wix.pay.fatzebra.FatzebraMatchers._
import com.wix.pay.fatzebra._
import com.wix.pay.fatzebra.model.{Purchase, Response}
import com.wix.pay.fatzebra.testkit.FatzebraDriver
import com.wix.pay.model.{CurrencyAmount, Customer, Deal}
import com.wix.pay.{PaymentErrorException, PaymentGateway, PaymentRejectedException}
import org.specs2.mutable.SpecWithJUnit
import org.specs2.specification.Scope
import spray.http.StatusCodes


class FatzebraGatewayIT extends SpecWithJUnit {
  val fatzebraPort = 9036

  val driver = new FatzebraDriver(port = fatzebraPort)


  step {
    driver.startProbe()
  }

  sequential


  trait Ctx extends Scope {
    val merchantParser = new JsonFatzebraMerchantParser()
    val authorizationParser = new JsonFatzebraAuthorizationParser()

    val someMerchant = new FatzebraMerchant("someUsername", "somePassword")
    val merchantKey = merchantParser.stringify(someMerchant)

    val fatzebra: PaymentGateway = new FatzebraGateway(
      merchantParser = merchantParser,
      authorizationParser = authorizationParser,
      requestFactory = new NetHttpTransport().createRequestFactory(),
      endpointUrl = s"http://localhost:$fatzebraPort"
    )

    driver.resetProbe()
  }

  "authorize request via FatZebra gateway" should {
    val someCurrencyAmount = CurrencyAmount("USD", 33.3)
    val someCreditCard = CreditCard(
      "4012888818888",
      YearMonth(2020, 12),
      Some(CreditCardOptionalFields.withFields(
        csc = Some("123"),
        holderName = Some("some holder name"))))
    val someDeal = Deal(
      id = "someDealId",
      title = null,
      description = null)
    val someCustomerIpAddress = "1.2.3.4"
    val someCustomer = Customer(ipAddress = Some(someCustomerIpAddress))

    "gracefully fail on invalid merchant key" in new Ctx {
      driver.aCreatePurchaseRequestFor(
        someMerchant.username,
        someMerchant.password,
        someCurrencyAmount,
        someDeal.id,
        someCustomerIpAddress,
        someCreditCard,
        capture = false) errors(
          StatusCodes.Unauthorized, new Response[Purchase](errors = List("Incorrect Username or Token")))

      fatzebra.authorize(
        merchantKey = merchantKey,
        creditCard = someCreditCard,
        currencyAmount = someCurrencyAmount,
        customer = Some(someCustomer),
        deal = Some(someDeal)
      ) must beAFailedTry(
        check = beAnInstanceOf[PaymentErrorException]
      )
    }

    "gracefully fail on invalid deal" in new Ctx {
      fatzebra.authorize(
        merchantKey = merchantKey,
        creditCard = someCreditCard,
        currencyAmount = someCurrencyAmount,
        customer = Some(someCustomer),
        deal = None
      ) must beAFailedTry(
        check = beAnInstanceOf[PaymentErrorException]
      )
    }

    "gracefully fail on invalid customer" in new Ctx {
      fatzebra.authorize(
        merchantKey = merchantKey,
        creditCard = someCreditCard,
        currencyAmount = someCurrencyAmount,
        customer = None,
        deal = Some(someDeal)
      ) must beAFailedTry(
        check = beAnInstanceOf[PaymentErrorException]
      )
    }

    "successfully yield an authorization key on valid request" in new Ctx {
      val somePurchaseId = "somePurchaseID"

      driver.aCreatePurchaseRequestFor(
        someMerchant.username,
        someMerchant.password,
        someCurrencyAmount,
        someDeal.id,
        someCustomerIpAddress,
        someCreditCard,
        capture = false
      ) returns somePurchaseId

      fatzebra.authorize(
        merchantKey = merchantKey,
        creditCard = someCreditCard,
        currencyAmount = someCurrencyAmount,
        customer = Some(someCustomer),
        deal = Some(someDeal)
      ) must beASuccessfulTry(
        check = beAuthorizationKey(
          authorization = beAuthorization(
            purchaseId = ===(somePurchaseId))
        )
      )
    }

    "gracefully fail on rejected card" in new Ctx {
      val somePurchaseId = "somePurchaseId"

      driver.aCreatePurchaseRequestFor(
        someMerchant.username,
        someMerchant.password,
        someCurrencyAmount,
        someDeal.id,
        someCustomerIpAddress,
        someCreditCard,
        capture = false
      ) isDeclined(somePurchaseId)

      fatzebra.authorize(
        merchantKey = merchantKey,
        creditCard = someCreditCard,
        currencyAmount = someCurrencyAmount,
        customer = Some(someCustomer),
        deal = Some(someDeal)
      ) must beAFailedTry(
        check = beAnInstanceOf[PaymentRejectedException]
      )
    }
  }


  "capture request via FatZebra gateway" should {
    "successfully yield a transaction ID on valid request" in new Ctx {
      val someAuthorization = FatzebraAuthorization(purchaseId = "somePurchaseId")
      val authorizationKey = authorizationParser.stringify(someAuthorization)
      val someAmount = 11.1

      driver.aCaptureRequestFor(
        someMerchant.username, someMerchant.password, someAuthorization.purchaseId, someAmount
      ) succeeds()

      fatzebra.capture(
        merchantKey = merchantKey,
        authorizationKey = authorizationKey,
        amount = someAmount
      ) must beASuccessfulTry(
        check = ===(someAuthorization.purchaseId)
      )
    }
  }


  "voidAuthorization request via FatZebra gateway" should {
    "successfully yield a transaction ID on valid request" in new Ctx {
      val someAuthorization = FatzebraAuthorization(purchaseId = "somePurchaseId")
      val authorizationKey = authorizationParser.stringify(someAuthorization)

      fatzebra.voidAuthorization(
        merchantKey = merchantKey,
        authorizationKey = authorizationKey
      ) must beASuccessfulTry(
        check = ===(someAuthorization.purchaseId)
      )
    }
  }


  step {
    driver.stopProbe()
  }
}
