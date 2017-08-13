package com.wix.pay.fatzebra

import com.wix.pay.creditcard.CreditCard
import com.wix.pay.fatzebra.model.CreatePurchaseRequest
import com.wix.pay.model.{Customer, Deal, Payment}
import com.wix.pay.testkit.LibPayTestSupport
import org.specs2.matcher.Matcher
import org.specs2.mutable.SpecWithJUnit
import org.specs2.specification.Scope

class FatZebraRequestBuilderTest extends SpecWithJUnit with LibPayTestSupport {
  "throw IllegalArgumentException when card holder name is absent" in new ctx {
    createPurchaseRequest(creditCard = _.withoutHolderName) must throwAn[IllegalArgumentException]
  }

  "throw IllegalArgumentException when payment installments != 1" in new ctx {
    createPurchaseRequest(payment = _.withInstallments(2)) must throwAn[IllegalArgumentException]
  }

  "throw IllegalArgumentException when deal is absent" in new ctx {
    createPurchaseRequest2(deal = None) must throwAn[IllegalArgumentException]
  }

  "throw IllegalArgumentException when deal id is empty" in new ctx {
    createPurchaseRequest(deal = _.withId("")) must throwAn[IllegalArgumentException]
  }

  "use customer ip when it is present in request" in new ctx {
    createPurchaseRequest(customer = _.withIpAddress(someIp)) must haveIp(someIp)
  }

  "use default ip when customer is absent in request" in new ctx {
    createPurchaseRequest2(customer = None) must haveIp(defaultIp)
  }

  "use default ip when customer ip is absent in request" in new ctx {
    createPurchaseRequest(customer = _.withoutIpAddress) must haveIp(defaultIp)
  }

  trait ctx extends Scope {
    val someIp = "someIp"
    val defaultIp = "someDefaultIp"
    val requestBuilder = new FatZebraRequestBuilder(defaultIp)

    def createPurchaseRequest(creditCard: CreditCard => CreditCard = identity,
                              payment: Payment => Payment = identity,
                              deal: Deal => Deal = identity,
                              customer: Customer => Customer = identity): CreatePurchaseRequest =
      createPurchaseRequest2(creditCard(someCreditCard), payment(somePayment), Some(deal(someDeal)), Some(customer(someCustomer)))

    def createPurchaseRequest2(creditCard: CreditCard = someCreditCard,
                               payment: Payment = somePayment,
                               deal: Option[Deal] = Some(someDeal),
                               customer: Option[Customer] = None): CreatePurchaseRequest =
      requestBuilder.createPurchaseRequest(creditCard, payment, deal, customer, capture = true)

    def haveIp(ip: String): Matcher[CreatePurchaseRequest] =
      be_==(ip) ^^ {(_: CreatePurchaseRequest).customer_ip}
  }
}
