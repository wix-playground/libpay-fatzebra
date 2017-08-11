package com.wix.pay.fatzebra

import com.wix.pay.creditcard.CreditCard
import com.wix.pay.fatzebra.model.Conversions.{toFatzebraAmount, toFatzebraYearMonth}
import com.wix.pay.fatzebra.model.CreatePurchaseRequest
import com.wix.pay.model.{Customer, Deal, Payment}

class FatZebraRequestBuilder(defaultIp: String = "127.0.0.1") {

  def createPurchaseRequest(creditCard: CreditCard,
                            payment: Payment,
                            deal: Option[Deal],
                            customer: Option[Customer],
                            capture: Boolean): CreatePurchaseRequest = {
    require(creditCard.holderName.nonEmpty, "Credit card holder name is mandatory for FatZebra")
    require(payment.installments == 1, "FatZebra does not support installments")
    require(deal.isDefined, "Deal is mandatory for FatZebra")
    require(deal.get.id.nonEmpty, "Deal ID is mandatory for FatZebra")

    CreatePurchaseRequest(
      card_holder = creditCard.holderName.get,
      card_number = creditCard.number,
      card_expiry = toFatzebraYearMonth(
        year = creditCard.expiration.year,
        month = creditCard.expiration.month),
      cvv = creditCard.csc.orNull,
      amount = toFatzebraAmount(payment.currencyAmount.amount),
      reference = deal.get.id,
      customer_ip = customer.flatMap(_.ipAddress).getOrElse(defaultIp),
      currency = payment.currencyAmount.currency,
      capture = capture)
  }
}
