/*      __ __ _____  __                                              *\
**     / // // /_/ |/ /          Wix                                 **
**    / // // / /|   /           (c) 2006-2015, Wix LTD.             **
**   / // // / //   |            http://www.wix.com/                 **
**   \__/|__/_//_/| |                                                **
\*                |/                                                 */
package com.wix.pay.fatzebra


import com.google.api.client.http.{ByteArrayContent, GenericUrl, HttpRequestFactory}
import com.wix.pay.creditcard.CreditCard
import com.wix.pay.fatzebra.model.Conversions._
import com.wix.pay.fatzebra.model.{CaptureRequest, CreatePurchaseRequest, Purchase, Response}
import com.wix.pay.model.{CurrencyAmount, Customer, Deal}
import com.wix.pay.{PaymentErrorException, PaymentException, PaymentGateway, PaymentRejectedException}

import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}


object Endpoints {
  val production = "https://gateway.fatzebra.com.au/v1.0"
  val sandbox = "https://gateway.sandbox.fatzebra.com.au/v1.0"
}

class FatzebraGateway(requestFactory: HttpRequestFactory,
                      connectTimeout: Option[Duration] = None,
                      readTimeout: Option[Duration] = None,
                      numberOfRetries: Int = 0,
                      endpointUrl: String = Endpoints.production,
                      merchantParser: FatzebraMerchantParser = new JsonFatzebraMerchantParser,
                      authorizationParser: FatzebraAuthorizationParser = new JsonFatzebraAuthorizationParser) extends PaymentGateway {
  override def authorize(merchantKey: String,
                         creditCard: CreditCard,
                         currencyAmount: CurrencyAmount,
                         customer: Option[Customer],
                         deal: Option[Deal]): Try[String] = {
    def authorizeOperation: String = {
      require(deal.isDefined, "Deal is mandatory for FatZebra")
      require(customer.isDefined, "Customer is mandatory for FatZebra")

      val merchant = merchantParser.parse(merchantKey)
      val request = createPurchaseRequest(creditCard, currencyAmount, deal.get, customer.get, capture = false)
      val requestJson = CreatePurchaseRequestParser.stringify(request)
      val response = submitRequest("/purchases", merchant.username, merchant.password, requestJson)

      response match {
        case ApprovedPurchase(authorization) => authorizationParser.stringify(FatzebraAuthorization(authorization))
        case RejectedPurchase(message) => throw PaymentRejectedException(message)
        case ErroneousPurchase(errors) => throw PaymentErrorException(errors)
        case _ => throw new IllegalArgumentException("FatZebra response is unexpectedly empty")
      }
    }

    executeSafe(authorizeOperation)
  }

  override def capture(merchantKey: String, authorizationKey: String, amount: Double): Try[String] = {
    def captureOperation: String = {
      val merchant = merchantParser.parse(merchantKey)
      val authorization = authorizationParser.parse(authorizationKey)
      val request = CaptureRequest(amount = toFatzebraAmount(amount))
      val requestJson = CaptureRequestParser.stringify(request)
      val response = submitRequest(
        s"/purchases/${authorization.purchaseId}/capture",
        merchant.username,
        merchant.password,
        requestJson)

      response match {
        case CapturedPurchase(purchaseId) => purchaseId
        case RejectedCapture(message) => throw PaymentRejectedException(message)
        case ErroneousPurchase(errors) => throw PaymentErrorException(errors)
        case _ => throw new IllegalArgumentException("FatZebra response is unexpectedly empty")
      }
    }

    executeSafe(captureOperation)
  }

  override def sale(merchantKey: String,
                    creditCard: CreditCard,
                    currencyAmount: CurrencyAmount,
                    customer: Option[Customer], deal: Option[Deal]): Try[String] = {
    def saleOperation: String = {
      require(deal.isDefined, "Deal is mandatory for FatZebra")
      require(customer.isDefined, "Customer is mandatory for FatZebra")

      val merchant = merchantParser.parse(merchantKey)
      val request = createPurchaseRequest(creditCard, currencyAmount, deal.get, customer.get, capture = true)
      val requestJson = CreatePurchaseRequestParser.stringify(request)
      val response = submitRequest("/purchases", merchant.username, merchant.password, requestJson)

      response match {
        case ApprovedPurchase(purchaseId) => purchaseId
        case RejectedPurchase(message) => throw PaymentRejectedException(message)
        case ErroneousPurchase(errors) => throw PaymentErrorException(errors)
        case _ => throw new IllegalArgumentException("FatZebra response is unexpectedly empty")
      }
    }

    executeSafe(saleOperation)
  }

  override def voidAuthorization(merchantKey: String, authorizationKey: String): Try[String] = {
    Try (authorizationParser.parse(authorizationKey).purchaseId)
  }

  private def submitRequest(resource: String,
                            username: String,
                            password: String,
                            requestJson: String): Response[Purchase] = {
    val httpRequest = requestFactory.buildPostRequest(
      new GenericUrl(s"$endpointUrl$resource"),
      new ByteArrayContent(
        "application/json",
        requestJson.getBytes("UTF-8")))

    connectTimeout foreach (to => httpRequest.setConnectTimeout(to.toMillis.toInt))
    readTimeout foreach (to => httpRequest.setReadTimeout(to.toMillis.toInt))
    httpRequest.setNumberOfRetries(numberOfRetries)

    httpRequest.getHeaders.setBasicAuthentication(username, password)
    httpRequest.setThrowExceptionOnExecuteError(false)

    val httpResponse = httpRequest.execute()

    try {
      PurchaseResponseParser.parse(httpResponse.parseAsString())
    } finally {
      httpResponse.ignore()
    }
  }

  private def createPurchaseRequest(creditCard: CreditCard,
                                    currencyAmount: CurrencyAmount,
                                    deal: Deal,
                                    customer: Customer,
                                    capture: Boolean): CreatePurchaseRequest = {
    require(creditCard.holderName.nonEmpty, "Credit card holder name is mandatory for FatZebra")
    require(deal.id.nonEmpty, "Deal ID is mandatory for FatZebra")
    require(customer.ipAddress.nonEmpty, "Customer IP address is mandatory for FatZebra")

    CreatePurchaseRequest(
      card_holder = creditCard.holderName.get,
      card_number = creditCard.number,
      card_expiry = toFatzebraYearMonth(
        year = creditCard.expiration.year,
        month = creditCard.expiration.month),
      cvv = creditCard.csc.orNull,
      amount = toFatzebraAmount(currencyAmount.amount),
      reference = deal.id,
      customer_ip = customer.ipAddress.get,
      currency = currencyAmount.currency,
      capture = capture)
  }

  private def executeSafe(f: => String): Try[String] = {
    Try {
      f
    } match {
      case Success(value) => Success(value)
      case Failure(e: PaymentException) => Failure(e)
      case Failure(e) => Failure(PaymentErrorException(e.getMessage, e))
    }
  }
}

object ApprovedPurchase {
  def unapply(response: Response[Purchase]): Option[String] = {
    response match {
      case Response(Some(purchase), _) if purchase.message == "Approved" =>
        Option(purchase.id)
      case _ =>
        None
    }
  }
}

object RejectedPurchase {
  def unapply(response: Response[Purchase]): Option[String] = {
    response match {
      case Response(Some(purchase), _) if purchase.message != "Approved" => Option(purchase.message)
      case _ => None
    }
  }
}

object ErroneousPurchase {
  def unapply(response: Response[Purchase]): Option[String] = {
    response match {
      case Response(None, Some(errors)) if errors.nonEmpty => Option(errors.toString())
      case _ => None
    }
  }
}

object CapturedPurchase {
  def unapply(response: Response[Purchase]): Option[String] = {
    response match {
      case Response(Some(purchase), _) if purchase.captured => Option(purchase.id)
      case _ => None
    }
  }
}

object RejectedCapture {
  def unapply(response: Response[Purchase]): Option[String] = {
    response match {
      case Response(Some(purchase), _) if !purchase.captured => Option(purchase.message)
      case _ => None
    }
  }
}
