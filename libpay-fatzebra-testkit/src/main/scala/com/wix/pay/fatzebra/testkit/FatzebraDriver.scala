package com.wix.pay.fatzebra.testkit


import com.google.api.client.util.Base64
import com.wix.hoopoe.http.testkit.EmbeddedHttpProbe
import com.wix.pay.creditcard.CreditCard
import com.wix.pay.fatzebra.model.Conversions._
import com.wix.pay.fatzebra.model._
import com.wix.pay.fatzebra.{CaptureRequestParser, CreatePurchaseRequestParser, PurchaseResponseParser}
import com.wix.pay.model.CurrencyAmount
import spray.http._


class FatzebraDriver(port: Int) {
  private val probe = new EmbeddedHttpProbe(port, EmbeddedHttpProbe.NotFoundHandler)
  private val createPurchaseRequestParser = new CreatePurchaseRequestParser
  private val captureRequestParser = new CaptureRequestParser
  private val purchaseResponseParser = new PurchaseResponseParser

  def startProbe() {
    probe.doStart()
  }

  def stopProbe() {
    probe.doStop()
  }

  def resetProbe() {
    probe.handlers.clear()
  }

  def aCreatePurchaseRequestFor(username: String,
                                password: String,
                                currencyAmount: CurrencyAmount,
                                reference: String,
                                customerIpAddress: String,
                                creditCard: CreditCard,
                                capture: Boolean): CreatePurchaseCtx = {
    new CreatePurchaseCtx(
      username = username,
      password = password,
      currencyAmount = currencyAmount,
      reference = reference,
      customerIpAddress = customerIpAddress,
      creditCard = creditCard,
      capture = capture)
  }

  def aCaptureRequestFor(username: String,
                         password: String,
                         purchaseId: String,
                         amount: Double): CaptureCtx = {
    new CaptureCtx(
      username = username,
      password = password,
      purchaseId = purchaseId,
      amount = amount)
  }

  abstract class Ctx(val resource: String, username: String, password: String) {
    def isStubbedRequestEntity(entity: HttpEntity, headers: List[HttpHeader]): Boolean = {
      isAuthorized(headers) && verifyContent(entity)
    }

    private def isAuthorized(headers: List[HttpHeader]): Boolean = {
      val expectedValue = s"Basic ${Base64.encodeBase64String(s"$username:$password".getBytes("UTF-8"))}"
      for (header <- headers) {
        if (header.name == "Authorization") {
          return header.value == expectedValue
        }
      }
      false
    }

    protected def verifyContent(entity: HttpEntity): Boolean

    def returns(statusCode: StatusCode, response: Response[Purchase]): Unit = {
      probe.handlers += {
        case HttpRequest(
        HttpMethods.POST,
        Uri.Path(`resource`),
        headers,
        entity,
        _) if isStubbedRequestEntity(entity, headers) =>
          HttpResponse(
            status = statusCode,
            entity = HttpEntity(ContentTypes.`application/json`, purchaseResponseParser.stringify(response)))
      }
    }

    def errors(statusCode: StatusCode, response: Response[Purchase]): Unit = {
      probe.handlers += {
        case HttpRequest(
        HttpMethods.POST,
        Uri.Path(`resource`),
        headers,
        entity,
        _) if isStubbedRequestEntity(entity, headers) =>
          HttpResponse(
            status = statusCode,
            entity = HttpEntity(ContentTypes.`application/json`, purchaseResponseParser.stringify(response)))
      }
    }
  }

  class CreatePurchaseCtx(username: String,
                          password: String,
                          currencyAmount: CurrencyAmount,
                          reference: String,
                          customerIpAddress: String,
                          creditCard: CreditCard,
                          capture: Boolean) extends Ctx("/purchases", username, password) {
    private val expectedCreatePurchaseRequest = CreatePurchaseRequest(
      card_holder = creditCard.holderName.get,
      card_number = creditCard.number,
      card_expiry = toFatzebraYearMonth(
        year = creditCard.expiration.year,
        month = creditCard.expiration.month
      ),
      cvv = creditCard.csc.orNull,
      amount = toFatzebraAmount(currencyAmount.amount),
      reference = reference,
      customer_ip = customerIpAddress,
      currency = currencyAmount.currency,
      capture = capture)

    override def verifyContent(entity: HttpEntity): Boolean = {
      val createPurchaseRequest = createPurchaseRequestParser.parse(entity.asString)
      createPurchaseRequest == expectedCreatePurchaseRequest
    }

    def returns(purchaseId: String): Unit = {
      returns(
        statusCode = StatusCodes.Created,
        response = new Response[Purchase](Some(Purchase(
          authorization = "55355",
          id = purchaseId,
          card_number = null,
          card_holder = null,
          card_expiry = null,
          card_token = null,
          amount = 1000,
          decimal_amount = 10.0,
          successful = None,
          authorized = true,
          message = "Approved",
          reference = reference,
          currency = currencyAmount.currency,
          transaction_id = null,
          settlement_date = null,
          transaction_date = null,
          response_code = "99",
          captured = false,
          captured_amount = null,
          rrn = null,
          cvv_match = "U"))))
    }

    def isDeclined(purchaseId: String): Unit = {
      returns(
        statusCode = StatusCodes.OK,
        response = new Response[Purchase](Some(Purchase(
          authorization = null,
          id = purchaseId,
          card_number = null,
          card_holder = null,
          card_expiry = null,
          card_token = null,
          amount = 1000,
          decimal_amount = 10.0,
          successful = Some(false),
          authorized = false,
          message = "Declined",
          reference = reference,
          currency = currencyAmount.currency,
          transaction_id = null,
          settlement_date = null,
          transaction_date = null,
          response_code = "99",
          captured = false,
          captured_amount = null,
          rrn = null,
          cvv_match = "U"))))
    }
  }

  class CaptureCtx(username: String,
                   password: String,
                   purchaseId: String,
                   amount: Double) extends Ctx(s"/purchases/$purchaseId/capture", username, password) {
    private val expectedCaptureRequest = CaptureRequest(
      amount = toFatzebraAmount(amount)
    )

    override def verifyContent(entity: HttpEntity): Boolean = {
      val captureRequest = captureRequestParser.parse(entity.asString)
      captureRequest == expectedCaptureRequest
    }

    def succeeds(): Unit = {
      returns(
        statusCode = StatusCodes.OK,
        response = new Response[Purchase](Some(Purchase(
          authorization = null,
          id = purchaseId,
          card_number = null,
          card_holder = null,
          card_expiry = null,
          card_token = null,
          amount = Conversions.toFatzebraAmount(amount),
          decimal_amount = amount,
          successful = Some(true),
          authorized = false,
          message = "someMessage",
          reference = "someReference",
          currency = "USD",
          transaction_id = null,
          settlement_date = null,
          transaction_date = null,
          response_code = "0",
          captured = true,
          captured_amount = Some(Conversions.toFatzebraAmount(amount)),
          rrn = null,
          cvv_match = "U"))))
    }
  }
}