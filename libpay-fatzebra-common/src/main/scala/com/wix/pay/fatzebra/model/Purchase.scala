package com.wix.pay.fatzebra.model

case class Purchase(authorization: String, id: String,
                    card_number: String, card_holder: String, card_expiry: String, card_token: String,
                    amount: Int, decimal_amount: Double, successful: Option[Boolean], authorized: Boolean, message: String,
                    reference: String, currency: String, transaction_id: String, settlement_date: String, transaction_date: String,
                    response_code: String, captured: Boolean, captured_amount: Option[Int], rrn: String, cvv_match: String)
