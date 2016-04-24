package com.wix.pay.fatzebra.model

case class CreatePurchaseRequest(card_holder: String,
                                 card_number: String,
                                 card_expiry: String,
                                 cvv: String,
                                 amount: Int,
                                 reference: String,
                                 customer_ip: String,
                                 currency: String,
                                 capture: Boolean) extends Serializable
