package com.wix.pay.fatzebra

import com.wix.pay.fatzebra.model.CreatePurchaseRequest
import org.json4s.DefaultFormats
import org.json4s.native.Serialization

object CreatePurchaseRequestParser {
  private implicit val formats = DefaultFormats

  def parse(str: String): CreatePurchaseRequest = {
    Serialization.read[CreatePurchaseRequest](str)
  }

  def stringify(obj: CreatePurchaseRequest): String = {
    Serialization.write(obj)
  }
}
