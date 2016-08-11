package com.wix.pay.fatzebra

import com.wix.pay.fatzebra.model.{Purchase, Response}
import org.json4s.DefaultFormats
import org.json4s.native.Serialization

object PurchaseResponseParser {
  private implicit val formats = DefaultFormats

  def parse(str: String): Response[Purchase] = {
    Serialization.read[Response[Purchase]](str)
  }

  def stringify(obj: Response[Purchase]): String = {
    Serialization.write(obj)
  }
}
