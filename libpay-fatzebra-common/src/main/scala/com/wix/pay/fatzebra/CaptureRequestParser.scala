package com.wix.pay.fatzebra

import com.wix.pay.fatzebra.model.CaptureRequest
import org.json4s.DefaultFormats
import org.json4s.native.Serialization

class CaptureRequestParser {
  private implicit val formats = DefaultFormats

  def parse(str: String): CaptureRequest = {
    Serialization.read[CaptureRequest](str)
  }

  def stringify(obj: CaptureRequest): String = {
    Serialization.write(obj)
  }
}
