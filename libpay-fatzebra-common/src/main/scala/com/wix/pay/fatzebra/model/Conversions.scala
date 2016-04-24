package com.wix.pay.fatzebra.model

import java.math.{BigDecimal => JBigDecimal}

object Conversions {
  def toFatzebraAmount(amount: Double): Int = {
    JBigDecimal.valueOf(amount).movePointRight(2).intValueExact()
  }

  def toFatzebraYearMonth(year: Int, month: Int): String = {
    f"$month%02d/$year%04d"
  }
}
