/*      __ __ _____  __                                              *\
**     / // // /_/ |/ /          Wix                                 **
**    / // // / /|   /           (c) 2006-2015, Wix LTD.             **
**   / // // / //   |            http://www.wix.com/                 **
**   \__/|__/_//_/| |                                                **
\*                |/                                                 */
package com.wix.pay.fatzebra.model


case class Response[T] private (val response: Option[T], val errors: Option[List[String]]) {
  def this(response: Option[T]) {this(response = response, errors = None)}
  def this(errors: List[String]) {this (response = None, errors = Option(errors))}
}