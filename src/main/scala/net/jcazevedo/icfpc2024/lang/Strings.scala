package net.jcazevedo.icfpc2024.lang

object Strings {
  final val Order: String =
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`|~ \n"

  def fromAlien(alien: String): String =
    alien.map(ch => Order(ch.toInt - 33)).mkString

  def fromHuman(human: String): String =
    human.map(ch => (Order.indexOf(ch) + 33).toChar).mkString
}
