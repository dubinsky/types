package org.podval.types.untyped

import scala.annotation.tailrec

sealed trait Term {
  final override def toString: String = Term.toString(this)
}

// Program=Proof 3.1
object Term {

  type VariableName = Char

  final case class Variable(name: VariableName) extends Term
  final case class Application(function: Term, argument: Term) extends Term
  final case class λ(variable: VariableName, body: Term) extends Term

  def parse(string: String): Term = parse(None, string)

  @tailrec
  private def parse(result: Option[Term], string: String): Term = if (string.isEmpty) result.get else {
    val (term: Term, tail: String) =
      if (string(0) == 'λ') {
        val dot: Int = string.indexOf('.')
        if (dot == -1) throw new IllegalArgumentException("No '.' in an abstraction")
        (abstractions(variables = string.substring(1, dot), body = parse(string.substring(dot + 1))), "")
      } else if (string(0) == '(') {
        val closing: Int = indexOfClosingParen(string.tail, 1, 0)
        if (closing == -1) throw new IllegalArgumentException("No closing paren")
        (parse(string.substring(1, closing)), string.substring(closing+1))
      } else (Variable(string(0)), string.tail)

    parse(
      result.map(result => Application(result, term)).orElse(Some(term)),
      tail
    )
  }

  @tailrec
  private def indexOfClosingParen(string: String, result: Int, level: Int): Int = if (string.isEmpty) -1 else {
    val char: Char = string.head
    if ((level == 0) && (char == ')')) result else indexOfClosingParen(
      string.tail,
      result+1,
      if (char == '(') level + 1 else if (char == ')') level - 1 else level
    )
  }

  @tailrec
  private def abstractions(variables: Seq[VariableName], body: Term): λ = {
    val result: λ = λ(variables.last, body)
    if (variables.length == 1) result else abstractions(variables.init, result)
  }

  def toString(term: Term): String = {
    @tailrec
    def abstractions(variables: Seq[Char], body: Term): String = body match {
      case λ(variable, body) => abstractions(variables :+ variable, body)
      case body => s"λ${variables.mkString("")}.${toString(body)}"
    }

    def inParens(condition: Boolean, term: Term): String =
      if (condition) "(" + term.toString + ")" else term.toString

    term match {
      case Variable(name)  => name.toString
      case λ(variable, body) => abstractions(Seq(variable), body)
      case Application(function, argument) =>
        inParens(function.isInstanceOf[λ], function) +
        inParens(!argument.isInstanceOf[Variable], argument)
    }
  }

  def FV(term: Term): Set[VariableName] = term match {
    case Variable(name)  => Set(name)
    case Application(function, argument) => FV(function) ++ FV(argument)
    case λ(variable, body) => FV(body) - variable
  }

  def isClosed(term: Term): Boolean = FV(term).isEmpty

  def isFresh(variable: VariableName, term: Term): Boolean = !FV(term).contains(variable)

  def renameIn(term: Term, to: VariableName, from: VariableName): Term = term match {
    case Variable(name) if name == from => Variable(to)
    case Variable(name)                 => Variable(name)

    case Application(function, argument) => Application(
      renameIn(function, to, from),
      renameIn(argument, to, from)
    )

    case λ(variable, body) if variable == from => λ(variable, body)
    case λ(variable, body) if variable == to   => λ(from    , renameIn(body, to, from))
    case λ(variable, body) => λ(variable, renameIn(body, to, from))
  }
}
