package main

import java.io.InputStreamReader

import scala.collection.mutable
import scala.util.parsing.combinator.RegexParsers

/**
 * @author Tobin Yehle
 */


object SimpleParser extends RegexParsers {
  implicit class RichParser[+T](p: Parser[T]) {
    def ^^? [U](f: T => Either[String,U]): Parser[U] = new Parser[U] {
      def apply(in: Input) = p(in) match {
        case Success(x, in1) => f(x) match {
          case Left(error) => Failure(error,in1)
          case Right(x1) => Success(x1,in1)
        }
        case failure:Failure => failure
        case error:Error => error
      }
    }
  }

  case class Output(statements: String, kind: String) {
    def isInt = kind == "int"
  }

//  override val skipWhitespace = false
  private var currentReg = 'A' - 1
  val registerAssignment = mutable.Map.empty[String, (Char, String)]
  private def lookup(name: String):Option[(Char, String)] = {
    if(registerAssignment.contains(name)) Some(registerAssignment(name))
    else None
  }
  private def allocate(name: String, kind: String):Option[Char] = {
    if(currentReg < 'B') {
      currentReg += 1
      registerAssignment(name) = (currentReg.toChar, kind)
      Some(currentReg.toChar)
    }
    else
      None
  }

  def constant:Parser[String] = """\d+""".r ^^ { _.toString }
  def variable:Parser[String] = """[a-zA-Z]+""".r ^^ { _.toString }
  def string = ( "\"" ~> "[a-zA-Z0-9]*".r <~ "\"" ) ^^ { chars => "["+chars+"]"}
  def kind = """int|string|boolean""".r ^^ { _.toString }


  // program -> statement *
  def program = rep(statement) ^^ { _.mkString }

  // statement -> type variable "=" exp ";" | variable "=" exp ";" | "print" exp ";"
  def statement =
    ( kind ~ variable ~ ( "=" ~> exp <~ ";" ) ) ^^? { case varKind ~ name ~ value =>
      lookup(name) match {
        case Some(reg) => Left(s"Variable $name is already defined")
        case None => allocate(name, varKind) match {
          case Some(reg) => Right(s"${value.statements} s$reg\n")
          case None => Left("Too many variables")
        }
      }
    } | ( variable ~ ("" ~> exp <~ ";") ) ^^? { case name ~ Output(value, expKind) =>
      lookup(name) match {
        case Some((reg, regKind)) if regKind == expKind => Right(s"$value s$reg\n")
        case Some((reg, regKind)) => Left(s"Expected $regKind, received $expKind")
        case None => Left(s"Unknown variable $variable")
      }
    } | ( "print" ~> exp <~ ";" ) ^^ { case Output(value, _) => s"$value n \n" }

  // exp -> term ("+" | "-") exp | term
  def exp:Parser[Output] = ( term ~ ("+" | "-") ~ exp ) ^^? {
    case left ~ "+" ~ right if left.isInt && right.isInt =>
      Right(Output(s"${left.statements} ${right.statements} +", right.kind))
    case left ~ "-" ~ right if left.isInt && right.isInt =>
      Right(Output(s"${left.statements} ${right.statements} -", right.kind))
    case left ~ _ ~ right =>
      Left(s"Expected ints, received ${left.kind} and ${right.kind}")
  } | term

  // term -> factor ("*" | "/") term | factor
  def term:Parser[Output] = ( factor ~ ("*" | "/") ~ term ) ^^? {
    case left ~ "*" ~ right if left.isInt && right.isInt =>
      Right(Output(s"${left.statements} ${right.statements} *", right.kind))
    case left ~ "/" ~ right if left.isInt && right.isInt =>
      Right(Output(s"${left.statements} ${right.statements} /", right.kind))
    case left ~ _ ~ right =>
      Left(s"Expected ints, received ${left.kind} and ${right.kind}")
  } | factor

  // factor -> "-" element | element
  def factor = ( "-" ~> element ) ^^? {
    case elem if elem.isInt => Right(Output(s"_${elem.statements}", elem.kind))
    case elem => Left(s"Expected int, received ${elem.kind}")
  } | element

  // element -> constant | variable | "(" exp ")"
  def element = constant ^^ {Output(_, "int")} | variable ^^? {
    name => lookup(name) match {
      case Some((reg, kind)) => Right(Output(s"l$reg", kind))
      case None => Left(s"Undefined variable $name")
    }
  } | "(" ~> exp <~ ")"

}

object Driver {
  def main(args: Array[String]):Unit = {
    val example = "x = 12; y = 42; print x+y;"
    SimpleParser.parseAll(SimpleParser.program, new InputStreamReader(System.in)) match {
      case SimpleParser.Success(matched, reader) => println(matched)
      case SimpleParser.Failure(msg, _) => println(s"FAILURE: $msg")
      case SimpleParser.Error(msg, _) => println(s"ERROR: $msg")
    }
  }
}
