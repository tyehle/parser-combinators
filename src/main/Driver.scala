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

//  override val skipWhitespace = false
  private var currentReg = 'A' - 1
  val registerAssignment = mutable.Map.empty[String, Char]
  private def getOrMake(name: String):Option[Char] = {
    if(registerAssignment.contains(name)) Some(registerAssignment(name))
    else allocate(name)
  }
  private def allocate(name: String):Option[Char] = {
    if(currentReg < 'B') {
      currentReg += 1
      registerAssignment(name) = currentReg.toChar
      Some(currentReg.toChar)
    }
    else
      None
  }

  def constant:Parser[String] = """\d+""".r ^^ { _.toString }
  def variable:Parser[String] = """[a-zA-Z]+""".r ^^ { _.toString }
  def string = ( "\"" ~> "[a-zA-Z0-9]*".r <~ "\"" ) ^^ { chars => "["+chars+"]"}


  // program -> statement *
  def program = rep(statement) ^^ { _.mkString }

  // statement -> variable "=" exp ";" | "print" exp ";"
  def statement =
    ( variable ~ ( "=" ~> exp <~ ";" )) ^^? { case name ~ value =>
      getOrMake(name) match {
        case Some(reg) => Right(s"$value s$reg\n")
        case None => Left("Too many variables")
      }
    } | ( "print" ~> exp <~ ";" ) ^^ { case value => s"$value n \n" }

  // exp -> term ("+" | "-") exp | term
  def exp:Parser[String] = ( term ~ ("+" | "-") ~ exp ) ^^ {
    case left ~ "+" ~ right => s"$left $right +"
    case left ~ "-" ~ right => s"$left $right -"
  } | term

  // term -> factor ("*" | "/") term | factor
  def term:Parser[String] = ( factor ~ ("*" | "/") ~ term ) ^^ {
    case left ~ "*" ~ right => s"$left $right *"
    case left ~ "/" ~ right => s"$left $right /"
  } | factor

  // factor -> "-" element | element
  def factor = ( "-" ~> element ) ^^ { elem => s"_$elem" } | element

  // element -> constant | variable | "(" exp ")"
  def element = constant | variable ^^? {
    name => registerAssignment.get(name) match {
      case Some(reg) => Right(s"l$reg")
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
