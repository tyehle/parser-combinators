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
    def combine(other: Output, op: String): Option[Output] = {
      op match {
        case "+" | "-" | "*" | "/"  if other.kind == "int" && kind == "int" =>
          Some(Output(s"$statements ${other.statements} $op", "int"))

        case "<" | ">" | "==" if other.kind == "int" && kind == "int" =>
          val outputOp:String = op.take(1)
          Some(Output(genMacro(statements, other.statements, outputOp), "boolean"))

        case "==" if other.kind == "boolean" && kind == "boolean" =>
          Some(Output(genMacro(statements, other.statements, "="), "boolean"))

        case _ => None
      }
    }

    def convert(op: String): Option[Output] = op match {
      case "-" if kind == "int" => Some(Output(s"$statements _1 *", "int"))

      case "!" if kind == "boolean" => Some(Output(s"$statements 1 + 2 %", "boolean"))

      case _ => None
    }
  }

  def applicationKindError(op: String, args: Output*) = {
    s"Can't apply $op to (${args.map(_.kind).mkString(", ")})"
  }

  def genMacro(a: String, b: String, op: String, tmpReg:String = "t") = {
    s"$a 0 r $b [s$tmpReg 1] s$tmpReg $op$tmpReg"
  }

  private var currentReg = 'A' - 1
  private val registerAssignment = mutable.Map.empty[String, (Char, String)]
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
  def boolConstant: Parser[String] = "true" ^^^ "1" | "false" ^^^ "0"
  def variable:Parser[String] = """[a-zA-Z]+""".r ^^ { _.toString }
  def string = ( "\"" ~> "[a-zA-Z0-9]*".r <~ "\"" ) ^^ { chars => "["+chars+"]"}
  def kind = """int|string|boolean""".r ^^ { _.toString }


  // program -> statement *
  def program = rep(statement) ^^ { _.mkString }

  // statement -> type variable "=" exp ";" | variable "=" exp ";" | "print" exp ";" | "newline" ";"
  def statement =
    ( kind ~ variable ~ ( "=" ~> exp <~ ";" ) ) ^^? { case varKind ~ name ~ value =>
      lookup(name) match {
        case Some(reg) => Left(s"Variable $name is already defined")
        case None if varKind != value.kind => Left(s"Expected $varKind, received ${value.kind}")
        case None => allocate(name, varKind) match {
          case Some(reg) => Right(s"${value.statements} s$reg\n")
          case None => Left("Too many variables")
        }
      }
    } | ( variable ~ ( "=" ~> exp <~ ";" ) ) ^^? { case name ~ Output(value, expKind) =>
      lookup(name) match {
        case Some((reg, regKind)) if regKind == expKind => Right(s"$value s$reg\n")
        case Some((reg, regKind)) => Left(s"Expected $regKind, received $expKind")
        case None => Left(s"Unknown variable $variable")
      }
    } | ( "print" ~> exp <~ ";" ) ^^ {
      case Output(value, "boolean") => s"$value [false] r 1 [st [true]] st =t n\n"
      case Output(value, _) => s"$value n \n"
    } | ("newline" ~ ";") ^^^ "\n"

  // exp -> arithExp (">" | "<" | "==") exp | arithExp
  def exp:Parser[Output] = ( arithExp ~ (">" | "<" | "==") ~ exp ) ^^? { case left ~ op ~ right =>
    left.combine(right, op) match {
      case Some(output) => Right(output)
      case None => Left(applicationKindError(op, left, right))
    }
  } | arithExp

  // arithExp -> term ("+" | "-") arithExp | term
  def arithExp:Parser[Output] = ( term ~ ("+" | "-") ~ arithExp ) ^^? { case left ~ op ~ right =>
    left.combine(right, op) match {
      case Some(output) => Right(output)
      case None => Left(applicationKindError(op, left, right))
    }
  } | term

  // term -> factor ("*" | "/") term | factor
  def term:Parser[Output] = ( factor ~ ("*" | "/") ~ term ) ^^? { case left ~ op ~ right =>
    left.combine(right, op) match {
      case Some(output) => Right(output)
      case None => Left(applicationKindError(op, left, right))
    }
  } | factor

  // factor -> ("-" | "!") element | element
  def factor = ( ( "-" | "!" ) ~ element ) ^^? { case op ~ value =>
    value.convert(op) match {
      case Some(output) => Right(output)
      case None => Left(applicationKindError(op, value))
    }
  } | element

  // element -> constant | boolConstant | string | variable | "(" exp ")"
  def element =
    constant ^^ {Output(_, "int")} | boolConstant ^^ {Output(_, "boolean")} | string ^^ {Output(_, "string")} |
    variable ^^? { name => lookup(name) match {
      case Some((reg, kind)) => Right(Output(s"l$reg", kind))
      case None => Left(s"Undefined variable $name")
    }} | "(" ~> exp <~ ")"

}

object Driver {
  def main(args: Array[String]):Unit = {
    val example = "int x = 12; int y = 42; print x+y;"
    val result = SimpleParser.parseAll(SimpleParser.program, new InputStreamReader(System.in)) match {
      case SimpleParser.Success(matched, reader) => println(matched); 0
      case SimpleParser.Failure(msg, _) => System.err.println(s"FAILURE: $msg"); 1
      case SimpleParser.Error(msg, _) => System.err.println(s"ERROR: $msg"); 2
    }
    System.exit(result)
  }
}
