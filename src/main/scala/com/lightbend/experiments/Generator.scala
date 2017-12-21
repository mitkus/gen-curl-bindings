package com.lightbend.experiments

import org.anarres.cpp._
import scala.annotation.tailrec

object Generator {

  private def parseInt(str: String) : Option[Int] = {
    try {
      if (str.startsWith("0x"))
        Some(Integer.parseInt(str.substring(2), 16))
      else
        Some(str.toInt)
    } catch {
      case _: NumberFormatException => None
    }
  }

  // Helper token predicates
  private def id(name: String)(t: Token) = {
    t != null && t.getType == Token.IDENTIFIER && t.getText == name
  }

  private def tok(name: String)(t: Token) = {
    t != null && t.getText == name
  }

  private def isId(t: Token) = t != null && t.getType == Token.IDENTIFIER
  private val isTypedef = id("typedef") _
  private val isEnum = id("enum") _
  private val isStruct = id("struct") _
  private val isOpenBrace = tok("{") _
  private val isCloseBrace = tok("}") _
  private val isPlus = tok("+") _
  private def isNumber(t: Token) = t != null && t.getType == Token.NUMBER && parseInt(t.getText).isDefined

  // Higher level token concept with hierarchy and some semantics
  sealed trait Node
  case class Empty() extends Node
  case class Single(t: Token) extends Node
  case class Number(n: Int) extends Node
  case class Enum(name: String, inside: Node) extends Node
  case class Struct(name: String, inside: Node) extends Node
  case class Scope(ts: Seq[Node]) extends Node

  def parseNodes(tokens: List[Token]) : List[Node] = {

    def parseScope(tokens: List[Token], acc: List[Node] = List.empty) : (Scope, List[Token]) = {
      tokens match {
        case a :: ts if isCloseBrace(a) => (Scope(acc.reverse), ts)
        case a :: ts if isOpenBrace(a) => {
          val (scope, rest) = parseScope(ts)
          parseScope(rest, scope :: acc)
        }
        case a :: ts if isNumber(a) => {
          parseScope(ts, Number(parseInt(a.getText).get) :: acc)
        }
        case a :: ts => {
          parseScope(ts, Single(a) :: acc)
        }
        case _ => throw new Exception("Malformed scope")
      }
    }

    def parse(tokens: List[Token], acc: List[Node] = List.empty) : List[Node] = {
      tokens match {
        case a :: b :: ts if isTypedef(a) && isEnum(b) => {
          parse(ts, Enum("[unresolved]", Empty()) :: acc)
        }
        case a :: b :: ts if isTypedef(a) && isStruct(b) => {
          parse(ts, Struct("[unresolved]", Empty()) :: acc)
        }
        case a :: b :: ts if isStruct(a) && isId(b) => {
          parse(ts, Struct(b.getText, Empty()) :: acc)
        }
        case a :: ts if isOpenBrace(a) => {
          val (scope, rest) = parseScope(ts)
          parse(rest, scope :: acc)
        }
        case a :: ts if isNumber(a) => {
          parse(ts, Number(parseInt(a.getText).get) :: acc)
        }
        case a :: ts => {
          parse(ts, Single(a) :: acc)
        }
        case _ => acc
      }
    }

    def resolve(nodes: List[Node], acc: List[Node] = List.empty) : List[Node] = {
      nodes match {
        case Enum(_, _) :: Scope(inside) :: Single(name) :: ts => {
          resolve(ts, Enum(name.getText, Scope(inside)) :: acc)
        }
        case Struct(structname, _) :: Scope(inside) :: Single(name) :: ts if structname == "[unresolved]" => {
          resolve(ts, Struct(name.getText, Scope(inside)) :: acc)
        }
        case Struct(name, _) :: Scope(inside) :: ts => {
          resolve(ts, Struct(name, Scope(inside)) :: acc)
        }
        case t :: ts => resolve(ts, t :: acc)
        case _ => acc
      }
    }

    def computeSums(nodes: List[Node], acc: List[Node] = List.empty) : List[Node] = {
      nodes match {
        case Number(a) :: Single(op) :: Number(b) :: ts if isPlus(op) => {
          computeSums(ts, Number(a + b) :: acc)
        }
        case Scope(inside) :: ts => {
          computeSums(ts, Scope(computeSums(inside.toList).reverse) :: acc)
        }
        case a :: ts => {
          computeSums(ts, a :: acc)
        }
        case _ => acc
      }
    }

    resolve(
      computeSums(
        parse(tokens).reverse
      ).reverse
    ).reverse
  }

  def generate(src : String) : Unit = {
    val preprocessor = new Preprocessor()
    preprocessor.addFeature(Feature.DEBUG)
    preprocessor.addWarning(Warning.IMPORT)
    preprocessor.addMacro("CURL_ISOCPP")

    def toVec(f: Unit => Token): List[Token] = {
      @tailrec
      def seq(acc : List[Token]) : List[Token] = {
        val tok = f()
        if (tok == null || tok.getType == Token.EOF)
          acc
        else
          seq(tok :: acc)
      }

      seq(List.empty).reverse
    }

    preprocessor.addInput(new StringLexerSource(src, true))
    val tokens = toVec(_ => preprocessor.token()).filter(t => {
      val tp = t.getType
      tp != Token.WHITESPACE && tp != Token.NL && tp != Token.CCOMMENT && tp != Token.CPPCOMMENT
    })

    val enums = parseNodes(tokens).filter(t => t.isInstanceOf[Enum] )

    enums.foreach({
      case Enum(name, inside) => {
        println(s"enum $name {")
        inside.asInstanceOf[Scope].ts.foreach({
          case Single(t) if t.getText == "," => print("\n")
          case Single(t) => print(t.getText + " ")
          case Number(n) => print("Int("+ n.toString + ") ")
        })
        println("\n}")
      }
      case _ =>
    })
  }
}
