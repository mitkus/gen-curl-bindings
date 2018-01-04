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
  case class EnumDef(name: String, value: Int) extends Node
  case class Struct(name: String, inside: Node) extends Node
  case class Scope(ts: Seq[Node]) extends Node

  private def parseNodes(tokens: List[Token]) : List[Node] = {

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

  private def parseEnum(enum: Enum) : List[EnumDef] = {
    def isCommaNode(n: Node) : Boolean = {
      n match {
        case Single(t) => t.getText == ","
        case _ => false
      }
    }
    def eatComma(nodes: List[Node]) = {
      nodes match {
        case a :: as if isCommaNode(a) => as
        case _ => nodes
      }
    }
    def parseDef(nodes: List[Node], nextVal: Int) : (EnumDef, List[Node], Int) = {
      val (definition, rest) = nodes.span(!isCommaNode(_))

      assert(rest.isEmpty || isCommaNode(rest.head))

      definition match {
        case Single(name) :: Single(eq) :: Number(value) :: ts => {
          (EnumDef(name.getText, value), eatComma(rest), value+1)
        }
        case Single(name) :: ts => {
          (EnumDef(name.getText, nextVal), eatComma(rest), nextVal+1)
        }
        case _ => throw new Exception("Malformed enum def")
      }
    }

    var nodes = enum.inside.asInstanceOf[Scope].ts
    var nextVal = 0
    var defs = List.empty[EnumDef]

    while(nodes.nonEmpty) {
      val (definition, rest, newVal) = parseDef(nodes.toList, nextVal)
      nodes = rest
      nextVal = newVal
      defs = definition :: defs
    }

    defs.reverse
  }

  def nodes(src : String) : List[Node] = {
    val preprocessor = new Preprocessor()
    preprocessor.addFeature(Feature.DEBUG)
    preprocessor.addWarning(Warning.IMPORT)
    preprocessor.addMacro("CURL_ISOCPP")

    def toVec(f: Unit => Token): List[Token] = {
      @tailrec
      def seq(acc: List[Token]): List[Token] = {
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

    parseNodes(tokens)
  }

  def getEnums(nodes : List[Node]) : Map[String, List[EnumDef]] = {
    val enums = nodes.flatMap {
      case n if n.isInstanceOf[Enum] => Some(n.asInstanceOf[Enum])
      case _ => None
    }

    enums.map(enum => {
      (enum.name, parseEnum(enum))
    }).toMap
  }

  def enumBinding(name: String, enums: Map[String, List[EnumDef]]) : List[String] = {
    val defs = enums(name)
    val prefix = List(
      s"class $name(val value: CInt) extends AnyVal",
      s"object $name {"
    )

    prefix ++ defs.map({
      case EnumDef(defname, value) => {
        val shortname = if (defname.startsWith(s"$name"))
          defname.substring(name.length + 1)
        else
          defname

        s"    val $shortname: $name = new $name($value)"
      }
    }) ++ List("}")
  }
}
