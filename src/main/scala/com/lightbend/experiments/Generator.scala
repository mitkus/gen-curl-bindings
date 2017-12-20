package com.lightbend.experiments

import org.anarres.cpp._
import scala.annotation.tailrec

object Generator {

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

  // Higher level token concept with hierarchy and some semantics
  sealed trait MetaToken
  case class Empty() extends MetaToken
  case class Single(t: Token) extends MetaToken
  case class Enum(name: String, inside: MetaToken) extends MetaToken
  case class Struct(name: String, inside: MetaToken) extends MetaToken
  case class Scope(ts: Seq[MetaToken]) extends MetaToken

  def parseMetatokens(tokens: List[Token]) : List[MetaToken] = {
    def parseScope(tokens: List[Token], acc: List[MetaToken]) : (Scope, List[Token]) = {
      tokens match {
        case a :: ts if isCloseBrace(a) => (Scope(acc.reverse), ts)
        case a :: ts if isOpenBrace(a) => {
          val (scope, rest) = parseScope(ts, List.empty)
          parseScope(rest, scope :: acc)
        }
        case a :: ts => {
          parseScope(ts, Single(a) :: acc)
        }
        case _ => throw new Exception("Malformed scope")
      }
    }

    def itr(tokens: List[Token], acc: List[MetaToken]) : List[MetaToken] = {
      tokens match {
        case a :: b :: ts if isTypedef(a) && isEnum(b) => {
          itr(ts, Enum("[unresolved]", Empty()) :: acc)
        }
        case a :: b :: ts if isTypedef(a) && isStruct(b) => {
          itr(ts, Struct("[unresolved]", Empty()) :: acc)
        }
        case a :: b :: ts if isStruct(a) && isId(b) => {
          itr(ts, Struct(b.getText, Empty()) :: acc)
        }
        case a :: ts if isOpenBrace(a) => {
          val (scope, rest) = parseScope(ts, List.empty)
          itr(rest, scope :: acc)
        }
        case a :: ts => {
          itr(ts, Single(a) :: acc)
        }
        case _ => acc
      }
    }

    def resolve(metatokens: List[MetaToken]) : List[MetaToken] = {
      metatokens match {
        case Enum(_, _) :: Scope(inside) :: Single(name) :: ts => {
          Enum(name.getText, Scope(inside)) :: resolve(ts)
        }
        case Struct(structname, _) :: Scope(inside) :: Single(name) :: ts if structname == "[unresolved]" => {
          Struct(name.getText, Scope(inside)) :: resolve(ts)
        }
        case Struct(name, _) :: Scope(inside) :: ts => {
          Struct(name, Scope(inside)) :: resolve(ts)
        }
        case t :: ts => t :: resolve(ts)
        case _ => List.empty
      }
    }

    resolve(itr(tokens, List.empty).reverse).reverse
  }

  def generate(src : String) : Unit = {
    val preprocessor = new Preprocessor()
    preprocessor.addFeature(Feature.DEBUG)
    preprocessor.addWarning(Warning.IMPORT)
    preprocessor.addMacro("CURL_ISOCPP")

    def toVec(f: Unit => Token): Vector[Token] = {
      @tailrec
      def seq(acc : Vector[Token]) : Vector[Token] = {
        val tok = f()
        if (tok == null || tok.getType == Token.EOF)
          acc
        else
          seq(acc :+ tok)
      }

      seq(Vector.empty)
    }

    preprocessor.addInput(new StringLexerSource(src, true))
    val tokens = toVec(_ => preprocessor.token()).filter(t => {
      val tp = t.getType
      tp != Token.WHITESPACE && tp != Token.NL && tp != Token.CCOMMENT && tp != Token.CPPCOMMENT
    })

    val enums = parseMetatokens(tokens.toList).filter( t => t.isInstanceOf[Enum] )

    enums.foreach({
      case Enum(name, inside) => {
        println(s"enum $name {")
        inside.asInstanceOf[Scope].ts.foreach({
          case Single(t) if t.getText == "," => print("\n")
          case Single(t) => print(t.getText + " ")
        })
        println("\n}")
      }
      case _ =>
    })

  }
}
