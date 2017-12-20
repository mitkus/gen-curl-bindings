package com.lightbend.experiments

import org.anarres.cpp._
import scala.annotation.tailrec

object Generator {

  private def id(name: String)(t: Token) = {
    t != null && t.getType == Token.IDENTIFIER && t.getText == name
  }

  private def tok(name: String)(t: Token) = {
    t != null && t.getText == name
  }

  private val isTypedef = id("typedef") _
  private val isEnum = id("enum") _
  private val isStruct = id("struct") _
  private val isOpenBrace = tok("{") _
  private val isCloseBrace = tok("}") _

  // Higher level token concept with hierarchy and some semantics
  sealed trait MetaToken
  case class Single(t: Token) extends MetaToken
  case class Enum(name: String) extends MetaToken
  case class Struct(name: String) extends MetaToken
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
          itr(ts, Enum("[unresolved]") :: acc)
        }
        case a :: b :: ts if isTypedef(a) && isStruct(b) => {
          itr(ts, Struct("[unresolved]") :: acc)
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

    itr(tokens, List.empty).reverse
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

    val metatokens = parseMetatokens(tokens.toList)

    println(metatokens.head.toString)
  }
}
