package tyqu_demo

import scala.quoted.*
import tyqu.translators.PostgreSqlTranslator
import tyqu.translators.SqlQuery
import tyqu.execution.PreparedQuery
import tyqu.{*, given}

import Schema.*


object Queries:

  transparent inline def releasesByArtistName = ${ releasesByArtistNameImpl }

  private def releasesByArtistNameImpl(using Quotes): Expr[PreparedQuery[?]] =
    PreparedQuery.createCompileTimeExpr(
      from(Releases).filter(_.artists.exists(_.name === RuntimeParameterExpression[String]("artistName"))),
      PostgreSqlTranslator,
    )
