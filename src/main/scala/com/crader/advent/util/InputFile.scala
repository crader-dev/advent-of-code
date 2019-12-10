package com.crader.advent.util

import java.io.FileNotFoundException

import scala.collection.Factory
import scala.io.Source
import scala.util.{Failure, Try, Using}

object InputFile {
  
  //TODO: I think the implicit factory usage can be improved
  def fromMultipleLines[T, C[_]](resourcePath: String)
                                (transformer: String => T = identity _)
                                (implicit factory: Factory[T, C[T]]): Try[C[T]] = {
    Option(getClass.getClassLoader.getResourceAsStream(resourcePath)) match {
      case Some(inputStream) =>
        Using(Source.fromInputStream(inputStream)) (
          _.getLines.map(transformer).to(factory)
        )
      case None =>
        Failure(new FileNotFoundException(s"Could not find input file at: .../$resourcePath"))
    }
  }

//  def fromSingleLine[T](resourcePath: String)(transformer: String => T): Try[T] = {
//    fromMultipleLines(resourcePath)(transformer)(Seq.toFactory(String)).flatMap { lines =>
//      if (lines.size != 1) {
//        Failure(new IllegalArgumentException(
//          s"Only expected a single input line, but found ${lines.size} lines in: .../$resourcePath"
//        ))
//      } else {
//        Success(lines.head)
//      }
//    }
//  }
}
