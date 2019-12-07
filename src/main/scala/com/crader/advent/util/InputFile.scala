package com.crader.advent.util

import java.io.FileNotFoundException

import scala.collection.Factory
import scala.io.Source
import scala.util.{Failure, Try, Using}

object InputFile {
  def asCollection[T, C[_]](resourcePath: String)
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
}
