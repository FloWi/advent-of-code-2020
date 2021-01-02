package helper

import scala.io.{BufferedSource, Source}

object Helper {

  def getCallingMainClass: Class[_] = {

    val stacktrace = Thread
      .currentThread()
      .getStackTrace

    val className = stacktrace
      .find { t =>
        val name = t.getClassName
        name.startsWith("day") && name.contains("part") && !name.contains("$")
      }
      .get
      .getClassName
    Class.forName(className)
  }

  def resourceFileByCurrentDayPackage(clazz: Class[_]): BufferedSource = {
    println(clazz)
    val resourceNameBasedOnPackage =
      s"/${clazz.getPackageName}.txt"

    val url = clazz.getResource(resourceNameBasedOnPackage)
    println(s"name: $resourceNameBasedOnPackage; url: $url")

    Source.fromURL(url)
  }

  def source(
      maybeFilename: Option[String]
  ): BufferedSource = {

    maybeFilename match {
      case Some(filename) =>
        Source.fromFile(filename)
      case None =>
        resourceFileByCurrentDayPackage(getCallingMainClass)
    }

  }
}
