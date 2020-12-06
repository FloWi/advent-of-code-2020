import java.nio.file.Paths
import java.io.File
import scala.io.Source
import scala.io.BufferedSource
object Helper {

  def getCallingMainClass: Class[_] = {
    val className = Thread.currentThread().getStackTrace().last.getClassName()
    Class.forName(className)
  }

  def resourceFileByCurrentDayPackage(clazz: Class[_]): File = {
    println(clazz)
    val resourceNameBasedOnPackage =
      s"/${clazz.getPackageName}.txt"
    println(resourceNameBasedOnPackage)

    Paths
      .get(clazz.getResource(resourceNameBasedOnPackage).toURI())
      .toFile()
  }

  def source(
      maybeFilename: Option[String]
  ): BufferedSource = {

    val filename = maybeFilename.getOrElse(
      resourceFileByCurrentDayPackage(getCallingMainClass).getAbsolutePath()
    )

    val file = Paths.get(filename).toFile()

    if (!file.exists()) {
      throw new IllegalArgumentException(s"file $file doesn't exist")
    }

    Source
      .fromFile(file)
  }
}
