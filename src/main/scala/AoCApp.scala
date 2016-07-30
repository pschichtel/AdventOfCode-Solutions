import scala.io.Source

/**
  * Created by phillip on 13.01.16.
  */
trait AoCApp extends App {
    def sourceFromCP(path: String): Source = Source.fromInputStream(getClass.getResourceAsStream(path))

    lazy val inputSource = sourceFromCP("/" + this.getClass.getName.replace("$", "").replaceAll("\\.", "/") + ".txt")
    def inputLines = inputSource.getLines().map(_.trim).filter(_.nonEmpty).toSeq
}
