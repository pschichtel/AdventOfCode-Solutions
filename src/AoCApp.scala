import scala.io.Source

/**
  * Created by phillip on 13.01.16.
  */
trait AoCApp extends App {
    def sourceFromCP(path: String): Source = Source.fromInputStream(getClass.getResourceAsStream(path))

    lazy val inputSource = sourceFromCP(getClass.getSimpleName + ".txt")
}
