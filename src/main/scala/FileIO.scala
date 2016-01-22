import java.io._

class FileIO( file: File ) {

  def write(lines : Iterable[String]) = {
    val out = new PrintWriter( file , "UTF-8")
    try{
      lines.foreach(out.println)
    } finally{
      try {
        out.close()
      } catch {
        case _ : Throwable => //NOOP
      }
    }
  }
}

object FileIO {

  implicit def enrichFile( file: File ): FileIO = new FileIO( file )

}