package fileSearcher

import java.io.File

trait CheckedObject {
  val file: File
  val name = file.getName()
}

case class FileObject(file: File) extends CheckedObject

case class DirectoryObject(file: File) extends CheckedObject {
  def children(): List[CheckedObject] =
    try
      file.listFiles().toList map (file => FileConverter.convertToCheckedObject(file))
    catch {
      case _ : NullPointerException => List()
    }
}


