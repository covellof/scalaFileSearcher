package fileSearcher

import java.io.File

object FileConverter {
  def convertToCheckedObject(file: File) =
    if(file.isDirectory()) new DirectoryObject(file)
    else new FileObject(file)

}
