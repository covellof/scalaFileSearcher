package fileSearcher

import java.io.File

import scala.util.control.NonFatal

class FilterChecker(filter: String) {

  val filterAsRegex = filter.r

  def matches(content: String) =
    filterAsRegex findFirstMatchIn content match {
      case None => false
      case Some (_) => true
    }

  def findMatchedFiles(checkedObjects: List[CheckedObject]) =
    for (checkedObject <- checkedObjects
         if checkedObject.isInstanceOf[FileObject]
         if matches(checkedObject.name))
      yield checkedObject

  def findMatchedContentCount(file: File) = {
    def getFilteredMatchCount(content: String) =
      (filterAsRegex findAllIn content).length

    import scala.io.Source
    try {
      val fileSource = Source.fromFile(file)
      try
        fileSource.getLines().foldLeft(0)(
          (accumulator, line) => accumulator + getFilteredMatchCount(line)
        )
      catch {
        case NonFatal(_) => 0
      }
      finally {
        fileSource.close()
      }
    } catch {
      case NonFatal(_) => 0
    }
  }

}

object FilterChecker {
  def apply(filter: String) = new FilterChecker(filter)
}

