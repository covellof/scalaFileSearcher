package fileSearcher

import java.io.File

class Matcher(val filter: String,
              val rootLocation: String = new File(".").getCanonicalPath(),
              checkSubfolders: Boolean = false,
              contentFilter: Option[String] = None) {

  val rootCheckedObject = FileConverter.convertToCheckedObject(new File(rootLocation))

  def execute = {

    def recursiveMatch(files: List[CheckedObject], currentList: List[FileObject]): List[FileObject] =
      files match {
        case List() => currentList
        case checkedObject :: rest =>
          checkedObject match {
            case file: FileObject if FilterChecker(filter) matches (file.name) =>
              recursiveMatch(rest, file :: currentList)
            case directory: DirectoryObject =>
              recursiveMatch(rest ::: directory.children(), currentList)
            case _ => recursiveMatch(rest, currentList)
          }
      }

    val matchedFiles = rootCheckedObject match {
      case file: FileObject if FilterChecker(filter) matches file.name => List(file)
      case directory: DirectoryObject =>
        if (checkSubfolders) recursiveMatch(directory.children(), List())
        else FilterChecker(filter) findMatchedFiles directory.children()
      case _ => List()
    }

    val filteredContent = contentFilter match {
      case Some(dataFilter) =>
        matchedFiles.map(checkedObject =>
          (checkedObject, Some(FilterChecker(dataFilter).findMatchedContentCount(checkedObject.file))))
          .filter(matchTuple => matchTuple._2.get > 0)
      case None => matchedFiles map (checkedObject => (checkedObject, None))
    }

    filteredContent map { case (checkedObject, count) => (checkedObject.name, count) }
  }


}

