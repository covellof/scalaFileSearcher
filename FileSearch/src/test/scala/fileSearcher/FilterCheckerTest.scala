package fileSearcher

import java.io.File

import org.scalatest._


class FilterCheckerTest extends FlatSpec {
  "FilterChecker passed a list where one file matches the filter" should
    "return a list with that file" in {
    val listOfFiles = List(FileObject(new File("random")), FileObject(new File("match")))
    val matchedFiles = FilterChecker("match") findMatchedFiles listOfFiles
    assert(matchedFiles == List(FileObject(new File("match"))))
  }

  "FilterChecker passed a list with a directory that matches the filter" should
    "should not return the directory" in {
    val listOfObjects = List(DirectoryObject(new File("random")), DirectoryObject(new File("match")))
    val matchedFiles = FilterChecker("match") findMatchedFiles listOfObjects
    assert(matchedFiles.length == 0)
  }

  "FilterChecker passed a file with content that matches the filter" should
  "return that the match is 1" in {
    val contentMatch = FilterChecker("sbt.version").findMatchedContentCount(new File("./project/build.properties"))
    assert(contentMatch == 1)
  }

  "FilterChecker passed a file with content that doesn't match the filter" should
    "return that the match is 0" in {
    val contentMatch = FilterChecker("no-match").findMatchedContentCount(new File("./project/build.properties"))
    assert(contentMatch == 0)
  }

}
