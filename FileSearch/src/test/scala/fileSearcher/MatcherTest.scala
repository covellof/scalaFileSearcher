package fileSearcher

import java.io.File

import org.scalatest._

class MatcherTest extends FlatSpec {

  "Matcher that is passed a file mathing the filter" should
  "return a list with that file name" in {
    val matcher = new Matcher("fake", "fakePath")
    val results = matcher.execute

    assert(results == List(("fakePath", None)))
  }

  "Matcher using a directory containing one file matching the filter" should
  "return a list with that file name" in {
    val matcher = new Matcher("properties", new File("project").getCanonicalPath())
    val result = matcher.execute

    assert(result == List(("build.properties", None)))
  }

  "Matcher that is not passed a root file location" should
  "search in current location" in {
    val matcher = new Matcher("filter")
    assert(matcher.rootLocation == new File(".").getCanonicalPath())
  }

  "Matcher using a directory containing more one file matching the filter in a subfolder" should
    "return a list with that file name" in {
    val searchSubFolders = true
    val matcher = new Matcher("iml", new File(".idea").getCanonicalPath(), searchSubFolders)
    val result = matcher.execute

    assert(result == List(("FileSearch-build.iml", None),("FileSearch.iml", None)))
  }

  "Matcher given a path that has one file that matches file filter and content filter" should
  "return a list with that file name" in {
    val matcher = new Matcher("build",
        new File(".").getCanonicalPath(),
      true,
        Some("sbt.version"))
    val matchedFiles = matcher.execute

    assert(matchedFiles == List(("build.properties", Some(1))))
  }

  "Matcher given a path that has no file that matches file filter and content filter" should
    "return an empty list" in {
    val matcher = new Matcher("txt",
        new File(".").getCanonicalPath(),
      true,
        Some("content-not-found"))
    val matchedFiles = matcher.execute

    assert(matchedFiles == List())
  }

}
