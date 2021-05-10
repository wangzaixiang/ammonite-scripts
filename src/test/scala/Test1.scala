import ammonite.ops._
import git._

import java.time.{ZoneId, ZonedDateTime}
import java.time.format.DateTimeFormatter
import java.util.Locale

object Test1 {

  // TODO need a record-output API, such as screen formatted output, CSV export etc.
  def main(args: Array[String]): Unit = {
    val dir = root/ "Users"/ "walter01.wang"/ "workspaces"/ "xl" / "api"
    val project = GitProject(dir)

    val commits = project.log()
      .since(ts"2021-05-06T00:00:00+08:00")
      .until(ts"2021-05-10T00:00:00+08:00")
      .all()
      // .authors("cuiwei")
      .getCommits()
      .filterNot(c => c.messgae.exists( txt => txt.contains("cherry picked from commit")))

    def adds(commits: List[Commit]) = commits.flatMap(_.changes).map(_.add).sum
    def removes(commits: List[Commit]) = commits.flatMap(_.changes).map(_.remove).sum

    println(s"commits: ${commits.size}, total add:${adds(commits)}  - remove:${removes(commits)} = ${adds(commits)-removes(commits)}")

    println("commits by committers")
    commits.groupBy(_.author).toList.sortBy(0 - _._2.size).foreach { case (author, commits) =>
      println(f"${author.get}%-50s\tcommits:${commits.size}%3d\t\t+${adds(commits)}%5d\t -${removes(commits)}%5d = ${adds(commits)-removes(commits)}%5d")
    }

    // val grid = Column(head="author", width="30", align="left", formatter="", data=f)
    // grid.printHeader
    // grid.printRow

    println()
    println("commits by tapd")
    commits.groupBy(c => tapdItem(c)).toList.sortBy(0 - _._2.size).foreach { case (id, commits) =>
      println(f"${id.getOrElse("<none>")}%-50s\tcommits:${commits.size}%3d\t\t+${adds(commits)}%5d\t -${removes(commits)}%5d = ${adds(commits)-removes(commits)}%5d")
    }

  }

  def tapdItem(commit: Commit): Option[String] = {
    val pattern = """^.*(feat|fix): (\d+)-(?:.*)$""".r
    commit.messgae.get.split(System.getProperty("line.separator")).toList match {
      case pattern(tag, id) :: tail => Some(id)
      case _ => None
    }
  }






}
