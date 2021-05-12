package git

import ammonite.ops._

import java.time.{ZoneId, ZonedDateTime}
import java.time.format.DateTimeFormatter
import java.util.Locale
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

case class GitProject(path: Path) {

  def log() = new GitLogBuilder(this)
  def pull() = %("git", "pull")(path)

}

case class Commit(id: String,
                  author: Option[String]=None,
                  autorDate: Option[ZonedDateTime] = None,
                  committer: Option[String] = None,
                  commitDate: Option[ZonedDateTime] = None,
                  merge: List[String] = Nil,
                  messgae: Option[String] = None,
                  changes: List[DiffStatus] = Nil)
case class DiffStatus(path: String, add: Int, remove: Int)


class GitLogBuilder(project: GitProject) {

  abstract class CommandOption {
    GitLogBuilder.this.options += this
    def getCommandParts(): List[String]
  }
  class CommandOption0(longOption: String) extends CommandOption {
    var set: Option[Unit] = None
    def apply() = {
      this.set = Some(())
      GitLogBuilder.this
    }
    override def getCommandParts(): List[String] = set.map(it=>s"--$longOption").toList
  }
  class CommandOption1[T](longOption:String) extends CommandOption {
    var value: Option[T] = None
    def apply(value: T) = {
      this.value = Some(value)
      GitLogBuilder.this
    }
    override def getCommandParts(): List[String] = value.map(it=>s"--$longOption=$it").toList
  }
  class CommandOptionN[T](longOption:String) extends CommandOption {
    var value: Seq[T] = Seq.empty
    def apply(value: T*) = {
      this.value = value
      GitLogBuilder.this
    }
    override def getCommandParts(): List[String] = value.map(it=>s"--$longOption=$it").toList
  }

  private val options = ListBuffer[CommandOption]()

  val limit = new CommandOption1[Int]("max-count")
  val skip = new CommandOption1[Int]("skip")
  val since = new CommandOption1[ZonedDateTime]("since")
  val until = new CommandOption1[ZonedDateTime]("until")
  val authors = new CommandOptionN[String]("author")
  val committers = new CommandOptionN[String]("committer")
  val grep = new CommandOptionN[String]("grep")
  val minParents = new CommandOption1[Int]("min-parents")
  val maxParents = new CommandOption1[Int]("max-parents")
  val all = new CommandOption0("all")
  val branches = new CommandOption1[String]("branches")
  val tags = new CommandOption1[String]("tags")
  val remotes = new CommandOption1[String]("remotes")
  val excludes = new CommandOptionN[String]("exclude")
  val numstat = new CommandOption0("numstat")
  val format = new CommandOption1[String]("format")

  private var _merges: Option[Boolean] = None
  private var _paths: List[String] = Nil

  def merges(flag: Boolean) = { this._merges = Some(flag); this }

  private def buildCommand(): Vector[String] = {
    val command = scala.collection.mutable.ListBuffer[String]()

    command ++= List("git", "log")
    options.foreach { option =>
      command ++= option.getCommandParts()
    }

    _merges.map { flag => val it  = if(flag) "--merges" else "--no-merges"; command += it }
    command.toVector
  }

  def exec(): String = {
    val command = Command( buildCommand(), Map.empty, null )
    val result = Shellout.executeStream(project.path, command)
    result.out.string
  }

  private def parseLog(log: String): List[Commit] = {

    val lines = log.split(System.getProperty("line.separator")).toList
    parseLog(lines, Nil, None)
  }

  object GitLogPatterns {
    val LINE_COMMIT = """^commit ([0-9a-fA-F]+)$""".r
    val LINE_AUTHOR = """^Author:\s*(.*)$""".r
    val LINE_AUTHOR_DATE = """^AuthorDate: (.*)$""".r
    val LINE_COMMITTER = """^Commit:\s*(.*)$""".r
    val LINE_COMMITTER_DATE = """^CommitDate: (.*)$""".r
    val LINE_MERGE = """^Merge: (.*)$""".r

    val LINE_EMPTY = "^$".r
    val LINE_MSG = """^\s{4}(.*)$""".r
    val LINE_FILE = """^(.*?)\t(.*?)\t(.*)$""".r
  }

  @tailrec
  private def parseLog(log: List[String], aggr: List[Commit], current: Option[Commit]): List[Commit] = {
    import GitLogPatterns._
    log match {
      case LINE_COMMIT(commit) :: tail =>
        val aggr2 = if(current.nonEmpty) current.get :: aggr else aggr
        val current2 = Commit(id = commit)
        parseLog(tail, aggr2, Some(current2))
      case LINE_AUTHOR(author) :: tail =>
        parseLog(tail, aggr, current.map { c => c.copy(author = Some(author))})
      case LINE_AUTHOR_DATE(date) :: tail =>
        parseLog(tail, aggr, current.map { c=> c.copy(autorDate = Some(parseDate(date))) })
      case LINE_COMMITTER(author) :: tail =>
        parseLog(tail, aggr, current.map { c => c.copy(committer = Some(author))})
      case LINE_COMMITTER_DATE(date) :: tail =>
        parseLog(tail, aggr, current.map { c=> c.copy(commitDate = Some(parseDate(date))) })
      case LINE_MERGE(merge) :: tail =>
        parseLog(tail, aggr, current.map { c => c.copy(merge = parseMerge(merge)) })
      case LINE_EMPTY() :: tail =>
        parseLog(tail, aggr, current)
      case LINE_MSG(msg) :: tail =>
        parseLog(tail, aggr, current.map { c => c.copy( messgae =
          if(c.messgae == None) Some(msg) else Some(c.messgae.get + "\n" + msg) )
        })
      case LINE_FILE(add, remove, path) :: tail =>
        parseLog(tail, aggr, current.map { c => c.copy(changes = parseChange(add, remove, path) :: c.changes)})
      case h@_ :: tail =>
        println(s"!! unknown ${h}")
        parseLog(tail, aggr, current)
      case _ =>
        if(current.nonEmpty) (current.get :: aggr).reverse
        else aggr.reverse
    }

  }

  // Sat Aug 19 20:34:28 2017 +0800
  // EEE MMM dd HH:mm:ss yyyy Z
  private def parseDate(dateStr: String): ZonedDateTime = {
    val formatter = DateTimeFormatter.ofPattern("EEE MMM d HH:mm:ss yyyy Z").withLocale(Locale.US).withZone(ZoneId.systemDefault())
    ZonedDateTime.parse(dateStr, formatter)
  }
  private def parseMerge(mergeStr: String) : List[String] = {
    mergeStr.split("\\s").toList
  }
  private def parseChange(add: String, remove: String, path: String): DiffStatus = {
    def convertToNum(text: String): Int = {
      val numPattern = """(\d+)""".r
      text match {
        case "-" => 0
        case numPattern(num) => num.toInt
        case _ =>
              println("!! format $text")
              0
      }
    }
    DiffStatus(path, convertToNum(add), convertToNum(remove))
  }

  def getCommits(): List[Commit] ={
    val display = this.format("fuller").numstat()
      .exec()
    parseLog(display)
  }

}
