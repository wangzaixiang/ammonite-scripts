package fileop

import java.nio.ByteBuffer

/**
 * a file tail util
 *
 * usage:
 * ```scala
 *    tail(file, tail.FromEnd)(println)
 * ```
 */
object tail {

  import ammonite.ops._

  import java.io.RandomAccessFile
  import scala.annotation.tailrec

  sealed trait TailMode
  object FromBegin extends TailMode
  object FromEnd extends TailMode

  sealed trait TailEvent
  case class Line(content: String) extends TailEvent
  case class LineWithoutEOL(content: String) extends TailEvent
  /**
   * after some tail event and without more lines, emit a Pause event for some process
   */
  object Pause extends TailEvent
  object WrapFromZero extends TailEvent

  private val MAX_LINE_LENGTH = 16384
  private val TAIL_SLEEP_PERIOD = 200 // 200ms

//  def main(args: Array[String]): Unit = {
//    println(s"pwd = $pwd")
//    tail(pwd / "test.txt")(print)
//  }

  def apply(path: Path, mode: TailMode = FromEnd)(callback: TailEvent => Unit): Unit = {

    val file = path.toIO
    val length = file.length()

    mode match {
      case FromBegin =>
        tailLoop(file, 0L, 0L, false)(callback)
      case FromEnd =>
        tailLoop(file, length, 0L, false)(callback)
    }


  }

  @tailrec
  private def tailLoop(file: java.io.File, offset: Long, lastLength: Long, pauseEnabled: Boolean)
                      (callback: TailEvent => Unit): Unit = {
    val length = file.length()

    if(lastLength == length) {
      Thread.sleep(TAIL_SLEEP_PERIOD)
      tailLoop(file, offset, length, pauseEnabled)(callback)
    }
    else if(length == offset) {
      Thread.sleep(TAIL_SLEEP_PERIOD)
      if(pauseEnabled) {
        callback(Pause)
        tailLoop(file, offset, length, false)(callback)
      }
      else
        tailLoop(file, offset, length, false)(callback)
    }
    else if (length < offset) {  // rewrap the file
      callback(WrapFromZero)
      tailLoop(file, 0L, length, false)(callback)
    }
    else {
      val position = readLines(file, offset, length)(callback)
      if (position == offset) // consume nothing, waiting a tick
        Thread.sleep(TAIL_SLEEP_PERIOD)

      tailLoop(file, position, length, true)(callback)  // force
    }
  }

  /**
   * readLines from position begin to end(exclusive) and emit the Lines as TailEvent.
   * return the position to be continue process(maybe a start-of-line if the line without EOL)
   */
  private def readLines(file: java.io.File, begin: Long, end: Long)
               (callback: TailEvent => Unit): Long = {

    val buffer = new Array[Byte](MAX_LINE_LENGTH)
    val raf = new RandomAccessFile(file, "r")
    try {
      raf.seek(begin)

      var cursor = begin
      var bufferPos = 0

      while (cursor < end) {
        val max = math.min(buffer.length - bufferPos, (end - cursor).toInt)
        val count = raf.read(buffer, bufferPos, max)
        bufferPos += count
        cursor += count

        val byteBuffer = ByteBuffer.wrap(buffer, 0, bufferPos)
        emitLines(byteBuffer)(callback)

        if (byteBuffer.hasRemaining) {
          if(byteBuffer.remaining() == buffer.length) { // the entire buffer without EOL
            callback.apply(LineWithoutEOL(new String(buffer)))
            bufferPos = 0
          }
          else {
            bufferPos = byteBuffer.remaining()
            byteBuffer.get(buffer, 0, bufferPos)
          }
        } else {
          bufferPos = 0
        }
      }
      cursor - bufferPos
    }
    finally {
      raf.close()
    }
  }

  /**
   * return the remains on buffer
   */
  @tailrec
  private def emitLines(buffer: ByteBuffer)(callback: TailEvent=>Unit): Unit = {
    val eolPos = findEol(buffer, buffer.position())
    eolPos match {
      case None =>
      case Some(pos) =>
        val lineBytes = new Array[Byte](pos+1 - buffer.position())
        buffer.get(lineBytes)
        // remove tail \r\n
        val c_n = lineBytes(lineBytes.length-1) == '\n'
        val c_r = c_n && lineBytes.length >=2 && lineBytes(lineBytes.length-2) == '\r'
        val strLen = lineBytes.length - (if(c_n) 1 else 0) - (if(c_r) 1 else 0)

        callback.apply( Line(new String(lineBytes,0, strLen) ))
        if(buffer.hasRemaining)
          emitLines(buffer)(callback)
    }
  }

  @tailrec
  private def findEol(buffer: ByteBuffer, from: Int): Option[Int] = {
    if(from < buffer.limit()){
      if(buffer.get(from) == '\n') Some(from)
      else findEol(buffer, from+1)
    }
    else None
  }

}
