package fileop

import java.nio.ByteBuffer

object TailOp {

  import ammonite.ops._

  import java.io.RandomAccessFile
  import scala.annotation.tailrec

  sealed trait TailMode
  object TailFromHead extends TailMode
  object TailFromEnd extends TailMode

  def main(args: Array[String]): Unit = {
    println(s"pwd = $pwd")
    tail(pwd / "test.txt")(print)
  }

  def tail(path: Path, mode: TailMode = TailFromEnd)(callback: String => Unit): Unit = {

    val file = path.toIO
    val length = file.length()

    mode match {
      case TailFromHead =>
        tailLoop(file, 0L, 0L)(callback)
      case TailFromEnd =>
        tailLoop(file, length, length)(callback)
    }


  }

  @tailrec
  def tailLoop(file: java.io.File, offset: Long, lastLength: Long)(callback: String => Unit): Unit = {
    val length = file.length()

    if(length == lastLength || length == offset) {
      Thread.sleep(200)
      tailLoop(file, offset, lastLength)(callback)
    }
    else if (length < offset) {  // rewrap the file
      tailLoop(file, 0L, 0L)(callback)
    }
    else {
      val available = (length - offset).toInt
      val bufferSize = if (available < 8192) 8192 else 8192
      val buffer = new Array[Byte](bufferSize)

      val position = readLines(file, offset, length, buffer)(callback)
      if (position == offset) // consume nothing, waiting a tick
        Thread.sleep(200)

      tailLoop(file, position, length)(callback)
    }
  }

  private def readLines(file: java.io.File, begin: Long, end: Long, buffer: Array[Byte])
               (callback: String => Unit): Long = {

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

        if (byteBuffer.remaining() == buffer.length)
          byteBuffer.clear()

        if (byteBuffer.hasRemaining) {
          bufferPos = byteBuffer.remaining()
          byteBuffer.get(buffer, 0, bufferPos)
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
  private def emitLines(buffer: ByteBuffer)(callback: String=>Unit): Unit = {
    val eolPos = findEol(buffer)
    eolPos match {
      case None =>
      case Some(pos) =>
        val lineBytes = new Array[Byte](pos+1 - buffer.position())
        buffer.get(lineBytes)
        callback.apply(new String(lineBytes))
        if(buffer.hasRemaining)
          emitLines(buffer)(callback)
    }
  }

  private def findEol(buffer: ByteBuffer): Option[Int] = {
    var pos = buffer.position()
    while(pos < buffer.limit()){
      if(buffer.get(pos) == '\n') return Some(pos)
        pos += 1
    }
    None
  }

}
