import java.time.ZonedDateTime

package object git {

  implicit class TimestampEx(sc: StringContext){

    def ts(args: Any*): ZonedDateTime = {
      val s = sc.s(args:_*)
      ZonedDateTime.parse(s)
    }

  }
}
