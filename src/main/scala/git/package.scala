import java.time.{Duration, LocalTime, Period, ZonedDateTime}

package object git {

  implicit class TimestampEx(sc: StringContext){
    def ts(args: Any*): ZonedDateTime = ZonedDateTime.parse(sc.s(args:_*))
  }
  implicit class PeriodEx(sc: StringContext){
    def period(args: Any*): Period = Period.parse(sc.s(args:_*))
  }
  implicit class DurationEx(sc: StringContext){
    def duration(args: Any*): Duration = Duration.parse(sc.s(args:_*))
  }

  implicit class TimestampLike(time: ZonedDateTime){
    def +(period: Period) : ZonedDateTime = time.plus(period)
    def -(period: Period): ZonedDateTime = time.minus(period)
    def +(duration: Duration): ZonedDateTime = time.plus(duration)
    def -(duration: Duration): ZonedDateTime = time.minus(duration)
  }

  def today: ZonedDateTime = {
    val now = ZonedDateTime.now()
    now.`with`(LocalTime.ofSecondOfDay(0))
  }

}
