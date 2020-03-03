package object wuji {

    abstract class wujiException extends Exception {
        val location : SLocation
        val reason : String

        def message = s"${location.start}-${location.end}: $reason"
    }

    case class ParseException (location:SLocation, reason : String) extends wuji.wujiException

    case class RuntimeException (location:SLocation, reason : String) extends wujiException

    def call(str:String) = {
        lib.Eval.process(SDao(), str)
    }

    def parse(str:String) = {
        wuji.Parser(str).next()
    }
}