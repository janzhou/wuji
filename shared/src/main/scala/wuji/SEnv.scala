package wuji

case class SEnv(override val parent:Option[SEnv]) extends SObject {
    val names = List("env")
    val location = SLocation(0, 0)
}

object SEnv {
    def apply(env:SEnv):SEnv = SEnv(Some(env))
    def apply():SEnv = SEnv(None)
}