package wuji

class SDao(val location:SLocation) extends SAny {
  val names = List("dao")
  setSClass(SDao)
}

object SDao extends SClass(List("dao"), List[SSymbol](), None) {
    def apply():SDao = new SDao(SLocation())
    def apply(location:SLocation):SDao = new SDao(location)

    def fun(args:SArgs, env:SEnv):SObject = {
      SDao(args.location)
    }
}