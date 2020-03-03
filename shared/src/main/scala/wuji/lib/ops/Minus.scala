package wuji.lib.ops

import wuji._

object Minus extends SFun(List("minus", "sub", "减"), List(
    SSymbol("target")
)) {
    def fun(args:SArgs, env:SEnv):SObject = {
        val a = args.caller
        val b = args.name(params(0)).get.value

        a match {
            case a:SInt.SInt => {
                b match {
                    case b:SInt.SInt => SInt(b.location, a.value - b.value)
                    case b:SFloat.SFloat => SFloat(a.location, a.value - b.value)
                    case o:SObject => throw RuntimeException(o.location, "parameter type error")
                }
            }
            case a:SFloat.SFloat => {
                b match {
                    case b:SInt.SInt => SFloat(b.location, a.value - b.value)
                    case b:SFloat.SFloat => SFloat(a.location, a.value - b.value)
                    case o:SObject => throw RuntimeException(o.location, "parameter type error")
                }
            }
            case o:SObject => throw RuntimeException(o.location, "parameter type error")
        }
    }
}