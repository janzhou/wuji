package wuji.lib.ops

import wuji._

object Add extends SFun(List("add", "加", "合", "和", "与", "计", "且"), List(
    SSymbol("target")
)) {
    def fun(args:SArgs, env:SEnv):SObject = {
        val a = args.caller
        val b = args.name(params(0)).get.value

        a match {
            case a:SInt.SInt => {
                b match {
                    case b:SInt.SInt => SInt(b.location, a.value + b.value)
                    case b:SFloat.SFloat => SFloat(b.location, a.value + b.value)
                    case b:SString.SString => SString(b.location, a.value.toString + b.value)
                    case o:SObject => throw RuntimeException(o.location, "parameter type error")
                }
            }
            case a:SFloat.SFloat => {
                b match {
                    case b:SInt.SInt => SFloat(b.location, a.value + b.value)
                    case b:SFloat.SFloat => SFloat(a.location, a.value + b.value)
                    case b:SString.SString => SString(b.location, a.value.toString + b.value)
                    case o:SObject => throw RuntimeException(o.location, "parameter type error")
                }
            }
            case a:SString.SString => {
                b match {
                    case b:SInt.SInt => SString(b.location, a.value + b.value.toString)
                    case b:SFloat.SFloat => SString(a.location, a.value + b.value.toString)
                    case b:SString.SString => SString(b.location, a.value + b.value)
                    case o:SObject => throw RuntimeException(o.location, "parameter type error")
                }
            }
            case o:SObject => throw RuntimeException(o.location, "parameter type error")
        }
    }
}