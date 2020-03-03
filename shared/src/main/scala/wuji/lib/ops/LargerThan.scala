package wuji.lib.ops

import wuji._

object LargerThan extends SFun(List("larger-than", "greater-than", "大于", "超过", "多于"), List(
    SSymbol("target")
)) {
    def fun(args:SArgs, env:SEnv):SObject = {
        val a = args.caller
        val b = args.name(params(0)).get.value

        val result = {
            a match {
                case a:SInt.SInt => {
                    b match {
                        case b:SInt.SInt => a.value > b.value
                        case b:SFloat.SFloat => a.value > b.value
                        case o:SObject => throw RuntimeException(o.location, "parameter type error")
                    }
                }
                case a:SFloat.SFloat => {
                    b match {
                        case b:SInt.SInt => a.value > b.value
                        case b:SFloat.SFloat => a.value > b.value
                        case o:SObject => throw RuntimeException(o.location, "parameter type error")
                    }
                }
                case o:SObject => throw RuntimeException(o.location, "parameter type error")
            }
        }

        SBoolean(b.location, result)
    }
}