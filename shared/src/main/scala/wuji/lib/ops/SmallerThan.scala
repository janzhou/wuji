package wuji.lib.ops

import wuji._

object SmallerThan extends SFun(List("smaller_than", "less_than", "小于", "不足", "少于"), List(
    SSymbol("target")
)) {
    def fun(args:SArgs, env:SEnv):SObject = {
        val a = args.caller
        val b = args.name(params(0)).get.value

        val result = {
            a match {
                case a:SInt.SInt => {
                    b match {
                        case b:SInt.SInt => a.value < b.value
                        case b:SFloat.SFloat => a.value < b.value
                        case o:SObject => throw RuntimeException(o.location, "parameter type error")
                    }
                }
                case a:SFloat.SFloat => {
                    b match {
                        case b:SInt.SInt => a.value < b.value
                        case b:SFloat.SFloat => a.value < b.value
                        case o:SObject => throw RuntimeException(o.location, "parameter type error")
                    }
                }
                case o:SObject => throw RuntimeException(o.location, "parameter type error")
            }
        }

        SBoolean(b.location, result)
    }
}