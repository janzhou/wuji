package wuji.lib.ops

import wuji._

object Equal extends SFun(List("equal", "equal-to", "等于"), List(
    SSymbol("target")
)) {
    def fun(args:SArgs, env:SEnv):SObject = {
        val a = args.caller
        val b = args.name(params(0)).get.value

        val result = {
            a match {
                case a:SBoolean.SBoolean => {
                    b match {
                        case b:SBoolean.SBoolean => a.value == b.value
                        case _ => false
                    }
                }
                case a:SInt.SInt => {
                    b match {
                        case b:SInt.SInt => a.value == b.value
                        case b:SFloat.SFloat => a.value == b.value
                        case o:SObject => throw RuntimeException(o.location, "parameter type error")
                    }
                }
                case a:SFloat.SFloat => {
                    b match {
                        case b:SInt.SInt => a.value == b.value
                        case b:SFloat.SFloat => a.value == b.value
                        case o:SObject => throw RuntimeException(o.location, "parameter type error")
                    }
                }
                case a:SString.SString => {
                    b match {
                        case b:SString.SString => a.value == b.value
                        case _ => false
                    }
                }
                case o:SObject => throw RuntimeException(o.location, "parameter type error")
            }
        }

        SBoolean(b.location, result)
    }
}