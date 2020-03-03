package wuji.lib.ops

import wuji._

object Or extends SFun(List("or", "或", "或者"), List(
    SSymbol("target")
)) {
    def fun(args:SArgs, env:SEnv):SObject = {
        val caller = args.caller match {
            case bool:SBoolean.SBoolean => bool.value
            case o:SExp => throw RuntimeException(o.location, "parameter type error")
        }

        args.name(params(0)).get.value match {
            case bool:SBoolean.SBoolean => SBoolean(bool.location, caller || bool.value)
            case o:SExp => throw RuntimeException(o.location, "parameter type error")
        }
    }
}