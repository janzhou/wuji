package wuji.lib.ops

import wuji._

object And extends SFun(List("and", "与", "并", "并且", "且"), List(
    SSymbol("target")
)) {
    def fun(args:SArgs, env:SEnv):SObject = {
        var caller = args.caller match {
            case bool:SBoolean.SBoolean => bool.value
            case o:SExp => throw RuntimeException(o.location, "parameter type error")
        }

        args.name(params(0)).get.value match {
            case bool:SBoolean.SBoolean => SBoolean(bool.location, bool.value && caller)
            case o:SExp => throw RuntimeException(o.location, "parameter type error")
        }
    }
}