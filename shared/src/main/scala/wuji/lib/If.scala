package wuji.lib

import wuji._

object If extends SFun(List("if", "若", "如", "倘", "如果", "假如", "假使"), List(
    SSymbol("clause"), SSymbol("true"), SSymbol("false")
)) {
    def fun(args:SArgs, env:SEnv):SObject = {
        args.name(params(0)).get.value match {
            case condition:SBoolean.SBoolean => {
                if(condition.value) args.name(params(1)).get.value 
                else args.name(params(2)).get.value
            }
            case o:SObject => throw RuntimeException(o.location, "if condition type error")
        }
    }
}