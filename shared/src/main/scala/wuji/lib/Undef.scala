package wuji.lib

import wuji._

object Undefobj extends SFun(List("undefobj", "undef-obj", "销毁", "删除"), List(
    SSymbol("name")
)) {
    def fun(args:SArgs, env:SEnv):SObject = {
        val caller = args.caller
        val symbol = args.name(params(0)).get.symbol

        caller.undef(symbol)

        SNil(symbol.location)
    }
}