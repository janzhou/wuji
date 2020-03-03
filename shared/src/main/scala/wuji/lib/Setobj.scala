package wuji.lib

import wuji._

object Setobj extends SFun(List("set", "setobj", "set-obj", "更", "更新", "改", "修改", "遇"), List(
    SSymbol("obj"), SSymbol("name")
)) {
    def fun(args:SArgs, env:SEnv):SObject = {
        val caller = args.caller

        val value = args.name(params(0)).get.value

        val symbol = args.name(params(1)).get.symbol

        val env = args.name(params(1)).get.env

        if(env.has(symbol)) {
            env.set(symbol, value)
        } else caller.set(symbol, value)

        value
    }
}