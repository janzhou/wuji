package wuji.lib

import wuji._

object Defobj extends SFun(List("defobj", "def-obj", "有", "今有"), List(
    SSymbol("obj"), SSymbol("name")
)) {
    def fun(args:SArgs, env:SEnv):SObject = {
        val caller = args.caller
        val obj = args.name(params(0)).get.value
        val symbol = args.name(params(1)).get.symbol
        caller.define(symbol, obj)
        obj
    }
}