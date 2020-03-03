package wuji.lib

import wuji._

object Letobj extends SFun(List("let", "letobj", "let-obj", "设"), List(
    SSymbol("obj"), SSymbol("name")
)) {
    def fun(args:SArgs, env:SEnv):SObject = {
        val caller = args.caller

        val value = args.name(params(0)).get.value

        val symbol = args.name(params(1)).get.symbol

        val env = args.name(params(1)).get.env

        env.define(symbol, value)

        value
    }
}