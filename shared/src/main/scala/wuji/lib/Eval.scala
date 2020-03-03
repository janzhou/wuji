package wuji.lib

import wuji._

object Eval extends SFun(List("eval", "行"), List(
    SSymbol("str")
)) {
    def fun(args:SArgs, env:SEnv):SObject = {
      val caller = args.caller

      args.name(params(0)).get.value match {
          case str:SString.SString => process(caller, str.value, args, env)
          case o:SObject => throw RuntimeException(o.location, s"string expected")
      }
    }

    def process(caller:SObject, str:String, args:SArgs = SArgs(), env:SEnv = SEnv()) = {
      val parser = Parser(str)

      var result:SObject = this
      var exit = false

      while(!exit) {
        parser.next() match {
          case exp:SExit  => exit = true
          case exp:SExp => result = caller.prepare_and_call(exp, args, env)
          case obj:SObject => result = obj
        }
      }

      result
    }
}