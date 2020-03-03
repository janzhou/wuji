package wuji.lib

import wuji._

object Defun extends SFun(List("defun", "def-obj", "有法"), List(
    SSymbol("names"), SSymbol("params"), SSymbol("exp")
)) {
    def fun(args:SArgs, env:SEnv):SObject = {
        val caller = args.caller

        val fun_names = args.name(params(0)).get.exp match {
            case exp:SList => exp.values.map(_ match {
                case exp:SSymbol => exp.value
                case exp:SExp => throw RuntimeException(exp.location, "name expected")
            })
            case exp:SSymbol => List(exp.value)
            case exp:SExp => throw RuntimeException(exp.location, "name expected")
        }

        val fun_params = args.name(params(1)).get.exp match {
            case exp:SList => exp.values.map(_ match {
                case exp:SSymbol => exp
                case exp:SExp => throw RuntimeException(exp.location, "param expected")
            })
            case exp:SSymbol => List(exp)
            case exp:SExp => throw RuntimeException(exp.location, "param expected")
        }

        val fun_exp = args.name(params(2)).get.exp match {
            case exp:SList => exp
            case exp:SExp => throw RuntimeException(exp.location, "fun exp expected")
        }

        val fun = SExpFun(fun_names, fun_params, fun_exp)
        caller.funs(fun)
        fun
    }
}

case class SExpFun(override val names:List[String], override val params:List[SSymbol], val exp:SList) extends SFun(names, params) {
  def fun(args:SArgs, env:SEnv):SObject = {
    val caller = args.caller

    if(exp.values.length == 0) SNil(exp.location)
    else exp.values.map(_ match {
      case exp:SExp => {
        caller.prepare_and_call(exp, args, env)
      }
      case obj:SObject => obj
    }).last
  }
}