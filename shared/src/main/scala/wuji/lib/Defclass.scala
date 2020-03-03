package wuji.lib

import wuji._

object Defclass extends SFun(List("defclass", "def-class", "有类"), List(
    SSymbol("names"), SSymbol("params"), SSymbol("base"), SSymbol("construct")
)) {
    def fun(args:SArgs, env:SEnv):SObject = {
        val caller = args.caller

        val class_names = args.name(params(0)).get.exp match {
            case exp:SList => exp.values.map(_ match {
                case exp:SSymbol => exp.value
                case exp:SExp => throw RuntimeException(exp.location, "name expected")
            })
            case exp:SSymbol => List(exp.value)
            case exp:SExp => throw RuntimeException(exp.location, "name expected")
        }

        val class_params = args.name(params(1)).get.exp match {
            case exp:SList => exp.values.map(_ match {
                case exp:SSymbol => exp
                case exp:SExp => throw RuntimeException(exp.location, "param expected")
            })
            case exp:SSymbol => List(exp)
            case exp:SExp => throw RuntimeException(exp.location, "param expected")
        }

        val base_exp = args.name(params(2)).get.exp match {
            case exp:SList => exp
            case exp:SExp => throw RuntimeException(exp.location, "base exp expected")
        }

        val construct_exp = args.name(params(2)).get.exp match {
            case exp:SList => exp
            case exp:SExp => throw RuntimeException(exp.location, "construct exp expected")
        }

        if(base_exp.length < 2) throw RuntimeException(base_exp.location, "base exp is not a constructor")

        val base_class = SArg(null, caller, base_exp.values(0), args.id(0).args, env).value match {
            case base_class:SClass => base_class
            case o:SObject => throw RuntimeException(o.location, "base class expected")
        }

        val sclass = new SExpClass(class_names, class_params, Some(base_class), base_exp, construct_exp)

        caller.funs(sclass)

        sclass
    }
}

class SExpClass(
    override val names:List[String],
    override val params:List[SSymbol],
    override val parent:Option[SClass],
    val base:SExp,
    val construct:SExp
) extends SClass(names, params, parent) {
    def fun(args:SArgs, env:SEnv):SObject = {
        val pobj = parent.getOrElse(SDao).prepare_and_call(base, args, env)
        pobj.prepare_and_call(construct, args, env)
        pobj.setSClass(this)
        pobj
    }
}