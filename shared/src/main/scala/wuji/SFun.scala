package wuji

abstract class SFun(val names:List[String], val params:List[SSymbol]) extends SObject {
    val location = SLocation(0, 0)

    override def prepare(exp:SExp, args:SArgs, env:SEnv):SArgs = {
        exp match {
            case ls:SList => {
                if(ls.length == 0) throw RuntimeException(ls.location, "this should not happen")
                val caller = ls.values.head

                val arg0 = SArg(SSymbol("this"), caller, caller, args, env)

                if(ls.length <= params.length) throw RuntimeException(ls.location, "no enouth params")

                val as = (params zip ls.values.tail).map { case (param, exp) => {
                    SArg(param, caller, exp, args, env)
                }}

                SArgs(arg0::as)
            }
            case o:SObject => throw RuntimeException(o.location, "this should not happen")
        }
    }

    override def call(exp:SExp, args:SArgs, env:SEnv):SObject = {
        fun(args, SEnv())
    }

    def fun(args:SArgs, env:SEnv):SObject

    override def toString() = s"(${names.mkString(" ")})"
}