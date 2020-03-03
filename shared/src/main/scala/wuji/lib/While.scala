package wuji.lib

import wuji._

object While extends SFun(List("while"), List(
    SSymbol("clause"), SSymbol("loop")
)) {
    def fun(args:SArgs, e:SEnv):SObject = {
        val caller = args.caller
        val env = SEnv(args.args(0).env)
        val clause = args.name(params(0)).get.exp
        val loop = args.name(params(1)).get.exp

        var continue_loop:Boolean = {
            clause match {
                case ls:SList => {
                    if(ls.length == 0) true
                    else {
                        ls.values.map(_ match {
                            case exp:SExp => caller.prepare_and_call(exp, args, env)
                            case any:SAny => any
                        }).last match {
                            case b:SBoolean.SBoolean => b.value
                            case o:SObject => throw RuntimeException(o.location, "expected bool")
                        }
                    }
                }
                case o:SObject => throw RuntimeException(o.location, "expedted list")
            }
        }

        while(continue_loop) {
            continue_loop = {
                loop match {
                    case ls:SList => {
                        if(ls.length == 0) true
                        else {
                            ls.values.map(_ match {
                                case exp:SExp => caller.prepare_and_call(exp, args, env)
                                case any:SAny => any
                            }).last match {
                                case b:SBoolean.SBoolean => b.value
                                case o:SObject => throw RuntimeException(o.location, "expected bool")
                            }
                        }
                    }
                    case o:SObject => throw RuntimeException(o.location, "expedted list")
                }
            }
        }

        SNil(loop.location)
    }
}