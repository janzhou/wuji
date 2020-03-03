package wuji

import scala.collection.mutable.Map

abstract class SObject {
  val names:List[String]
  val location:SLocation
  val parent:Option[SObject] = None

  private val definitions:Map[String, SObject] = Map.empty

  def define(symbol:SSymbol, value: SObject): Unit = {
    if(definitions.contains(symbol.value)) {
      throw RuntimeException(symbol.location, s"SSymbol ${symbol.value} already defined before")
    } else definitions.update(symbol.value, value)
  }

  def get(symbol: SSymbol): SObject = {
    definitions.getOrElse(
      symbol.value,
      parent.getOrElse(
        throw RuntimeException(symbol.location, s"SSymbol ${symbol.value} undefined before get")
      ).get(symbol)
    )
  }

  def has(symbol: SSymbol): Boolean = {
    hasLocal(symbol) || parent.map(_.has(symbol)).getOrElse(false)
  }

  def hasLocal(symbol: SSymbol): Boolean = {
    definitions.contains(symbol.value)
  }

  def empty: Unit = definitions.empty

  def set(symbol: SSymbol, value: SObject): Unit = {
    definitions.get(symbol.value) match {
      case Some(previous) => definitions.update(symbol.value, value)
      case None    => {
        parent.getOrElse(
          throw RuntimeException(symbol.location, s"SSymbol ${symbol.value} undefined before set")
        ).set(symbol, value)
      }
    }
  }

  def undef(symbol: SSymbol): Unit = {
    if(definitions.contains(symbol.value)) {
      definitions.remove(symbol.value)
    } else {
      parent.getOrElse(
        throw RuntimeException(symbol.location, s"SSymbol ${symbol.value} undefined before undef")
      ).undef(symbol)
    }
  }

  def funs(funs: SFun*):SObject = {
    funs.foreach(fun => fun.names.foreach(name => definitions.update(name, fun)))
    this
  }

  def setSClass(sclass: SClass): Unit = {
    SClass.names.foreach(name => definitions.update(name, sclass))
  }

  def prepare(exp:SExp, args:SArgs, env:SEnv):SArgs = {
    val arg0 = SArg(SSymbol("this"), this, this, args, env)
    exp match {
        case ls:SList => {
            ls.length match {
                case 0 => SArgs(arg0, SArg(null, this, SNil(ls.location), args, env))
                case _ => {
                  val arg1 = SArg(null, this, ls.values.head, args, env)
                  SArgs(arg0::arg1::ls.values.tail.map(exp => {
                    SArg(null, this, exp, args, env)
                  }))
                }
            }
        }
        case exp:SExp => SArgs(arg0, SArg(null, this, exp, args, env))
    }
  }

  def prepare_and_call(exp:SExp, args:SArgs, env:SEnv):SObject = {
    call(exp, prepare(exp, args, env), env)
  }

  def call(exp:SExp, args:SArgs, env:SEnv):SObject = args.id(1).value match {
    case fun:SFun => if(args.length > 2) {
      exp match {
        case ls:SList => fun.prepare_and_call(SList(this::ls.values.tail), args.id(0).args, env)
      }
    } else fun
    case any:SAny => if(args.length > 2) {
      exp match {
        case ls:SList => {
          any.prepare_and_call(SList(ls.values.tail), args, env)
        }
        case exp:SExp => throw RuntimeException(location, "impossible")
      }
    } else any
  }

  override def toString() = {
    s"(${names.mkString(" ")})"
  }
}