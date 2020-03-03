package wuji

// name == null when call object(any)
// name != null when call fun
case class SArg(name:SSymbol, obj:SObject, exp:SObject, args:SArgs, env:SEnv) extends SObject {
  val names = List("SArg")
  val location = if(name == null) exp.location else name.location
  def value:SObject = {
    exp match {
      case any:SAny => any
      case symbol:SSymbol => {
        if(env.has(symbol)) {
            env.get(symbol)
        } else {
          args.name(symbol).map(_.value).getOrElse(
            obj.get(symbol)
          )
        }
      }
      case exp:SExp => obj.prepare_and_call(exp, args, SEnv(env))
    }
  }

  def symbol = exp match {
    case symbol:SSymbol => symbol
    case obj:SObject => throw RuntimeException(obj.location, s"expected symbol ${obj}")
  }
}

class SArgs(val args:SArg*) extends SObject {
    val names = List("SArgs")
    val location = if(args.length > 0) args.map(_.location).reduce(_ + _) else SLocation(0, 0)
    val length = args.length
    def id(id:Int):SArg = {
        if(id < 0 || id >= length) throw RuntimeException(location, s"arg not exist in $id")
        args(id)
    }

    def name(name:SSymbol):Option[SArg] = {
        args.filter(_.name != null).filter(_.name.value == name.value).headOption
    }

    def exp(exp:SObject):Option[SArg] = {
        args.filter(_.exp == exp).headOption
    }

    def caller = id(0).value
}

object SArgs {
    def apply(as:List[SArg]):SArgs = new SArgs(as:_*)
    def apply(as:SArg*):SArgs = new SArgs(as:_*)
}