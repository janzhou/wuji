package wuji

object SBoolean extends SClass(List("boolean"), List(SSymbol("boolean")), Some(SDao)) {
    case class SBoolean(location:SLocation, value:Boolean) extends SAny() {
        val names = wuji.SBoolean.names
        override def toString() = s"#${value}"
        funs(lib.ops.And, lib.ops.Or, lib.ops.Equal)
    }

    def apply(location:SLocation, value:Boolean) = {
        val result = SBoolean(location, value)
        result.setSClass(this)
        result
    }

    def fun(args:SArgs, env:SEnv):SObject = {
      val arg = args.name(params(0)).get
      arg.value match {
        case v:SBoolean => apply(v.location, v.value)
        case o:SObject => throw RuntimeException (o.location, "boolean expected")
      }
    }
}

object SInt extends SClass(List("int"), List(SSymbol("int")), Some(SDao)) {
    case class SInt(location:SLocation, value:Long) extends SAny() {
        val names = wuji.SInt.names
        override def toString() = value.toString
        funs(lib.ops.Add, lib.ops.Minus, lib.ops.Equal, lib.ops.LargerThan, lib.ops.SmallerThan)
    }

    def apply(location:SLocation, value:Long) = {
        val result = SInt(location, value)
        result.setSClass(this)
        result
    }

    def fun(args:SArgs, env:SEnv):SObject = {
      val arg = args.name(params(0)).get
      arg.value match {
        case v:SInt=> apply(v.location, v.value)
        case v:SFloat.SFloat => apply(v.location, v.value.toLong)
        case o:SObject => throw RuntimeException (o.location, "boolean expected")
      }
    }
}

object SFloat extends SClass(List("float"), List(SSymbol("float")), Some(SDao)) {
    case class SFloat(location:SLocation, value:Double) extends SAny() {
        val names = wuji.SFloat.names
        override def toString() = value.toString
        funs(lib.ops.Add, lib.ops.Minus, lib.ops.Equal, lib.ops.LargerThan, lib.ops.SmallerThan)
    }

    def apply(location:SLocation, value:Double) = {
        val result = SFloat(location, value)
        result.setSClass(this)
        result
    }

    def fun(args:SArgs, env:SEnv):SObject = {
      val arg = args.name(params(0)).get
      arg.value match {
        case v:SInt.SInt => apply(v.location, v.value.toInt)
        case v:SFloat => apply(v.location, v.value)
        case o:SObject => throw RuntimeException (o.location, "boolean expected")
      }
    }
}

object SString extends SClass(List("string"), List(SSymbol("string")), Some(SDao)) {
    case class SString(location:SLocation, value:String) extends SAny() {
        val names = wuji.SString.names
        override def toString() = value.toString
        funs(lib.ops.Add, lib.ops.Equal)
    }

    def apply(location:SLocation, value:String) = {
        val result = SString(location, value)
        result.setSClass(this)
        result
    }

    def fun(args:SArgs, env:SEnv):SObject = {
      val arg = args.name(params(0)).get
      arg.value match {
        case v:SInt.SInt => apply(v.location, v.value.toString)
        case v:SFloat.SFloat => apply(v.location, v.value.toString)
        case v:SBoolean.SBoolean => apply(v.location, v.value.toString)
        case v:SString => apply(v.location, v.value)
        case o:SObject => throw RuntimeException (o.location, "boolean expected")
      }
    }
}

object SNil extends SClass(List("nil"), List[SSymbol](), Some(SDao)) {
    case class SNil(location:SLocation) extends SAny() {
        val names = wuji.SNil.names
        override def toString() = names.head
    }

    def apply(location:SLocation) = {
        val result = SNil(location)
        result.setSClass(this)
        result
    }

    def fun(args:SArgs, env:SEnv):SObject = {
      SNil(args.location)
    }
}