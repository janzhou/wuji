package wuji

abstract class SClass(
    override val names:List[String],
    override val params:List[SSymbol],
    override val parent:Option[SClass]
) extends SFun(names, params) {
    override def toString = {
        val name =  s"(${names.mkString(" ")})"
        parent.map(p => s"${p.toString} -> $name").getOrElse(name)
    }

}

object SClass extends SClass(List("class", "类"), List[SSymbol](), Some(SDao)) {
    setSClass(this)

    def fun(args:SArgs, env:SEnv):SObject = SDao.fun(args, env)
}