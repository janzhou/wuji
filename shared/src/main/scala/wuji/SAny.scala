package wuji

abstract class SAny() extends SObject {
  funs(
    lib.Defobj, lib.Undefobj, lib.Letobj, lib.Setobj,
    lib.Defun, lib.Defclass, lib.Eval,
    lib.If, lib.While,
    SDao, SBoolean, SInt, SFloat, SString
  )
}