package sdtlc

sealed trait UntypedExp {
  def betaReduce: UntypedExp

  def subst(x: UntypedVar, v: UntypedExp): UntypedExp
}

final case class UntypedApply(f: UntypedExp, value: UntypedExp) extends UntypedExp {
  override def betaReduce: UntypedExp = f.betaReduce match {
    case UntypedLambda(arg, exp) => exp.subst(arg, value).betaReduce
    case f => UntypedApply(f, value.betaReduce)
  }

  override def subst(x: UntypedVar, v: UntypedExp): UntypedExp = UntypedApply(f.subst(x, v), value.subst(x, v))
}

final case class UntypedVar(x: Symbol) extends UntypedExp {
  override def betaReduce: UntypedExp = this

  override def subst(x: UntypedVar, v: UntypedExp): UntypedExp = if (x == this) {
    v
  } else {
    this
  }
}

final case class UntypedLambda(arg: UntypedVar, exp: UntypedExp) extends UntypedExp {
  override def betaReduce: UntypedExp = UntypedLambda(arg, exp.betaReduce)

  override def subst(x: UntypedVar, v: UntypedExp): UntypedExp = if (x == arg) {
    this
  } else {
    UntypedLambda(arg, exp.subst(x, v))
  }
}