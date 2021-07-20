package sdtlc

sealed trait Exp {
  def untypedNormalForm: Exp

  def subst(x: Var, v: Exp): Exp
}

final case class Apply(f: Exp, x: Exp) extends Exp {
  override def untypedNormalForm: Exp = f.untypedNormalForm match {
    case Lambda(arg, body) => body.subst(arg, x).untypedNormalForm
    case f => Apply(f, x)
  }

  override def subst(x: Var, v: Exp): Exp = Apply(f.subst(x, v), x.subst(x, v))
}

final case class Var(x: Symbol) extends Exp {
  override def untypedNormalForm: Exp = this

  override def subst(x: Var, v: Exp): Exp = if (x == this) {
    v
  } else {
    this
  }
}

final case class Lambda(arg: Var, exp: Exp) extends Exp {
  override def untypedNormalForm: Exp = Lambda(arg, exp.untypedNormalForm)

  override def subst(x: Var, v: Exp): Exp = if (x == arg) {
    this
  } else {
    Lambda(arg, exp.subst(x, v))
  }
}

final case class Universe(level: Exp) extends Exp {
  override def untypedNormalForm: Exp = level.untypedNormalForm

  override def subst(x: Var, v: Exp): Exp = Universe(level.subst(x, v))
}

final case class Pi(arg: Var, domain: Exp, codomain: Exp) extends Exp {
  override def untypedNormalForm: Exp = Pi(arg, domain.untypedNormalForm, codomain.untypedNormalForm)

  override def subst(x: Var, v: Exp): Exp = Pi(arg, domain.subst(x, v), if (x == arg) {
    codomain
  } else {
    codomain.subst(x, v)
  })
}