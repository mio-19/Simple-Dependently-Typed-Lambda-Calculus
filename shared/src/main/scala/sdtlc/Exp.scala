package sdtlc

import scala.collection.immutable.HashMap

final case class Context(inner: HashMap[Var, Exp]) {
  def extend(x: Var, t: Exp): Context = Context(inner.updated(x, t))

  def get(x: Var): Option[Exp] = inner.get(x)

  def getOrElse(x: Var, default: => Exp): Exp = inner.getOrElse(x, {
    default
  })

  def toList: List[(Var, Exp)] = inner.toList
}

object Context {
  val Empty: Context = Context(HashMap())
}

sealed trait Exp {
  def untypedNormalForm: Exp

  def subst(x: Var, v: Exp): Exp

  final def storeType(xs: List[(Var, Exp)]): Exp = xs match {
    case Nil => this
    case (v, t) :: xs => this.subst(v, The(t, v)).storeType(xs)
  }

  final def storeType(x: Context): Exp = this.storeType(x.toList)

  def infer(env: Context): Option[Exp]

  final def checkInfer(env: Context, t: Exp): Boolean = this.infer(env) match {
    case Some(t0) => t.equalType(t0)
    case None => false
  }

  def check(env: Context, t: Exp): Boolean = checkInfer(env, t)

  def equalType(other: Exp): Boolean = ???
}

final case class Apply(f: Exp, x: Exp) extends Exp {
  override def untypedNormalForm: Exp = f.untypedNormalForm match {
    case Lambda(arg, body) => body.subst(arg, x).untypedNormalForm
    case f => Apply(f, x)
  }

  override def subst(x: Var, v: Exp): Exp = Apply(f.subst(x, v), x.subst(x, v))

  override def infer(env: Context): Option[Exp] = f.infer(env) flatMap {
    case Pi(argPi, domainPi, codomainPi) if x.check(env, domainPi) => Some(codomainPi.subst(argPi, The(domainPi, x)))
    case _ => None
  }

  override def check(env: Context, t: Exp): Boolean = this.checkInfer(env, t)
}

final case class Var(x: Symbol) extends Exp {
  override def untypedNormalForm: Exp = this

  override def subst(x: Var, v: Exp): Exp = if (x == this) {
    v
  } else {
    this
  }

  override def infer(env: Context): Option[Exp] = env.get(this)
}

final case class The(t: Exp, x: Exp) extends Exp {
  override def untypedNormalForm: Exp = x.untypedNormalForm

  override def subst(x: Var, v: Exp): Exp = The(t.subst(x, v), x.subst(x, v))

  override def infer(env: Context): Option[Exp] = Some(t)
}

final case class Lambda(arg: Var, exp: Exp) extends Exp {
  override def untypedNormalForm: Exp = Lambda(arg, exp.untypedNormalForm)

  override def subst(x: Var, v: Exp): Exp = if (x == arg) {
    this
  } else {
    Lambda(arg, exp.subst(x, v))
  }

  override def infer(env: Context): Option[Exp] = None

  override def check(env: Context, t: Exp): Boolean = t.untypedNormalForm match {
    case Pi(argPi, domainPi, codomainPi) => exp.check(env.extend(arg, domainPi), codomainPi.subst(argPi, arg))
    case _ => false
  }
}

final case class Universe(level: Exp) extends Exp {
  override def untypedNormalForm: Exp = level.untypedNormalForm

  override def subst(x: Var, v: Exp): Exp = Universe(level.subst(x, v))

  override def infer(env: Context): Option[Exp] = Some(Universe(???))
}

final case class Pi(arg: Var, domain: Exp, codomain: Exp) extends Exp {
  override def untypedNormalForm: Exp = Pi(arg, domain.untypedNormalForm, codomain.untypedNormalForm)

  override def subst(x: Var, v: Exp): Exp = Pi(arg, domain.subst(x, v), if (x == arg) {
    codomain
  } else {
    codomain.subst(x, v)
  })

  override def infer(env: Context): Option[Exp] = Some(Universe(???))
}