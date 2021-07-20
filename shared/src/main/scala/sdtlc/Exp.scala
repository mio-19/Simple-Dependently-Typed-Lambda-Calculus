package sdtlc

sealed trait Exp {

}

final case class Apply(f: Exp, x: Exp) extends Exp

final case class Var(x: Symbol) extends Exp

final case class Lambda(x: Symbol, exp: Exp) extends Exp

final case class Universe(level: Exp) extends Exp

final case class Pi(x: Symbol, domain: Exp, codomain: Exp) extends Exp