// PEANO numbers data
abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat = new Succ(this)
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat {
  def isZero: Boolean = true
  def predecessor: Nat = throw new Error("0.predecessor")
  def + (that: Nat): Nat = that
  def -(that: Nat) = if(that.isZero) this else throw new Error("negative")
  def - (that: Nat): Nat = if(that.isZero) this else throw new Error("0.predecessor")
  }
}
class Succ(n: Nat) extends Nat {
  def isZero = false
  def predecessor = n
  def + (that: Nat) = new Succ(n + that)
  def - (that: Nat) =  if (that.isZero) this else n - that.predecessor
}

var first = new Succ(Zero)
println(first.isZero)
