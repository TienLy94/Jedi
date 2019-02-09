package value

class Variable(var content: Value) extends Value
{
  override def toString = "[" + content.toString() + "]"
}

object Variable
{
  def apply(con: Value) = new Variable(con)
}