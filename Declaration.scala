package expression
import context._
import value._

case class Declaration(val ident: Identifier, val exp: Expression) extends SpecialForm 
{
  def execute(env: Environment): Value =
  {
    env(ident) = exp.execute(env)
    Notification.OK
  }
}