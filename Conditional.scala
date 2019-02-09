package expression
import context._
import value._

case class Conditional(val condition: Expression, val consequent: Expression, val alternative: Expression = null) extends SpecialForm
{
  def execute(env: Environment): Value =
  {
    if (!condition.execute(env).isInstanceOf[Boole])throw new TypeException("Condition is wrong.")
    if (condition.execute(env) == Boole(true))
    {
      consequent.execute(env)
    }
    else
    {
      if (alternative == null)
      {
        Notification.UNSPECIFIED
      }
      else
      {
        alternative.execute(env)
      }
    }
  }
}
