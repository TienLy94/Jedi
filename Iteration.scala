package expression
import value._
import context._

case class Iteration(val condition: Expression, val body: Expression) extends SpecialForm
{
  def execute(env: Environment): Value = {
    if (!condition.execute(env).isInstanceOf[Boole]) throw new TypeException("Condition type is wrong.")
    while(condition.execute(env).asInstanceOf[Boole] == Boole(true))
    {
      body.execute(env)
    }
    Notification.DONE
  }
}