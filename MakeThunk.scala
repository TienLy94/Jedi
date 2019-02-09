package expression
import context._
import value._

case class MakeThunk(val body: Expression) extends SpecialForm
{
  def execute(env: Environment): Value = {
    new Thunk(body, env)
  }
}