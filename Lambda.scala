package expression
import context._
import value._

case class Lambda(val parameters: List[Identifier], val body: Expression) extends SpecialForm 
{
  def execute(env: Environment): Value = 
  {
    Closure.apply(parameters, body, env)
  }
}