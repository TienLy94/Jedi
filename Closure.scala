package value
import expression._
import context._

class Closure(val parameters: List[Identifier], val body: Expression, val defEnv: Environment) extends Value 
{
  def apply(args: List[Value]): Value = 
  {
    val tempEnv = new Environment(defEnv)
    tempEnv.bulkPut(parameters, args)
    body.execute(tempEnv)
  }
}

object Closure
{
  def apply(param: List[Identifier], body: Expression, defE: Environment) = new Closure(param, body, defE)
}