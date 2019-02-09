package expression
import context._
import value._

case class Block(val exps: List[Expression]) extends SpecialForm
{
  def execute(env: Environment): Value = 
  {
    val tempEnv = new Environment(env)
    for (i <- 0 to exps.length - 2)
      exps(i).execute(tempEnv)
    exps(exps.length - 1).execute(tempEnv)
  }
}