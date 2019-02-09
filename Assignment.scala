package expression
import value._
import context._

case class Assignment(val vbl: Identifier, val update: Expression) extends SpecialForm
{
  def execute(env: Environment): Value = {
    if (!vbl.execute(env).isInstanceOf[Variable]) throw new TypeException("Wrong type input.")
    var someVar = vbl.execute(env).asInstanceOf[Variable]
    someVar.content = update.execute(env)
    Notification.DONE
  }
}