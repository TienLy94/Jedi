package expression
import context._
import value._

case class Identifier(val name: String) extends Expression 
{
   override def toString = name
   def execute(env: Environment) = {
     val tempValue = env(this)
     if (tempValue.isInstanceOf[Thunk])
       tempValue.asInstanceOf[Thunk].thunk()
     else
       tempValue
   }
}
