package value
import expression._
import context._

class Thunk(override val body: Expression, override val defEnv: Environment) extends Closure(Nil, body, defEnv) 
{  
  var cache: Value = null
  
  def thunk(): Value = 
  {
    if (cache == null) 
      cache = super.apply(Nil)
    cache
  }
}

object Thunk {
  def apply(body: Expression, defEnv: Environment) = new Thunk(body, defEnv)
}
