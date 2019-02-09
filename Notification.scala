package value

class Notification(val msg: String) extends Value
{
  override def toString = msg 
}

object Notification
{
  def apply(msg: String) = new Notification(msg)
  val OK = new Notification("Ok")
  val DONE = new Notification("Done")
  val UNSPECIFIED = new Notification("Unspecified")
}