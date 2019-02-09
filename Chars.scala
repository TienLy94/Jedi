package value
import expression._

case class Chars(val value: String) extends Literal with Ordered[Chars] with Equals 
{
  def +(other: Chars) = Chars(this.value + other.value)
  def length: Integer = Integer(value.length)
  def substring(start: Integer, end: Integer = Integer(this.value.size))= this.value.substring(start.value, end.value)
  
  override def toString = value.toString
  
  def compare(other: Chars): Int = this.value.compare(other.value)
  override def canEqual(other: Any) =  other.isInstanceOf[Chars]
  override def equals(other: Any): Boolean = 
    other match 
    {
       case other: Chars => this.canEqual(other) && (other.value == this.value)
       case _ => false
    }
  override def hashCode = this.toString.##
}