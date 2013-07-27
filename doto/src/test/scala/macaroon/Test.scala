package macaroon

object Test extends App {

  class Foo {
    var a:String = _
    var b:Int = _
    var c:Boolean = _
    var self:Foo = _
    override def toString = List(a, b, c).mkString(";")
  }

  def setB(x:Foo) = x.b = 10

  val setC = (x:Foo) => x.c = true

  val foo = doto(new Foo)(_.a = "hello", setB, setC, x => x.self = x)

  println(foo)

  println(doto(new java.util.HashMap[String, Int])(_.put("a", 1), _.put("b", 2)))

}
