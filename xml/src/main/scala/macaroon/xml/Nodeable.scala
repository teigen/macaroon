package macaroon.xml

import com.codecommit.antixml

trait Nodeable[A]{
  def node(a:A):antixml.Node
}

object Nodeable {
  def apply[A](f:A => antixml.Node):Nodeable[A] = new Nodeable[A]{
    def node(a: A) = f(a)
  }

  implicit val string = Nodeable[String](antixml.Text(_))
}
