import macaroon.xml._

object AntiTest extends App {
  val variable = "oj"

  val group = anti(
    <hello xmlns:x="xxx">
      <ps:world foo="bar" x:monkey="donkey"/>
      {variable}
    </hello>
    <bah>{variable}</bah>
  )

  println(group)

  val elem = anti(<foo/>)

  println(elem)
}