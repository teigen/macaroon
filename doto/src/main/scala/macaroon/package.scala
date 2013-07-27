import language.experimental.macros
import reflect.macros.Context

package object macaroon {

  /**
   * like http://clojure.org/java_interop#Java%20Interop-The%20Dot%20special%20form-(doto%20instance-expr%20(instanceMethodName-symbol%20args*)*) but also accepts functions.
   *
   * lambdas and method references will be inlined/unpacked so you don't get the overhead of function creation and invocation
   *
   * doto(new java.util.HashMap[String, Int])(_.put("a", 1), _.put("b", 2))
   *
   * expands to
   *
   * {
   *   val $1$ = new java.util.HashMap[String, Int]
   *   $1$.put("a", 1)
   *   $1$.put("b", 2)
   *   $1$
   * }
   *
   */
  def doto[A](a:A)(fs:(A => Any)*):A = macro doto_impl[A]

  def doto_impl[A](c:Context)(a:c.Expr[A])(fs:c.Expr[A => Any]*) = {
    import c.universe._

    val ref = ValDef(Modifiers(), newTermName(c.fresh()), TypeTree(), a.tree)

    def inline(tree:Tree):Tree = tree match {
      case Function(List(ValDef(_, name, _, _)), body) =>
        replace(name, body)
      case Block(stats, Function(List(ValDef(_, name, _, _)), body)) =>
        Block(stats, replace(name, body))
      case unknown =>
        Apply(unknown, List(Ident(ref.name)))
    }

    def replace(term:TermName, tree:Tree) = (new Transformer {
      override def transform(tree: Tree):Tree = tree match {
        case Ident(`term`)    => Ident(ref.name)
        case Select(q, n)     => Select(transform(q), n)
        case Apply(fun, args) => Apply(transform(fun), transformTrees(args))
        case _                => super.transform(tree)
      }
    }).transform(tree)

    val inlined = for(f <- fs) yield inline(f.tree)

    val block = Block(ref :: inlined.toList, Ident(ref.name))

    c.Expr[A](block)
  }

}
