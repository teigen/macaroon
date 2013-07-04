package macaroon.xml

import reflect.macros.Context
import com.codecommit.antixml

/**
 * compile time translation of literal scala.xml to antixml
 *
 * the shape of the generated code closely resembles that of the generated
 * scala.xml code, but favoring antixml builders instead of the reassigning to vars
 *
 * I have a strong suspicion that this isn't really the correct way of doing this, as
 * the code is an unreadable mess - but it typechecks, and produces the expected output
 * (atleast for my tiny testcases)
 *
 * the translation also improves typesafety, requiring an instance of the typeclass [[macaroon.xml.Nodeable]]
 * for each variable. this has the nice benefit of allowing formatting and customized rendering for various types
 * e.g Dates
 */
object Anti {

  def anti_nodeseq(c:Context)(ns:c.Expr[scala.xml.NodeSeq]) = {
    val impl        = new Impl[c.type](c)
    val transformed = impl.transformer.transform(ns.tree)
    c.Expr[antixml.Group[antixml.Node]](transformed)
  }

  def anti_elem(c:Context)(elem:c.Expr[scala.xml.Elem]) = {
    val impl        = new Impl[c.type](c)
    val transformed = impl.transformer.transform(elem.tree)
    c.Expr[antixml.Elem](transformed)
  }

  class Impl[C <: Context](val c:C){
    import c.universe._

    def dump(tree:Tree){
      println("---" * 10)
      println(show(tree, printTypes = false))
      println("***" * 10)
      println(showRaw(tree, printTypes = false))
      println("---" * 10)
    }

    object transformer extends Transformer {

      val $buf         = newTermName("$buf")
      val $md          = newTermName("$md")
      val $tmpscope    = newTermName("$tmpscope")
      val &+           = newTermName("$amp$plus")
      val seqToNodeSeq = newTermName("seqToNodeSeq")
      val +=           = newTermName("$plus$eq")
      val $scope       = newTermName("$scope")

      object NewScalaXmlElem {
        def unapply(tree:Tree) = tree match {
          case Apply(Select(New(text), _), List(arg)) if text.tpe == typeOf[scala.xml.Text] => Some(arg)
          case _ => None
        }
      }

      override def transform(tree: Tree):Tree = tree match {
        case ValDef(_, `$buf`, _, _) =>
          ValDef(Modifiers(), $buf, TypeTree(typeOf[collection.mutable.Builder[antixml.Node, antixml.Group[antixml.Node]]]), reify(antixml.Group.newBuilder[antixml.Node]).tree)

        case Apply(Select(Ident(`$buf`), `&+`), List(arg)) =>
          val _arg = transform(arg)
          val node = if(_arg.tpe == null) _arg else Apply(Select(c.inferImplicitValue(appliedType(typeOf[Nodeable[_]], List(arg.tpe))), newTermName("node")), List(_arg))
          Apply(Select(Ident($buf), +=), List(node))

        case ValDef(_, `$md`, _, _) =>
          ValDef(Modifiers(), $md, TypeTree(typeOf[collection.mutable.Builder[(antixml.QName, String), antixml.Attributes]]), reify(antixml.Attributes.newBuilder).tree)

        case Assign(Ident(`$md`), Apply(Select(New(prefixed), _), List(prefix, key, NewScalaXmlElem(value), _))) if prefixed.tpe == typeOf[scala.xml.PrefixedAttribute] =>
          val p = reify((
            antixml.QName(c.Expr[String](prefix).splice, c.Expr[String](key).splice)),
            c.Expr[String](value).splice).tree
          Apply(Select(Ident($md), +=), List(p))

        case Assign(Ident(`$md`), Apply(Select(New(unprefixed), _), List(key, NewScalaXmlElem(value), _))) if unprefixed.tpe == typeOf[scala.xml.UnprefixedAttribute] =>
          val u = reify((
            antixml.QName(c.Expr[String](key).splice),
            c.Expr[String](value).splice)).tree
          Apply(Select(Ident($md), +=), List(u))

        case ValDef(modifiers, `$tmpscope`, _, _) =>
          ValDef(modifiers, $tmpscope, TypeTree(typeOf[antixml.NamespaceBinding]), reify(antixml.NamespaceBinding.empty).tree)

        case ValDef(modifiers, `$scope`, _, Ident(`$tmpscope`)) =>
          ValDef(modifiers, $scope, TypeTree(typeOf[antixml.NamespaceBinding]), Ident($tmpscope))

        case Assign(Ident(`$tmpscope`), Apply(_, List(prefix, namespace, _))) =>
          Assign(Ident($tmpscope), Apply(Select(Ident($tmpscope), newTermName("append")), List(prefix, namespace)))

        case NewScalaXmlElem(text) =>
          val Apply(what, _) = reify(antixml.Text("")).tree
          Apply(what, List(text))

        case Apply(Select(New(elem), _), prefix :: label :: attributes :: scope :: minimizeEmpty :: children) if elem.tpe == typeOf[scala.xml.Elem] =>
          val Apply(prefixFun, _) = reify(Option[String]("")).tree
          val _prefix             = Apply(prefixFun, List(prefix))

          val Apply(childrenFun, _) = reify(antixml.Group[antixml.Node]()).tree
          val _children             = Apply(childrenFun, children.map(t => transform(t)))

          val _namespace = if(scope.tpe == typeOf[scala.xml.TopScope.type]) reify(antixml.NamespaceBinding.empty).tree else Ident($scope)
          val _attributes = if(attributes.tpe == typeOf[scala.xml.Null.type]) reify(antixml.Attributes()).tree else Apply(Select(Ident($md), newTermName("result")), Nil)
          Apply(Select(New(TypeTree(typeOf[antixml.Elem])), nme.CONSTRUCTOR), List(_prefix, label, _attributes, _namespace, _children))

        case Typed(t, Ident(tpnme.WILDCARD_STAR)) =>
          Typed(transform(t), Ident(tpnme.WILDCARD_STAR))

        case Block(pre, Ident(`$buf`)) =>
          Block(pre.map(t => transform(t)), Apply(Select(Ident($buf), newTermName("result")), Nil))

        case Apply(Select(_, `seqToNodeSeq`), List(arg)) =>
          transform(arg)

        case Block(stats, expr) =>
          Block(stats.map(s => transform(s)), transform(expr))

        case _ =>
          super.transform(tree)
      }
    }
  }
}
