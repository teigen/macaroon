import scala.reflect.macros.Context
import language.experimental.macros

/*
 * Example:
 * 
 * case MapExt((1, a), (2, b)) => (a, b)
 *
 * is rewritten to  
 *
 * object Test$1{
 *   val $1 = MapExt.Key(1)
 *   val $2 = MapExt.Key(2)
 * }
 * case MapExt.And(Test$1.$1(a), Test$1.$2(b)) => (a, b)
 *
 * implementation is using untyped macros http://docs.scala-lang.org/overviews/macros/untypedmacros.html
 * and quasiquotes http://docs.scala-lang.org/overviews/macros/quasiquotes.html
 *
 * see src/test/scala/MapTest.scala for example
 */
object MapExt {
  def impl(c: Context)(xs: c.Tree*) = {
    import c.universe._    
        
    val name       = c.freshName(c.enclosingImpl.name).toTermName
    val pid        = c.enclosingPackage.pid
    def fresh      = TermName(c.freshName())    
    def mk(l:Tree) = q"val ${fresh} = MapExt.Key($l)"
    
    val args = xs.toList.map{
      case q"scala.Tuple2($key, $bind)" => mk(key) -> bind
      case q"->($key, $bind)"           => mk(key) -> bind
    }
    
    val body = args.map(_._1)
    val module = q"object $name { ..$body }"
    /*
    TODO : I don't really want to 'introduceTopLevel' as that requires all keys to be referrable from top level.
    I want to introduce the $body code (preferrably without the $module) right before the match block, which would allow everything
    in scope to be usable as key-extractors. 
     */
    c.introduceTopLevel(pid.toString, module)
    
    val refs = args.map{ case (k, v) => 
      val ref = q"$pid.$name.${k.name}"
      Apply(ref, List(v))
    }
    
    refs.reduceLeft[Tree]{
      case (left, right) => q"MapExt.And($left, $right)"
    }    
  }
  
  def unapply(xs: _*) = macro impl
  
  case class Key[A](key:A){
    def unapply[B](map:Map[A, B]) = map.get(key)
  }
  
  object And {
    def unapply[A](a:A) = Some((a,a))
  }
}