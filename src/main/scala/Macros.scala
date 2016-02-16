import scala.language.dynamics
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

case class Multiply(first: Any, second: Any)

case class Power(first: Any, second: Any)

case class Add(first: Any, second: Any)

case class Variable(name: String)


object Macros {

  def integrate(f: Double => Double): Double => Double = macro integrateImpl

  def integrateImpl(c: whitebox.Context)(f: c.Expr[Double => Double]): c.Expr[Double => Double] = {
    import c.universe._

    object SelectJavaMathPow {
      def unapply(x: c.Tree) = x match {
        case Select(Select(Select(This(TypeName("java")), TermName("lang")), TermName("Math")), TermName("pow")) => true
        case _ => false
      }
    }

    def extractComponent(tree: c.Tree): Any = tree match {
      case Apply(SelectJavaMathPow(), List(a, b)) => Power(extractComponent(a), extractComponent(b))
      case Ident(TermName(x)) => Variable(x)
      case Literal(Constant(a)) => a.toString.toDouble
      case Apply(Select(somethingElse, TermName("$times")), List(args)) =>
        Multiply(extractComponent(args), extractComponent(somethingElse))
    }

    def extractComponents(tree: c.Tree): List[Any] = tree match {

      /*addition*/
      case Apply(Select(nextTree, TermName("$plus")), List(arg)) =>
        extractComponent(arg) :: extractComponents(nextTree)

      /*multiplication*/
      case Apply(Select(somethingElse, TermName("$times")), List(args)) =>
        (extractComponent(args), extractComponent(somethingElse)) :: Nil

      /*single literal*/
      case identOrLiteral => extractComponent(identOrLiteral) :: Nil
    }


    val Function(_ /*todo*/ , funcBody) = f.tree

    println(extractComponents(funcBody))

    val xx: c.universe.Tree = Apply(Select(Ident(TermName("x")), TermName("$plus")), List(Literal(Constant(5))))

    val z = q"(x: Double) => $xx"
    c.Expr(z)
  }

  def debug[T](param: T) = macro debugImpl[T]

  def debugImpl[T](c: whitebox.Context)(param: c.Expr[T]): c.Expr[Unit] = {
    import c.universe._

    val name = param.tree.toString
    val value = param.tree

    c.Expr(q"""println($name + " = " +  $value)""")
  }
}