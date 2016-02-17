import scala.language.experimental.macros
import scala.reflect.api.Trees
import scala.reflect.macros.blackbox

sealed trait Component {
  def toTree(implicit c: blackbox.Context): c.Tree
  def derive: Component
}

case class Variable(name: String) extends Component {
  override def toTree(implicit c: blackbox.Context): c.Tree = {
    import c.universe._
    Ident(TermName(name))
  }

  override def derive: Component = DoubleConstant(1)
}

case class DoubleConstant(value: Double) extends Component {
  override def toTree(implicit c: blackbox.Context): c.Tree = {
    import c.universe._
    Literal(Constant(value))
  }

  override def derive: Component = DoubleConstant(0)
}

object Scalac {

  private def getComponent(tree: Trees#Tree)(implicit c: blackbox.Context): Component = {
    import c.universe._
    tree match {
      case Ident(TermName(x)) => Variable(x)
      case Literal(Constant(a)) => DoubleConstant(a.toString.toDouble)
    }
  }

  private def extractComponents(tree: Trees#Tree)(implicit c: blackbox.Context): List[Component] = {
    import c.universe._
    tree match {
      case q"$nextTree + $arg" =>  getComponent(arg) :: extractComponents(nextTree)
      case somethingElse => getComponent(somethingElse) :: Nil
    }
  }

  def derivative(f: Double => Double): Double => Double = macro derivativeImpl

  def derivativeImpl(c: blackbox.Context)(f: c.Expr[Double => Double]): c.Expr[Double => Double] = {
    import c.universe._

    val Function(List(ValDef(_, name, _, _)), funcBody) = f.tree

    val components = extractComponents(funcBody)(c)
    val transformedComponents = components.map(_.derive.toTree(c)).reduce((a, b) => q"$a + $b")

    c.Expr(q"($name: Double) => $transformedComponents")
  }
}

object Macros {

  def hello = macro helloImpl

  def helloImpl(c: blackbox.Context): c.Expr[Unit] = {
    import c.universe._

    /*reify {
      println("hello!")
    }

    c.Expr {
      Apply(Ident(TermName("println")), List(Literal(Constant("hello!"))))
    }*/

    c.Expr(q"""println("hello!")""")
  }

  def hello2(s: String) = macro hello2Impl

  def hello2Impl(c: blackbox.Context)(s: c.Expr[String]): c.Expr[Unit] = {
    import c.universe._

    /*reify {
      println(s"hello ${s.splice}!")
    }

    c.Expr {
      Apply(
        Ident(TermName("println")),
        List(
          Apply(
            Select(
              Apply(
                Select(
                  Literal(Constant("hello ")),
                  TermName("$plus")
                ),
                List(
                  s.tree
                )
              ),
              TermName("$plus")
            ),
            List(
              Literal(Constant("!"))
            )
          )
        )
      )
    }*/

    c.Expr(q"""println("hello " + ${s.tree} + "!")""")
  }

  def debug[T](param: T) = macro debugImpl[T]

  def debugImpl[T](c: blackbox.Context)(param: c.Expr[T]): c.Expr[Unit] = {
    import c.universe._

    //    c.Expr(q"""println(${showRaw(param.tree)} + " = " + $param)""")
    val z = c.Expr[String](Literal(Constant(showRaw(param.tree))))

    reify {
      println(z.splice + " = " + param.splice)
    }
  }
}