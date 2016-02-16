import scala.language.dynamics
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

sealed trait Component

case class Negate(value: Component) extends Component

case class Multiply(first: Component, second: Component) extends Component

case class Power(first: Component, second: Component) extends Component

case class Add(first: Component, second: Component) extends Component

case class Variable(name: String) extends Component

case class DoubleConstant(value: Double) extends Component {
  override def toString: String = value.toString
}

object Macros {

  def integrate(f: Double => Double): Double => Double = macro integrateImpl

  def integrateImpl(c: whitebox.Context)(f: c.Expr[Double => Double]): c.Expr[Double => Double] = {
    import c.universe._

    object SelectJavaMathPow {
      def unapply(x: c.Tree): Boolean = x match {
        case Select(Select(Select(This(TypeName("java")), TermName("lang")), TermName("Math")), TermName("pow")) => true
        case _ => false
      }
    }

    object BinaryOp {
      def unapply(x: c.Tree): Option[(String, c.Tree, c.Tree)] = x match {
        case Apply(Select(a, TermName(op)), List(b)) => Some((op, a, b))
        case _ => None
      }
    }

    def getComponent(tree: c.Tree): Component = tree match {
      /*variable*/
      case Ident(TermName(x)) => Variable(x)
      /*constant*/
      case Literal(Constant(a)) => DoubleConstant(a.toString.toDouble)
      /*multiplication*/
      case BinaryOp("$times", a, b) =>
        Multiply(getComponent(a), getComponent(b))
      /*exponentation*/
      case Apply(SelectJavaMathPow(), List(a, b)) =>
        Power(getComponent(a), getComponent(b))
      case Select(x, TermName("unary_$minus")) =>
        Negate(getComponent(x))
      case Select(x, TermName("unary_$plus")) =>
        getComponent(x)
    }

    def extractComponents(tree: c.Tree): List[Component] = tree match {
      /*addition*/
      case BinaryOp("$plus", arg, nextTree) =>
        getComponent(nextTree) :: extractComponents(arg)
      /*substraction*/
      case BinaryOp("$minus", arg, nextTree) =>
        Negate(getComponent(nextTree)) :: extractComponents(arg)
      /*single component*/
      case identOrLiteral => getComponent(identOrLiteral) :: Nil
    }

    val Function(List(ValDef(_, TermName(argName), _, _)), funcBody) = f.tree

    val components = extractComponents(funcBody)

    /*this part is actually unnecessary*/
    println("is function valid? " + components.forall {
      case Variable(name) if name != argName => false
      case _ => true
    })

    println("Body: " + components)

    def getTreeOf(comp: Component): c.Tree = comp match {
      case Variable(a) => Ident(TermName(a))
      case DoubleConstant(a) => q"$a"
      case Multiply(a, b) => q"${getTreeOf(a)} * ${getTreeOf(b)}"
      case Power(a, b) => q"Math.pow(${getTreeOf(a)}, ${getTreeOf(b)})"
      case Add(a, b) => q"${getTreeOf(a)} + ${getTreeOf(b)}"
      case Negate(a) => q"-${getTreeOf(a)}"
    }

    val transformedComponents = components.map(comp => getTreeOf(comp)).reduce((a, b) => q"$a + $b")

    val z = q"(x: Double) => $transformedComponents"

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