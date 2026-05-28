package vutils.graph

final case class NodeDim(name: String) {
  override def toString: String = name
}

final class NodeSelector private (
  val values: Map[String, String]
) {
  def +(choice: (NodeDim, String)): NodeSelector = {
    val (dim, value) = choice
    NodeSelector.fromMap(values + (dim.name -> value))
  }

  def ++(other: NodeSelector): NodeSelector =
    NodeSelector.fromMap(values ++ other.values)

  def get(dim: NodeDim): Option[String] =
    values.get(dim.name)

  def apply(dim: NodeDim): String =
    values.getOrElse(dim.name, throw new NoSuchElementException(s"NodeSelector: dimension '${dim.name}' not found"))

  def contains(dim: NodeDim): Boolean =
    values.contains(dim.name)

  def matches(required: NodeSelector): Boolean =
    required.values.forall { case (dim, value) => values.get(dim).contains(value) }

  def score(required: NodeSelector): Int =
    required.values.count { case (dim, value) => values.get(dim).contains(value) }

  def isEmpty: Boolean = values.isEmpty

  def nonEmpty: Boolean = values.nonEmpty

  def canonicalName: String =
    if (values.isEmpty) "default" else values.toSeq.sortBy(_._1).map { case (dim, value) => s"${dim}_$value" }.mkString("__")

  override def toString: String = canonicalName
}

object NodeSelector {
  val empty: NodeSelector = new NodeSelector(Map.empty[String, String])

  def fromMap(values: Map[String, String]): NodeSelector =
    new NodeSelector(values)

  def apply(choices: (NodeDim, String)*): NodeSelector =
    new NodeSelector(choices.map { case (dim, value) => dim.name -> value }.toMap)
}

final case class NodeConfig(
  selector: NodeSelector = NodeSelector.empty,
  options: Map[String, Any] = Map.empty
) {
  def +(choice: (NodeDim, String)): NodeConfig =
    copy(selector = selector + choice)

  def ++(other: NodeConfig): NodeConfig =
    NodeConfig(selector ++ other.selector, options ++ other.options)

  def option[T](name: String): T =
    options.getOrElse(name, throw new NoSuchElementException(s"NodeConfig: option '$name' not found")).asInstanceOf[T]

  def optionOrElse[T](name: String, default: T): T =
    options.get(name).map(_.asInstanceOf[T]).getOrElse(default)

  def withOption(name: String, value: Any): NodeConfig =
    copy(options = options + (name -> value))
}
