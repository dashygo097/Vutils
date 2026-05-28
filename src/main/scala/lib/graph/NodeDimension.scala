package vutils.graph

trait NodeDimensionImpl extends NodeNamed {
  def nodeType: NodeType
  def dim: NodeDim
  def value: String
}

class NodeDimensionRegistry[T <: NodeDimensionImpl](
  val nodeType: NodeType,
  val dim: NodeDim
) extends NodeRegistry[T](s"NodeDimension:${nodeType.name}:${dim.name}") {
  def select(value: String): T =
    get(value).getOrElse {
      throw new NoSuchElementException(s"NodeDimension '${nodeType.name}.${dim.name}' has no value '$value'. Available: ${listAvailable().mkString(", ")}")
    }

  def select(config: NodeConfig): T =
    select(config.selector(dim))
}
