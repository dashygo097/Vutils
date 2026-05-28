package vutils.graph

import scala.collection.mutable

trait NodeNamed {
  def name: String
}

class NodeRegistry[T <: NodeNamed](val registryName: String) {
  private val registry = mutable.LinkedHashMap.empty[String, T]

  protected def normalize(name: String): String =
    name.trim.toLowerCase

  def register(item: T): Unit = {
    val key = normalize(item.name)
    registry(key) = item
  }

  def get(name: String): Option[T] =
    registry.get(normalize(name))

  def getOrThrow(name: String): T =
    registry.getOrElse(
      normalize(name),
      throw new NoSuchElementException(s"$registryName '$name' not found. Available: ${listAvailable().mkString(", ")}")
    )

  def getOrElse(name: String, default: T): T =
    registry.getOrElse(normalize(name), default)

  def contains(name: String): Boolean =
    registry.contains(normalize(name))

  def listAvailable(): Seq[String] =
    registry.keys.toSeq.sorted

  def getAll(): Seq[T] =
    registry.values.toSeq
}

trait RegisteredNodeUtils[T <: NodeNamed] {
  def utils: T
  def registry: NodeRegistry[T]

  registry.register(utils)
}
