package com.github.reggert.sgitl.plumbing.objects

import scala.collection.immutable.SortedSet

final case class LooseTree(val entries : SortedSet[TreeEntry]) extends LooseObject
{
	override def objectType = ObjectType.Tree
	override lazy val content = entries.toIndexedSeq.flatMap(_.encoded)
}