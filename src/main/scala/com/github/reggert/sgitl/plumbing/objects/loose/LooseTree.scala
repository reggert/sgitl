package com.github.reggert.sgitl.plumbing.objects.loose

import scala.collection.immutable.SortedSet
import com.github.reggert.sgitl.plumbing.objects.GitTree
import com.github.reggert.sgitl.plumbing.objects.ObjectType
import com.github.reggert.sgitl.plumbing.objects.TreeEntry

final case class LooseTree(override val entries : SortedSet[TreeEntry]) extends LooseObject with GitTree
{
	override lazy val content = entries.toIndexedSeq.flatMap(_.encoded)
}