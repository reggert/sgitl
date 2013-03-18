package com.github.reggert.sgitl.plumbing.objects

import scala.collection.immutable.SortedSet

trait GitTree extends GitObject 
{
	final override def objectType = ObjectType.Tree
	
	def entries : SortedSet[TreeEntry]
}