package com.github.reggert.sgitl.plumbing.objects

final case class LooseBlob(override val content : IndexedSeq[Byte]) extends LooseObject 
{
	override def objectType = ObjectType.Blob
}