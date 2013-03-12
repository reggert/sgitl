package com.github.reggert.sgitl.plumbing.objects

import scala.collection.immutable.VectorBuilder

final case class TreeEntry(fileMode : FileMode, rawName : Seq[Byte], referencedObjectId : SHA1)
	extends Ordered[TreeEntry]
{
	import TreeEntry._
	
	require(!rawName.contains(NullByte))
	
	def encoded : IndexedSeq[Byte] = 
		(new VectorBuilder[Byte] ++= fileMode.toBytes += SpaceByte ++= rawName += NullByte ++= referencedObjectId.toBytes).result
		
	def utf8Name = rawName match
	{
		case UTF8(s) => s
	}
	
	def systemName = rawName match
	{
		case SystemEncoding(s) => s
	}
	
	private def comparableName = 
		if (fileMode == FileMode.Tree) rawName :+ SlashByte else rawName
		
	private def mappedComparableName = comparableName.view.map(_.toInt & 0xff)
	
	override def compare(that : TreeEntry) = 
		Ordering.Implicits.seqDerivedOrdering(Ordering.Int).compare(this.mappedComparableName, that.mappedComparableName)
	
	override def toString = s"TreeEntry($fileMode, $utf8Name, $referencedObjectId)"
}


object TreeEntry
{
	private val SpaceByte = 0x20.toByte  // the byte... from space!
	private val SlashByte = 0x2f.toByte
	
	private object ModeAndName
	{
		def unapply(encoded : Seq[Byte]) : Option[(FileMode, Seq[Byte])] = encoded.span(_ != SpaceByte) match
		{
			case (UTF8(FileMode.AsString(mode)), SpaceByte +: rawName) => Some(mode, rawName)
			case _ => None
		}
	}
	
	object Encoded
	{
		def apply(treeEntry : TreeEntry) = treeEntry.encoded
		
		def unapply(encoded : Seq[Byte]) : Option[TreeEntry] = encoded.span(_ != NullByte) match
		{
			case (ModeAndName(mode, rawName), NullByte +: SHA1.AsBytes(sha1)) => 
				Some(new TreeEntry(mode, rawName, sha1))
			case _ => None
		}
	}
	
	
	object EncodedSeq
	{
		def apply(treeEntries : Seq[TreeEntry]) = treeEntries.toIndexedSeq.flatMap(_.encoded)
		
		def unapplySeq(encoded : IndexedSeq[Byte]) : Option[Seq[TreeEntry]] = encoded.span(_ != NullByte) match
		{
			case (ModeAndName(mode, rawName), NullByte +: afterNull) if afterNull.size >= SHA1.AsBytes.ExpectedLength =>
				afterNull.splitAt(SHA1.AsBytes.ExpectedLength) match
				{
					case (SHA1.AsBytes(sha1), rest) => rest match
					{
						case IndexedSeq() => Some(Seq(new TreeEntry(mode, rawName, sha1)))
						case EncodedSeq(moreEntries @ _*) => Some(new TreeEntry(mode, rawName, sha1) +: moreEntries)
						case _ => None
					}
					case _ => None
				}
			case _ => None
		}
	}
}