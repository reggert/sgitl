package com.github.reggert.sgitl.plumbing.objects


final class TreeEntry(val fileMode : FileMode, val rawName : Seq[Byte], val referencedObjectId : SHA1)
	extends Ordered[TreeEntry]
{
	import TreeEntry._
	
	require(!rawName.contains(NullByte))
	
	def encoded : IndexedSeq[Byte] = 
		(fileMode.toBytes :+ SpaceByte) ++ rawName ++ referencedObjectId.toBytes
		
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
	
}


object TreeEntry
{
	private val SpaceByte = 0x20.toByte  // the byte... from space!
	private val SlashByte = 0x2f.toByte
	
	private object ModeAndName
	{
		def unapply(encoded : Seq[Byte]) : Option[(FileMode, Seq[Byte])] = encoded.span(_ != SpaceByte) match
		{
			case (UTF8(FileMode.AsString(mode)), rawName) => Some(mode, rawName)
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
			case (ModeAndName(mode, rawName), NullByte +: afterNull) if afterNull.size >= SHA1.HashBytesLength =>
				afterNull.splitAt(SHA1.HashBytesLength) match
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