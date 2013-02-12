package com.github.reggert.sgitl.plumbing.objects


final case class TreeEntry private(val fileMode : FileMode, val rawName : Seq[Byte], val referencedObjectId : SHA1) 
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
}


object TreeEntry
{
	private val SpaceByte = 0x20.toByte  // the byte... from space!
	
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
			case (ModeAndName(mode, rawName), NullByte +: SHA1.DigestBytes(sha1)) => 
				Some(TreeEntry(mode, rawName, sha1))
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
					case (SHA1.DigestBytes(sha1), rest) => rest match
					{
						case IndexedSeq() => Some(Seq(TreeEntry(mode, rawName, sha1)))
						case EncodedSeq(moreEntries @ _*) => Some(TreeEntry(mode, rawName, sha1) +: moreEntries)
						case _ => None
					}
					case _ => None
				}
			case _ => None
		}
	}
}