package com.github.reggert.sgitl.plumbing.objects


final case class TreeEntry private(val fileMode : FileMode, val name : String, val referencedObjectId : SHA1) 
{
	// TODO: validate contents of 'name'
	
	def encoded : IndexedSeq[Byte] = 
		UTF8(StringBuilder.newBuilder.append(fileMode).append(' ').append(name).toString) ++ 
		referencedObjectId.toBytes
}


object TreeEntry
{
	val ModeAndName = "(\\d+) (.*)".r
	
	object Encoded
	{
		def apply(treeEntry : TreeEntry) = treeEntry.encoded
		
		def unapply(encoded : Seq[Byte]) : Option[TreeEntry] = encoded.span(_ != NullByte) match
		{
			case (UTF8(ModeAndName(FileMode.AsString(mode), name)), NullByte +: SHA1.DigestBytes(sha1)) => 
				Some(TreeEntry(mode, name, sha1))
			case _ => None
		}
	}
	
	
	object EncodedSeq
	{
		def apply(treeEntries : Seq[TreeEntry]) = treeEntries.toIndexedSeq.flatMap(_.encoded)
		
		def unapplySeq(encoded : IndexedSeq[Byte]) : Option[Seq[TreeEntry]] = encoded.span(_ != NullByte) match
		{
			case (UTF8(ModeAndName(FileMode.AsString(mode), name)), NullByte +: afterNull) if afterNull.size >= SHA1.HashBytesLength =>
				afterNull.splitAt(SHA1.HashBytesLength) match
				{
					case (SHA1.DigestBytes(sha1), IndexedSeq()) => Some(Seq(TreeEntry(mode, name, sha1)))
					case (SHA1.DigestBytes(sha1), EncodedSeq(rest @ _*)) => Some(TreeEntry(mode, name, sha1) +: rest)
					case _ => None
				}
			case _ => None
		}
	}
}