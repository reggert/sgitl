package com.github.reggert.sgitl.plumbing.objects

import java.io.{ByteArrayOutputStream, OutputStreamWriter}

final case class TreeEntry(val fileMode : FileMode, val name : String, val referencedObjectId : SHA1) 
{
	// TODO: validate contents of 'name'
	
	def encoded : IndexedSeq[Byte] = 
	{
		val arrayStream = new ByteArrayOutputStream
		val writer = new OutputStreamWriter(arrayStream, ASCII)
		writer.write(fileMode.toString)
		writer.write(" ")
		writer.write(name)
		writer.flush()
		arrayStream.write(0)
		
		arrayStream.toByteArray ++: referencedObjectId.toBytes
	}
}