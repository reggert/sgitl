package com.github.reggert.sgitl.plumbing.objects

import java.util.zip.{Deflater,Inflater}
import java.util.zip.DeflaterInputStream
import java.util.zip.InflaterInputStream

trait GitObject 
{
	def objectType : ObjectType
	def content : Traversable[Byte]
	def contentLength : Long = content.size.toLong
	
	def objectId : SHA1
}


object GitObject
{
	val HeaderPattern = """(\w+) +(\d+)""".r
	
	object HeaderLine
	{
		import java.lang.Long.parseLong
		
		def apply(objectType : ObjectType, contentLength : Long) : String =
		{
			require (contentLength >= 0L)
			s"$objectType $contentLength"
		}
		
		def unapply(s : String) : Option[(ObjectType, Long)] = s match
		{
			case HeaderPattern(typeId, contentLengthString) => typeId match
			{
				case ObjectType(objectType) => 
					try {Some(objectType, parseLong(contentLengthString))}
					catch {case _ : NumberFormatException => None}
				case _ => None
			}
			case _ => None
		}
	}
}