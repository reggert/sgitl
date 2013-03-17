package com.github.reggert.sgitl.plumbing.objects

import java.nio.charset.Charset
import java.io.InputStream
import java.util.zip.{Deflater,Inflater}
import java.util.zip.DeflaterInputStream
import java.util.zip.InflaterInputStream
import scala.collection.immutable.SortedSet

import Implicits._

abstract class LooseObject private[objects]()
{
	def objectType : ObjectType
	def content : Traversable[Byte]
	def contentLength : Long = content.size.toLong

	import LooseObject._
	
	final def header = StringBuilder.newBuilder.append(objectType).append(' ').append(contentLength).toString
	
	private final lazy val encodedHeader : Seq[Byte] = UTF8(header) :+ NullByte
	
	final def uncompressed = encodedHeader ++ content
	
	final def compressed(level : Int = Deflater.DEFAULT_COMPRESSION) = 
	{
		val deflater = new Deflater(level)
		val stream = new DeflaterInputStream(uncompressed, deflater)
		try {stream.toIndexedSeq}
		finally {stream.close(); deflater.end()}
	}
	
	final lazy val objectId = SHA1.digest(uncompressed)
}



object LooseObject
{
	private val HeaderPattern = """(\w+) +(\d+)""".r
	
	object HeaderLine
	{
		import java.lang.Long.parseLong
		
		def apply(objectType : ObjectType, contentLength : Long) : String =
		{
			require (contentLength >= 0L)
			StringBuilder.newBuilder.append(objectType).append(' ').append(contentLength).toString
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
	
	object Uncompressed
	{
		def apply(obj : LooseObject) = obj.uncompressed
	
		def unapply(input : Iterator[Byte]) : Option[LooseObject] = input.span(_ != NullByte) match
		{
			case (UTF8(HeaderLine(objectType, contentLength)), afterHeader) if contentLength <= Int.MaxValue && afterHeader.hasNext =>
			{
				val content = afterHeader.drop(1)
				objectType match
				{
					case ObjectType.Blob => 
						Some(new LooseBlob(content.take(contentLength.toInt).toIndexedSeq))
					case ObjectType.Tree => content.toIndexedSeq match
					{
						case TreeEntry.EncodedSeq(entries @ _*) => 
							Some(new LooseTree(SortedSet(entries : _*)))
						case _ => None
					}
					case ObjectType.Commit => content.toIndexedSeq match
					{
						case HeaderMessageObject.EncodedContent(headers, message) => 
							Some(new LooseCommit(headers, message))
						case _ => None
					}
					case ObjectType.Tag => content.toIndexedSeq match
					{
						case HeaderMessageObject.EncodedContent(headers, message) => 
							Some(new LooseTag(headers, message))
						case _ => None
					}
				}
			}
			case _ => None
		}
	}
		
		
	def readUncompressed(input : InputStream) : LooseObject = 
		readUncompressed(Implicits.inputStream2Iterator(input))
		
	def readUncompressed(input : Iterable[Byte]) : LooseObject =
		readUncompressed(input.iterator)
	
	def readUncompressed(input : Iterator[Byte]) : LooseObject = input match
	{
		case LooseObject.Uncompressed(obj) => obj
		case _ => throw new InvalidObjectFormatException("input did not match object format")
	}
	
	def read(input : InputStream) : LooseObject =
	{
		val inflater = new Inflater
		try {readUncompressed(new InflaterInputStream(input, inflater))}
		finally {inflater.end()}
	}
	
	def read(input : Iterator[Byte]) : LooseObject = read(Implicits.IteratorInputStream(input))
	
	def read(input : Iterable[Byte]) : LooseObject = read(input.iterator)
	
}
