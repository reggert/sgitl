package com.github.reggert.sgitl.plumbing.objects

import java.nio.charset.Charset
import java.io.InputStream
import java.util.zip.{Deflater,Inflater}
import java.util.zip.DeflaterInputStream
import java.util.zip.InflaterInputStream
import scala.collection.immutable.SortedSet

import Implicits._

sealed abstract class LooseObject
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


final case class LooseBlob(override val content : IndexedSeq[Byte]) extends LooseObject 
{
	override def objectType = ObjectType.Blob
}


final case class LooseTree(val entries : SortedSet[TreeEntry]) extends LooseObject
{
	override def objectType = ObjectType.Tree
	override lazy val content = entries.toIndexedSeq.flatMap(_.encoded)
}



object LooseObject
{
	private val HeaderPattern = "(\\w+) +(\\d+)".r
	
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
	
	private def parseHeader(encodedHeader : TraversableOnce[Byte]) = encodedHeader match
	{
		case UTF8(HeaderLine(objectType, contentLength)) => (objectType, contentLength)
		case _ => throw new InvalidObjectFormatException("invalidHeader")
	}
		
		
	def readUncompressed(input : InputStream) : LooseObject = 
		readUncompressed(Implicits.inputStream2Iterator(input))
		
	def readUncompressed(input : Iterable[Byte]) : LooseObject =
		readUncompressed(input.iterator)
	
	def readUncompressed(input : Iterator[Byte]) : LooseObject =
	{
		val (encodedHeader, afterHeader) = input.span(_ != NullByte)
		if (afterHeader.hasNext)
		{
			val (objectType, contentLength) = parseHeader(encodedHeader)
			if (contentLength > Int.MaxValue) // FIXME: remove this limitation
				throw new InvalidObjectFormatException("Exceeded maximum object size: " + contentLength)
			val content = afterHeader.drop(1).take(contentLength.toInt).toIndexedSeq
			if (content.length != contentLength)
				throw new InvalidObjectFormatException("content.length != contentLength")
			objectType match
			{
				case ObjectType.Blob => new LooseBlob(content)
				case ObjectType.Tree => content match
				{
					case TreeEntry.EncodedSeq(entries @ _*) => new LooseTree(SortedSet(entries : _*))
					case _ => throw new InvalidObjectFormatException("Invalid tree object")
				}
				case _ => throw new UnsupportedOperationException("Only blobs and trees are supported")
			}
		}
		else
			throw new InvalidObjectFormatException("No null byte found")
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
