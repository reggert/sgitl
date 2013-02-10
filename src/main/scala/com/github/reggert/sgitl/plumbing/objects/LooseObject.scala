package com.github.reggert.sgitl.plumbing.objects

import java.nio.charset.Charset
import java.util.zip.Deflater
import java.util.zip.DeflaterInputStream
import java.util.zip.InflaterInputStream

import Implicits._

sealed abstract class LooseObject
{
	def typeId : String
	def content : Traversable[Byte]
	def contentLength : Long

	import LooseObject._
	
	final def header = typeId + ' ' + java.lang.Long.toString(contentLength)
	
	private final lazy val encodedHeader : Stream[Byte] = ASCII.encode(header) ++: Stream(NullByte)
	
	final def uncompressed : Stream[Byte] = encodedHeader ++ content
	
	final def compressed(level : Int = Deflater.DEFAULT_COMPRESSION) : Stream[Byte] = 
		new DeflaterInputStream(uncompressed, new Deflater(level)) ++: Stream.empty
	
	final lazy val objectId = SHA1.hashBytes(uncompressed)
}

final class LooseBlob(override val content : IndexedSeq[Byte]) extends LooseObject
{
	override def typeId = LooseBlob.TypeId
	override def contentLength = content.size
}

object LooseBlob
{
	val TypeId = "blob"
}

object LooseObject
{
	private val ASCII = Charset.forName("US-ASCII")
	private val NullByte : Byte = 0
	private val HeaderPattern = "(\\w+) +(\\d+)".r
	
	object HeaderLine
	{
		import java.lang.Long.parseLong
		def apply(objectType : ObjectType, contentLength : Long) : String =
		{
			require (contentLength >= 0L)
			(new StringBuilder).append(objectType).append(' ').append(contentLength).toString
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
	
	
	def read(input : Iterator[Byte]) : LooseObject =
	{
		val (encodedHeader, afterHeader) = new InflaterInputStream(input).span(_ != NullByte)
		if (afterHeader.hasNext)
		{
			val content = afterHeader.drop(1)
			ASCII.decode(encodedHeader).toString match
			{
				case HeaderLine(typeId, contentLength) => typeId match
				{
					case ObjectType.Blob =>
						if (contentLength > Int.MaxValue) // FIXME: remove this limitation
							throw new InvalidObjectFormatException("Exceeded maximum object size: " + contentLength)
						else
							new LooseBlob(content.take(contentLength.toInt).toIndexedSeq)
					case _ => throw new UnsupportedOperationException("Only blobs are supported")
				}
				case _ => throw new InvalidObjectFormatException("Invalid header")
			}
		}
		else
			throw new InvalidObjectFormatException("No null byte found")
	}
	
	def read(input : Iterable[Byte]) : LooseObject = read(input.iterator)
	
}
