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

final class LooseBlob(
		override val content : IndexedSeq[Byte], 
		override val contentLength : Long
	) extends LooseObject
{
	override def typeId = LooseBlob.TypeId
}

object LooseBlob
{
	val TypeId = "blob"
}

object LooseObject
{
	private val ASCII = Charset.forName("US-ASCII")
	private val NullByte : Byte = 0
	private val Header = "(\\w+) +(\\d+)".r
	
	private object ContentLength
	{
		def unapply(s : String) : Option[Long] =
			try {Some(java.lang.Long.parseLong(s))}
			catch {case _ : NumberFormatException => None}
	}
	
	def read(input : Iterator[Byte]) : LooseObject =
	{
		val (encodedHeader, afterHeader) = new InflaterInputStream(input).span(_ != NullByte)
		if (afterHeader.hasNext)
		{
			val content = afterHeader.take(1).toIndexedSeq
			val contentLength = content.size.toLong
			ASCII.decode(encodedHeader).toString match
			{
				case Header(typeId, ContentLength(contentLength)) => typeId match
				{
					case LooseBlob.TypeId => new LooseBlob(content.toIndexedSeq, contentLength)
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
