package com.github.reggert.sgitl.plumbing.objects

import java.nio.charset.Charset
import java.util.zip.Deflater
import java.util.zip.DeflaterInputStream
import java.util.zip.InflaterInputStream

import Implicits.StreamInputStream
import Implicits.byteBuffer2Stream
import Implicits.inputStream2Stream
import Implicits.stream2ByteBuffer

sealed abstract class LooseObject
{
	def typeId : String
	def content : Traversable[Byte]
	def contentLength : Long

	import LooseObject._
	
	final def header = typeId + ' ' + java.lang.Long.toString(contentLength)
	
	private final lazy val encodedHeader : Stream[Byte] = ASCII.encode(header) :+ NullByte
	
	final def uncompressed : Stream[Byte] = encodedHeader ++ content
	
	final def compressed(level : Int = Deflater.DEFAULT_COMPRESSION) : Stream[Byte] = 
		new DeflaterInputStream(uncompressed, new Deflater(level))
	
	final lazy val objectId = SHA1.hashBytes(uncompressed)
}

final class LooseBlob(
		override val content : Traversable[Byte], 
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
	
	def fromStream(stream : Stream[Byte]) : LooseObject =
		new InflaterInputStream(stream).span(_ != NullByte) match
		{
			case (encodedHeader, NullByte #:: content) =>
				ASCII.decode(encodedHeader).toString match
				{
					case Header(typeId, ContentLength(contentLength)) =>
						typeId match
						{
							case LooseBlob.TypeId => new LooseBlob(content, contentLength)
							case _ => throw new UnsupportedOperationException("Only blobs are supported")
						}
					case _ =>
						throw new InvalidObjectFormatException("Invalid header")
				}
			case _ => throw new InvalidObjectFormatException("No null byte found")
		}
	
}