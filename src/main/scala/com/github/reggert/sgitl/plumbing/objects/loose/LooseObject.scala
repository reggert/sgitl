package com.github.reggert.sgitl.plumbing.objects.loose

import java.io.InputStream
import java.util.zip.{Deflater,Inflater}
import java.util.zip.DeflaterInputStream
import java.util.zip.InflaterInputStream
import scala.collection.immutable.SortedSet
import com.github.reggert.sgitl.plumbing.objects.GitObject
import com.github.reggert.sgitl.plumbing.objects.HeaderMessageObject
import com.github.reggert.sgitl.plumbing.io.Implicits
import com.github.reggert.sgitl.plumbing.objects.InvalidObjectFormatException
import com.github.reggert.sgitl.plumbing.objects.ObjectType
import com.github.reggert.sgitl.plumbing.objects.SHA1
import com.github.reggert.sgitl.plumbing.objects.TreeEntry
import com.github.reggert.sgitl.plumbing.objects.{NullByte,UTF8}
import java.lang.Long.parseLong

abstract class LooseObject private[objects]() extends GitObject
{
	import GitObject._
	
	final override lazy val objectId = SHA1.digest(uncompressed)
	
	final def header = HeaderLine(objectType, contentLength)
	
	protected final lazy val encodedHeader : Seq[Byte] = UTF8(header) :+ NullByte
	
	final def uncompressed = encodedHeader ++ content
	
	final def compressed(level : Int = Deflater.DEFAULT_COMPRESSION) = 
	{
		import Implicits._
		val deflater = new Deflater(level)
		val stream = new DeflaterInputStream(uncompressed, deflater)
		try {stream.toIndexedSeq}
		finally {stream.close(); deflater.end()}
	}
}


object LooseObject
{
	import GitObject._
	
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
