package com.github.reggert.sgitl.plumbing.io

import java.io.{BufferedInputStream, File, FileInputStream}
import scala.annotation.tailrec

final class TraversableFile private(val file : File, val startOffset : Long, val limit : Option[Long]) 
	extends Traversable[Byte] 
{
	
	/**
	 * Opens the file and seeks to the startOffset, then invokes the specified 
	 * function on the file stream.
	 */
	private[this] def withStream[A](f : BufferedInputStream => A) : A =
	{
		val fileStream = new FileInputStream(file)
		try 
		{
			val bufferedStream = new BufferedInputStream(fileStream)
			
			@tailrec
			def skipRecursively(bytesToSkip : Long) : Unit = 
				bufferedStream.skip(bytesToSkip) match
				{
					case 0 => ()
					case bytesSkipped => skipRecursively(bytesToSkip - bytesSkipped)
				}
			
			skipRecursively(startOffset)
			f(bufferedStream)
		}
		finally {fileStream.close()}
	}
	
	
	override def foreach[U](f : Byte => U) : Unit = withStream 
	{bufferedStream =>
			
		@tailrec			
		def readRecursively(bytesRead : Long) : Unit = limit match
		{
			case Some(n) if (bytesRead >= n) => ()
			case _ => bufferedStream.read() match
			{
				case -1 => ()
				case n => f(n.toByte); readRecursively(bytesRead + 1L)
			}
		}
		readRecursively(0)
	}
	
	
	def dropLong(n : Long) = 
		if (n <= 0)
			this
		else
			new TraversableFile(file, startOffset + n, limit.map(_ - n))
	
	
	override def drop(n: Int) = dropLong(n)
	
	
	def takeLong(n : Long) = 
		if (n <= 0)
			new TraversableFile(file, startOffset, Some(0L))
		else
			new TraversableFile(file, startOffset, Some(limit.map(l => Math.min(n, l)).getOrElse(n.toLong)))
	
	
	override def take(n : Int) = takeLong(n)
	
	
	def splitAtLong(n : Long) = (takeLong(n), dropLong(n))
	
	
	override def splitAt(n: Int) = splitAtLong(n)

	
	override def span(p : Byte => Boolean) = withStream
	{bufferedStream =>
		
		@tailrec
		def findSpanOffset(offset : Long) : Long = limit match
		{
			case Some(l) if offset >= l => offset
			case _ => bufferedStream.read() match
			{
				case -1 => offset
				case n if p(n.toByte) => offset
				case _ => findSpanOffset(offset + 1)
			}
		}
		val spanOffset = findSpanOffset(0)
		
		(takeLong(spanOffset), dropLong(spanOffset))
	}
	
	override def takeWhile(p : Byte => Boolean) = span(p)._1
	
	override def dropWhile(p : Byte => Boolean) = span(p)._2
	
}