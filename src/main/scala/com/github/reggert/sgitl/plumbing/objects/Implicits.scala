package com.github.reggert.sgitl.plumbing.objects

import java.nio.ByteBuffer
import java.io.InputStream

trait Implicits 
{
	implicit def byteBuffer2Iterator(buffer : ByteBuffer) : Iterator[Byte] =
		Iterator.continually(buffer.get()).take(buffer.remaining)
	
	implicit def traversable2ByteBuffer(traversable : TraversableOnce[Byte]) : ByteBuffer =
		ByteBuffer.wrap(traversable.toArray)
		
	implicit def inputStream2Iterator(inputStream : InputStream) : Iterator[Byte] =
		Iterator.continually(inputStream.read()).takeWhile(_ != -1).map(_.toByte)
	
	implicit class IteratorInputStream(val iterator : Iterator[Byte]) extends InputStream
	{
		override def read() =
			if (!iterator.hasNext)
				-1
			else
				iterator.next
	}

	implicit def iterable2InputStream(iterable : Iterable[Byte]) =
		new IteratorInputStream(iterable.iterator)		
}


object Implicits extends Implicits
