package com.github.reggert.sgitl.plumbing.objects

import java.nio.ByteBuffer
import java.io.InputStream

trait Implicits 
{
	implicit def byteBuffer2Stream(buffer : ByteBuffer) : Stream[Byte] =
		Stream.continually(buffer.get()).take(buffer.remaining)
	
	implicit def stream2ByteBuffer(stream : Stream[Byte]) : ByteBuffer =
		ByteBuffer.wrap(stream.toArray)
		
	implicit def inputStream2Stream(inputStream : InputStream) : Stream[Byte] =
		Stream.continually(inputStream.read()).takeWhile(_ != -1).map(_.toByte)
	
	implicit class StreamInputStream(val stream : Stream[Byte]) extends InputStream
	{
		private var currentStream = stream
		
		override def read() =
			if (currentStream.isEmpty)
				-1
			else
			{
				val x = currentStream.head
				currentStream = currentStream.tail
				x
			}
	}
}


object Implicits extends Implicits