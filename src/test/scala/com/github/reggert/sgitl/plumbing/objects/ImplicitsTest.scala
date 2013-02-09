package com.github.reggert.sgitl.plumbing.objects

import org.scalatest.Suite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import java.nio.ByteBuffer
import java.io.ByteArrayInputStream

@RunWith(classOf[JUnitRunner])
class ImplicitsTest extends Suite with ShouldMatchers
{
	def testByteBuffer2Iterator()
	{
		val array = (1 to 100).map(_.toByte).toArray
		val buffer = ByteBuffer.wrap(array)
		
		val iterator = Implicits.byteBuffer2Iterator(buffer)
		
		val derivedArray = iterator.toArray
		
		derivedArray should equal (array)
		buffer.hasRemaining() should be (false)
	}
	
	
	def testTraversable2ByteBuffer()
	{
		val values = (1 to 100).map(_.toByte)
		val traversable = values.iterator
		
		val buffer = Implicits.traversable2ByteBuffer(traversable)
		
		val copyBuffer = ByteBuffer.allocate(100)
		copyBuffer.put(buffer)
		copyBuffer.flip()
		
		val copiedValues = new Array[Byte](100)
		copyBuffer.get(copiedValues)
		
		copiedValues.toIndexedSeq should equal (values)
		copyBuffer.hasRemaining() should be (false)
		buffer.hasRemaining() should be (false)
	}
	
	
	def testInputStream2Iterator()
	{
		val values = (1 to 100).map(_.toByte)
		val inputStream = new ByteArrayInputStream(values.toArray)
		
		val iterator = Implicits.inputStream2Iterator(inputStream)
		val copiedValues = iterator ++: Seq.empty
		
		copiedValues should equal (values)
		iterator.hasNext should be (false)
		inputStream.read() should equal (-1)
	}
	
	
	def testIteratorInputStream()
	{
		val values = (1 to 100)
		val iterator = values.map(_.toByte).iterator
		
		val inputStream = new Implicits.IteratorInputStream(iterator)
		for (n <- values)
			inputStream.read() should equal (n)
		inputStream.read() should equal (-1)
		iterator.hasNext should be (false)
	}
	
	
	def testIterable2InputStream()
	{
		val values = (1 to 100)
		val iterable = values.map(_.toByte)
		
		val inputStream = Implicits.iterable2InputStream(iterable)
		for (n <- values)
			inputStream.read() should equal (n)
		inputStream.read() should equal (-1)
	}
}