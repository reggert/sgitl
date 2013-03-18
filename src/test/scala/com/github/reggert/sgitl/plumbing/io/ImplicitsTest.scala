package com.github.reggert.sgitl.plumbing.io

import org.scalatest.Suite
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import java.nio.ByteBuffer
import java.io.ByteArrayInputStream

@RunWith(classOf[JUnitRunner])
class ImplicitsTest extends Suite with ShouldMatchers
{
	def testByteBuffer2Iterator()
	{
		val input = (1 to 100).map(_.toByte)
		val buffer = ByteBuffer.wrap(input.toArray)
		
		val iterator = Implicits.byteBuffer2Iterator(buffer)
		
		val output = iterator.toIndexedSeq
		
		output should equal (input)
		buffer.hasRemaining() should be (false)
	}
	
	
	def testByteBuffer2IteratorTakeWhile()
	{
		val input = (1 to 100).map(_.toByte)
		val buffer = ByteBuffer.wrap(input.toArray)
		
		val iterator = Implicits.byteBuffer2Iterator(buffer)
		
		val output = iterator.takeWhile(_ <= 50).toIndexedSeq
		
		output should equal (input.takeWhile(_ <= 50))
		buffer.remaining should equal (49)
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
		val copiedValues = iterator.toSeq
		
		copiedValues should equal (values)
		iterator.hasNext should be (false)
		inputStream.read() should equal (-1)
	}
	
	
	def testInputStream2IteratorTakeWhile()
	{
		val values = (1 to 100).map(_.toByte)
		val inputStream = new ByteArrayInputStream(values.toArray)
		
		val iterator = Implicits.inputStream2Iterator(inputStream)
		val copiedValues = iterator.takeWhile(_ <= 50).toIndexedSeq
		
		copiedValues should equal (values.takeWhile(_ <= 50))
		iterator.hasNext should be (true)
		inputStream.read() should equal (53)
	}
	
	
	def testInputStream2IteratorSpan()
	{
		val values = (1 to 100).map(_.toByte)
		val inputStream = new ByteArrayInputStream(values.toArray)
		
		val iterator = Implicits.inputStream2Iterator(inputStream)
		val (leftIterator, rightIterator) = iterator.span(_ <= 50)
		rightIterator.hasNext should be (true)
		leftIterator.hasNext should be (true)
		val (leftCopy, rightCopy) = (leftIterator.toSeq, rightIterator.toSeq)
		
		leftCopy should equal (1 to 50)
		rightCopy should equal (51 to 100)
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
	
	
	def testInputStream2Iterator2InputStream2Iterator()
	{
		val values = (1 to 200).map(_.toByte)
		val originalInputStream = new ByteArrayInputStream(values.toArray)
		val iterator1 = Implicits.inputStream2Iterator(originalInputStream)
		val wrapperInputStream = new Implicits.IteratorInputStream(iterator1)
		val iterator2 = Implicits.inputStream2Iterator(wrapperInputStream)
		val (first100, second100) = iterator2.span(_ <= 100)
		val copiedValues = first100.toIndexedSeq ++ second100.drop(1).toIndexedSeq
		copiedValues should equal (values.filterNot(_ == 101))
	}
	// TODO: test repeated wrapping of iterators and InputStreams
}