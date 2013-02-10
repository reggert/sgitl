package com.github.reggert.sgitl.plumbing.objects

import org.scalatest.Suite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import java.io.{ByteArrayOutputStream, ByteArrayInputStream, OutputStreamWriter}
import java.util.zip.DeflaterOutputStream
import java.nio.charset.Charset

@RunWith(classOf[JUnitRunner])
class LooseObjectTest extends Suite with ShouldMatchers
{

	def testHeaderLineApply()
	{
		val headerLine = LooseObject.HeaderLine(ObjectType.Blob, 1234L)
		headerLine should equal ("blob 1234")
	}
	
	def testHeaderLineUnapply()
	{
		import LooseObject.HeaderLine
		withClue("blob 1234")
		{
			HeaderLine.unapply("blob 1234") should equal (Some(ObjectType.Blob, 1234L))
		}
		withClue("tree 567")
		{
			HeaderLine.unapply("tree 567") should equal (Some(ObjectType.Tree, 567L))
		}
		withClue("commit 89")
		{
			HeaderLine.unapply("commit 89") should equal (Some(ObjectType.Commit, 89L))
		}
		withClue("tag 987")
		{
			HeaderLine.unapply("tag 987") should equal (Some(ObjectType.Tag, 987L))
		}
		withClue("invalid 123")
		{
			HeaderLine.unapply("invalid 123") should equal (None)
		}
		withClue("blob xyz")
		{
			HeaderLine.unapply("blob xyz") should equal(None)
		}
	}
	
	private def string2ByteArray(s : String) =
	{
		val outputStream = new ByteArrayOutputStream
		val writer = new OutputStreamWriter(outputStream, Charset.forName("US-ASCII"))
		writer.write(s)
		writer.close()
		outputStream.toByteArray
	}
	
	
	
	def testReadBlob()
	{
		val content = "This is some test data.  This is only a test.  Don't get too excited."
		val encodedContent = string2ByteArray(content)
		val header = "blob " + encodedContent.length + "\u0000"
		val encodedHeader = string2ByteArray(header)
		val data = encodedHeader ++ encodedContent
		
		val arrayOutputStream = new ByteArrayOutputStream
		val deflaterOutputStream = new DeflaterOutputStream(arrayOutputStream)
		deflaterOutputStream.write(data)
		deflaterOutputStream.close()
		val compressedData = arrayOutputStream.toByteArray
		
		val arrayInputStream = new ByteArrayInputStream(compressedData)
		val obj = LooseObject.read(Implicits.inputStream2Iterator(arrayInputStream))
		obj.objectType should be (ObjectType.Blob)
		obj.contentLength should equal (encodedContent.length)
		obj.content should equal (encodedContent.toIndexedSeq)
	}
}