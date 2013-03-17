package com.github.reggert.sgitl.plumbing.objects

import org.scalatest.Suite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import java.io.{ByteArrayOutputStream, ByteArrayInputStream, OutputStreamWriter}
import java.util.zip.DeflaterOutputStream
import java.nio.charset.Charset
import scala.collection.immutable.SortedSet

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
	
	
	
	def testReadBlob()
	{
		val content = "This is some test data.  This is only a test.  Don't get too excited."
		val encodedContent = UTF8(content)
		val header = s"blob ${encodedContent.length}\u0000"
		val encodedHeader = UTF8(header)
		val data = encodedHeader ++ encodedContent
		
		val arrayOutputStream = new ByteArrayOutputStream
		val deflaterOutputStream = new DeflaterOutputStream(arrayOutputStream)
		deflaterOutputStream.write(data.toArray)
		deflaterOutputStream.close()
		val compressedData = arrayOutputStream.toByteArray
		
		val arrayInputStream = new ByteArrayInputStream(compressedData)
		val obj = LooseObject.read(arrayInputStream)
		obj.objectType should be (ObjectType.Blob)
		obj.contentLength should equal (encodedContent.length)
		obj.content should equal (encodedContent)
	}
	
	
	
	def testWriteBlob()
	{
		val content = "This is some test data.  This is only a test.  Don't get too excited."
		val encodedContent = UTF8(content)
		val blob = new LooseBlob(encodedContent)
		
		val compressedData = blob.compressed().toIndexedSeq
		
		val copiedBlob = LooseObject.read(compressedData)
		copiedBlob should equal(blob)
	}
	
	
	def testReadWriteTree()
	{
		val entries = SortedSet(
				new TreeEntry(FileMode.NonExecutableFile, UTF8("file1.foo"), SHA1.FromBytes(Array.fill(SHA1.AsBytes.ExpectedLength)(1.toByte))),
				new TreeEntry(FileMode.ExecutableFile, UTF8("file2.sh"), SHA1.FromBytes(Array.fill(SHA1.AsBytes.ExpectedLength)(2.toByte))),
				new TreeEntry(FileMode.GitLink, UTF8("submodule1"), SHA1.FromBytes(Array.fill(SHA1.AsBytes.ExpectedLength)(3.toByte))),
				new TreeEntry(FileMode.SymLink, UTF8("file3.txt"), SHA1.FromBytes(Array.fill(SHA1.AsBytes.ExpectedLength)(4.toByte))),
				new TreeEntry(FileMode.Tree, UTF8("subdir1"), SHA1.FromBytes(Array.fill(SHA1.AsBytes.ExpectedLength)(5.toByte)))
			)
		val tree = new LooseTree(entries)
		
		val uncompressedData = tree.uncompressed
		val compressedData = tree.compressed()
		
		val copiedUncompressedTree = LooseObject.readUncompressed(uncompressedData)
		copiedUncompressedTree should equal(tree)
		
		val copiedCompressedTree = LooseObject.read(compressedData)
		copiedCompressedTree should equal(tree)
	}
	
	
	private def randomHash = 
	{
		import scala.util.Random
		val bytes = new Array[Byte](SHA1.AsBytes.ExpectedLength)
		Random.nextBytes(bytes)
		SHA1.FromBytes(bytes)
	}
	
	
	def testReadWriteCommit()
	{
		val treeHash, parentHash = randomHash
		
		val originalCommit = new LooseCommit(
				tree=treeHash, 
				parents=Seq(parentHash), 
				author="Nobody <nobody@nowhere.nil> 1234567890 +0000",
				committer="Nobody <nobody@nowhere.nil> 1234567890 +0000",
				message="This is a test."
			)
		
		val uncompressedData = originalCommit.uncompressed
		val compressedData = originalCommit.compressed()
		
		val copiedUncompressedCommit = LooseObject.readUncompressed(uncompressedData)
		copiedUncompressedCommit should equal (originalCommit)
		
		val copiedCompressedCommit = LooseObject.read(compressedData)
		copiedCompressedCommit should equal (originalCommit)
	}
	
	
	
	def testReadWriteTag()
	{
		val targetHash = randomHash
		
		val originalTag = new LooseTag(
				taggedObjectId = targetHash, 
				taggedObjectType = ObjectType.Commit, 
				name="Test Tag",
				tagger="Nobody <nobody@nowhere.nil> 1234567890 +0000",
				message="This is a test."
			)
		
		val uncompressedData = originalTag.uncompressed
		val compressedData = originalTag.compressed()
		
		val copiedUncompressedTag = LooseObject.readUncompressed(uncompressedData)
		copiedUncompressedTag should equal (originalTag)
		
		val copiedCompressedTag = LooseObject.read(compressedData)
		copiedCompressedTag should equal (originalTag)
	}
}