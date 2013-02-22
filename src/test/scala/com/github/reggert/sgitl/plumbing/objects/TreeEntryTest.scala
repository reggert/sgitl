package com.github.reggert.sgitl.plumbing.objects

import org.scalatest.Suite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TreeEntryTest extends Suite with ShouldMatchers
{
	def testEncoded()
	{
		val sha1 = SHA1.digest(UTF8("blah blah blah"))
		val treeEntry = new TreeEntry(FileMode.NonExecutableFile, UTF8("test.txt"), sha1)
		
		treeEntry.encoded should equal ((UTF8("100644 test.txt") :+ 0.toByte) ++ sha1.toBytes)
	}
	
	
	def testExtraction()
	{
		val sha1 = SHA1.digest(UTF8("blah blah blah"))
		val input = (UTF8("100644 test.txt") :+ 0.toByte) ++ sha1.toBytes
		
		TreeEntry.Encoded.unapply(input) should equal (Some(new TreeEntry(FileMode.NonExecutableFile, UTF8("test.txt"), sha1)))
	}
	
	def testSeqExtraction()
	{
		val sha1a = SHA1.digest(UTF8("blah blah blah"))
		val sha1b = SHA1.digest(UTF8("zzzzzzzzzzzzzzzzzzz"))
		val input = ((UTF8("100644 test.txt") :+ 0.toByte) ++ sha1a.toBytes) ++ (((UTF8("040000 foo") :+ 0.toByte) ++ sha1b.toBytes))
		
		TreeEntry.EncodedSeq.unapplySeq(input) should equal (
				Some(
						Seq(
								new TreeEntry(FileMode.NonExecutableFile, UTF8("test.txt"), sha1a),
								new TreeEntry(FileMode.Tree, UTF8("foo"), sha1b)
							)
					)
			)
	}
}
