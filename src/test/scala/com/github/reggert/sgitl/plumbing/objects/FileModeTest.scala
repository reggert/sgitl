package com.github.reggert.sgitl.plumbing.objects

import org.scalatest.Suite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FileModeTest extends Suite with ShouldMatchers
{
	
	def testUnapplySuccess()
	{
		FileMode.Tree.unapply("040000") should equal (Some(FileMode.Tree))
		FileMode.SymLink.unapply("120000") should equal (Some(FileMode.SymLink))
		FileMode.GitLink.unapply("160000") should equal (Some(FileMode.GitLink))
		FileMode.NonExecutableFile.unapply("100644") should equal (Some(FileMode.NonExecutableFile))
		FileMode.ExecutableFile.unapply("100755") should equal (Some(FileMode.ExecutableFile))
	}
	
	
	def testUnapplyFail()
	{
		FileMode.Tree.unapply("000000") should equal (None)
		FileMode.SymLink.unapply("000000") should equal (None)
		FileMode.GitLink.unapply("000000") should equal (None)
		FileMode.NonExecutableFile.unapply("000000") should equal (None)
		FileMode.ExecutableFile.unapply("000000") should equal (None)
	}
	
	
	def testAsStringUnapply()
	{
		FileMode.AsString.unapply("040000") should equal (Some(FileMode.Tree))
		FileMode.AsString.unapply("120000") should equal (Some(FileMode.SymLink))
		FileMode.AsString.unapply("160000") should equal (Some(FileMode.GitLink))
		FileMode.AsString.unapply("100644") should equal (Some(FileMode.NonExecutableFile))
		FileMode.AsString.unapply("100755") should equal (Some(FileMode.ExecutableFile))
	}
	
	
	def testAsBytesUnapply()
	{
		FileMode.AsBytes.unapply(UTF8("040000")) should equal (Some(FileMode.Tree))
		FileMode.AsBytes.unapply(UTF8("120000")) should equal (Some(FileMode.SymLink))
		FileMode.AsBytes.unapply(UTF8("160000")) should equal (Some(FileMode.GitLink))
		FileMode.AsBytes.unapply(UTF8("100644")) should equal (Some(FileMode.NonExecutableFile))
		FileMode.AsBytes.unapply(UTF8("100755")) should equal (Some(FileMode.ExecutableFile))
	}
}
