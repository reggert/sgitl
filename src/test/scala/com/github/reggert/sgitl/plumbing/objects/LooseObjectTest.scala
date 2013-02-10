package com.github.reggert.sgitl.plumbing.objects

import org.scalatest.Suite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class LooseObjectTest extends Suite with ShouldMatchers
{

	def testHeaderLineApply()
	{
		val headerLine = LooseObject.HeaderLine(ObjectType.Blob, 1234L)
	}
}