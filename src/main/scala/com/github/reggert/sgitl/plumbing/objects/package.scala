package com.github.reggert.sgitl.plumbing

import java.nio.charset.Charset

package object objects 
{
	val NullByte : Byte = 0
	val UTF8 = new CharacterSet(Charset.forName("UTF-8"))
	val SystemEncoding = new CharacterSet(Charset.defaultCharset)
}