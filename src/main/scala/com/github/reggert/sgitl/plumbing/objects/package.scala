package com.github.reggert.sgitl.plumbing

import java.nio.charset.Charset

package object objects 
{
	private[objects] val NullByte : Byte = 0
	private[objects] val UTF8 = new CharacterSet(Charset.forName("UTF-8"))
}