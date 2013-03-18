package com.github.reggert.sgitl.plumbing.io

import java.io.{BufferedInputStream, File, FileInputStream}

final class TraversableFile(val file : File) extends Traversable[Byte] 
{
	override def foreach[U](f: Byte => U): Unit =
	{
		val fileStream = new FileInputStream(file)
		try
		{
			val bufferedStream = new BufferedInputStream(fileStream)
			Implicits.inputStream2Iterator(bufferedStream).foreach(f)
		}
		finally {fileStream.close()}
	}
}