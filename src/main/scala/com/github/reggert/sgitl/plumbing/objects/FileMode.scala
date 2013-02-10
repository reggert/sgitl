package com.github.reggert.sgitl.plumbing.objects

sealed abstract class FileMode 
{
	def toInt : Int
	
	def unapply(s : String) : Option[this.type] = 
		if (s == toString) Some(this) else None
}





object FileMode
{
	def apply(fileMode : FileMode) = fileMode.toString
	
	def unapply(s : String) : Option[FileMode] = 
		NonExecutableFile.unapply(s) orElse
		Tree.unapply(s) orElse
		GitLink.unapply(s) orElse
		ExecutableFile.unapply(s) orElse
		SymLink.unapply(s)
	
	object Tree extends FileMode
	{
		override def toString = "040000"
		override def toInt = 0x40000
	}


	object SymLink extends FileMode
	{
		override def toString = "120000"
		override def toInt = 0x120000
	}
	
	
	object GitLink extends FileMode
	{
		override def toString = "160000"
		override def toInt = 0x160000
	}
	
	
	abstract sealed class RegularFile extends FileMode
	{
		def isExecutable : Boolean
	}
	
	
	object NonExecutableFile extends RegularFile
	{
		override def isExecutable = false
		override def toString = "100644"
		override def toInt = 0x100644
	}
	
	
	object ExecutableFile extends RegularFile
	{
		override def isExecutable = true
		override def toString ="100755" 
		override def toInt = 0x100755
	}
}