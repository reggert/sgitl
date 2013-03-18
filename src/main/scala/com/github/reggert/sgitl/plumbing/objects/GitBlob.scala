package com.github.reggert.sgitl.plumbing.objects

trait GitBlob extends GitObject 
{
	final override def objectType = ObjectType.Blob
}