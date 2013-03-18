package com.github.reggert.sgitl.plumbing.objects.loose

import com.github.reggert.sgitl.plumbing.objects.{ObjectType, GitBlob}

final case class LooseBlob(override val content : IndexedSeq[Byte]) extends LooseObject with GitBlob
