// Copyright (c) 2020 The DAML Authors. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.testing.postgresql

import java.nio.file.{Path, Paths}

import com.digitalasset.daml.bazeltools.BazelRunfiles.rlocation

private case class Tool private[postgresql] (name: String) {

  import Tool._

  def path: Path = rlocation(binPath.resolve(name + binExtension))
}

private[postgresql] object Tool {
  private val binPath = Paths.get("external", "postgresql_dev_env", "bin")

  private val binExtension =
    if (isWindows)
      ".exe"
    else
      ""

  val createdb = Tool("createdb")
  val initdb = Tool("initdb")
  val pg_ctl = Tool("pg_ctl")
}
