// Copyright (c) 2020 The DAML Authors. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.platform.sandbox.perf

import java.io.File

import akka.stream.Materializer
import com.digitalasset.daml.lf.archive.UniversalArchiveReader
import com.digitalasset.daml.lf.data.Ref
import com.digitalasset.grpc.adapter.ExecutionSequencerFactory
import com.digitalasset.ledger.api.domain.LedgerId
import com.digitalasset.ledger.api.testing.utils.Resource
import com.digitalasset.platform.common.LedgerIdMode
import com.digitalasset.platform.sandbox.config.SandboxConfig
import com.digitalasset.testing.postgresql.{PostgresFixture, PostgresResource}

object LedgerFactories {

  private def getPackageIdOrThrow(file: File): Ref.PackageId =
    UniversalArchiveReader().readFile(file).map(_.all.head._1).get

  private def sandboxConfig(jdbcUrl: Option[String], darFiles: List[File]) =
    SandboxConfig.default.copy(
      port = 0,
      damlPackages = darFiles,
      ledgerIdMode =
        LedgerIdMode.Static(LedgerId(Ref.LedgerString.assertFromString("ledger-server"))),
      jdbcUrl = jdbcUrl,
    )

  val mem = "InMemory"
  val sql = "Postgres"

  def createSandboxResource(store: String, darFiles: List[File])(
      implicit esf: ExecutionSequencerFactory,
      mat: Materializer): Resource[LedgerContext] = {
    def createResource(sandboxConfig: SandboxConfig): Resource[LedgerContext] =
      SandboxServerResource(sandboxConfig).map(
        new LedgerContext(_, darFiles.map(getPackageIdOrThrow)))

    store match {
      case `mem` =>
        createResource(sandboxConfig(None, darFiles))
      case `sql` =>
        new Resource[LedgerContext] {
          @volatile private var postgres: Resource[PostgresFixture] = _
          @volatile private var sandbox: Resource[LedgerContext] = _

          override def value: LedgerContext = sandbox.value

          override def setup(): Unit = {
            postgres = PostgresResource()
            postgres.setup()
            sandbox = createResource(sandboxConfig(Some(postgres.value.jdbcUrl), darFiles))
            sandbox.setup()
          }

          override def close(): Unit = {
            sandbox.close()
            postgres.close()
            sandbox = null
            postgres = null
          }
        }
    }

  }
}
