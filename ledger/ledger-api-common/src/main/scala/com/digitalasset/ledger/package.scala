// Copyright (c) 2020 The DAML Authors. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset

import com.digitalasset.daml.lf.data.Ref.LedgerString

package object ledger {

  /** Identifiers used to correlate submission with results. */
  type CommandId = LedgerString
  val CommandId: LedgerString.type = LedgerString

  /** Identifiers used for correlating submission with a workflow.  */
  type WorkflowId = LedgerString
  val WorkflowId: LedgerString.type = LedgerString

  /** Identifiers for submitting client applications. */
  type ApplicationId = LedgerString
  val ApplicationId: LedgerString.type = LedgerString

  type EventId = LedgerString
  val EventId: LedgerString.type = LedgerString

  val TransactionId: LedgerString.type = LedgerString
  type TransactionId = LedgerString

}
