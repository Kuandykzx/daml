// Copyright (c) 2020 The DAML Authors. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.platform.store

import java.time.Instant

import com.digitalasset.daml.lf.data.Ref.Party
import com.digitalasset.daml.lf.transaction.Node.KeyWithMaintainers
import com.digitalasset.daml.lf.value.Value
import com.digitalasset.daml.lf.value.Value.{AbsoluteContractId, ContractInst, VersionedValue}
import com.digitalasset.ledger.{EventId, TransactionId, WorkflowId}

/** A contract that is part of the [[ActiveLedgerState]].
  * Depending on where the contract came from, other metadata may be available.
  */
sealed abstract class Contract {
  def id: Value.AbsoluteContractId

  def contract: ContractInst[VersionedValue[AbsoluteContractId]]

  /** For each party, the transaction id at which the contract was divulged */
  def divulgences: Map[Party, TransactionId]

  /** Returns the new divulgences after the contract has been divulged to the given parties at the given transaction */
  def divulgeTo(
      parties: Set[Party],
      transactionId: TransactionId
  ): Map[Party, TransactionId] =
    parties.foldLeft(divulgences)((m, e) => if (m.contains(e)) m else m + (e -> transactionId))
}

object Contract {

  /**
    * For divulged contracts, we only know their contract argument, but no other metadata.
    * Note also that a ledger node may not be notified when a divulged contract gets archived.
    *
    * These contracts are only used for transaction validation, they are not part of the active contract set.
    */
  final case class DivulgedContract(
      id: Value.AbsoluteContractId,
      contract: ContractInst[VersionedValue[AbsoluteContractId]],
      /** For each party, the transaction id at which the contract was divulged */
      divulgences: Map[Party, TransactionId],
  ) extends Contract

  /**
    * For active contracts, we know all metadata.
    */
  final case class ActiveContract(
      id: Value.AbsoluteContractId,
      let: Instant, // time when the contract was committed
      transactionId: TransactionId, // transaction id where the contract originates
      eventId: EventId,
      workflowId: Option[WorkflowId], // workflow id from where the contract originates
      contract: ContractInst[VersionedValue[AbsoluteContractId]],
      witnesses: Set[Party],
      divulgences: Map[Party, TransactionId], // for each party, the transaction id at which the contract was divulged
      key: Option[KeyWithMaintainers[VersionedValue[Nothing]]],
      signatories: Set[Party],
      observers: Set[Party],
      agreementText: String
  ) extends Contract

}
