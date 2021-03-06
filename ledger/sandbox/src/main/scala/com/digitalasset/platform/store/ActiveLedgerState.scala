// Copyright (c) 2020 The DAML Authors. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.platform.store

import java.time.Instant

import com.daml.ledger.participant.state.v1.AbsoluteContractInst
import com.digitalasset.daml.lf.data.Ref.Party
import com.digitalasset.daml.lf.data.Relation.Relation
import com.digitalasset.daml.lf.transaction.Node.GlobalKey
import com.digitalasset.daml.lf.value.Value
import com.digitalasset.daml.lf.value.Value.AbsoluteContractId
import com.digitalasset.ledger.TransactionId
import com.digitalasset.platform.store.Contract.ActiveContract

sealed abstract class LetLookup

/** Contract exists, but contract LET is unknown (e.g., a divulged contract) */
case object LetUnknown extends LetLookup

/** Contract exists with the given LET */
final case class Let(instant: Instant) extends LetLookup

/**
  * An abstract representation of the active ledger state:
  * - Active contracts
  * - Divulged contracts
  * - Contract keys
  * - Known parties
  *
  * The active ledger state is used for validating transactions,
  * see [[ActiveLedgerStateManager]].
  *
  * The active ledger state could be derived from the transaction stream,
  * we keep track of it explicitly for performance reasons.
  */
trait ActiveLedgerState[+Self] { this: ActiveLedgerState[Self] =>

  /** Callback to query an active or divulged contract, used for transaction validation
    * Returns:
    * - None if the contract does not exist
    * - Some(LetUnknown) if the contract exists, but its LET is unknown (i.e., a divulged contract)
    * - Some(Let(_)) if the contract exists and its LET is known
    * */
  def lookupContractLet(cid: AbsoluteContractId): Option[LetLookup]

  /** Callback to query a contract by key, used for validating NodeLookupByKey nodes.
    * */
  def lookupContractByKey(key: GlobalKey): Option[AbsoluteContractId]

  /** Called when a new contract is created */
  def addContract(c: ActiveContract, keyO: Option[GlobalKey]): Self

  /** Called when the given contract is archived */
  def removeContract(cid: AbsoluteContractId): Self

  /** Called once for each transaction with the set of parties found in that transaction.
    * As the sandbox has an open world of parties, any party name mentioned in a transaction
    * will implicitly add that name to the list of known parties.
    */
  def addParties(parties: Set[Party]): Self

  /** Note that this method is about divulging contracts _that have already been
    * committed_. Implementors of [[ActiveLedgerState]] must take care to also store
    * divulgence information already present in `ActiveContract#divulgences` in the `addContract`
    * method.
    */
  def divulgeAlreadyCommittedContracts(
      transactionId: TransactionId,
      global: Relation[AbsoluteContractId, Party],
      referencedContracts: List[(Value.AbsoluteContractId, AbsoluteContractInst)]): Self
}
