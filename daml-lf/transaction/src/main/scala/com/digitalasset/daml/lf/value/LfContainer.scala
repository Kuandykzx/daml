package com.digitalasset.daml.lf.value

import com.digitalasset.daml.lf.data.Ref
import com.digitalasset.daml.lf.data.Ref.ContractIdString
import com.digitalasset.daml.lf.value.Value.AbsoluteContractId

import scala.language.higherKinds
import scala.util.control.NoStackTrace

sealed trait CidResolver[-A, +B, X] {
  private[value] def resolve(x: X): A => B
}

object CidResolver {

  sealed class Ball[X] {
    case class Ball(x: X) extends Throwable with NoStackTrace
    private[CidResolver] def throws(x: X) = throw Ball(x)
    private[value] def catches[A](a: => A): Either[X, A] =
      try { Right(a) } catch { case Ball(x) => Left(x) }
  }

  type RelCidResolver[-A, +B] = CidResolver[A, B, Value.RelativeContractId => Ref.ContractIdString]
  type NoCidResolver[-A, +B] = CidResolver[A, B, Ball[Value.ContractId]]
  type NoRelCidResolver[-A, +B] = CidResolver[A, B, Ball[Value.RelativeContractId]]

  private[lf] val AtomicRelCidResolver: RelCidResolver[Value.ContractId, Value.AbsoluteContractId] =
    new RelCidResolver[Value.ContractId, Value.AbsoluteContractId] {
      override private[value] def resolve(
          f: Value.RelativeContractId => ContractIdString
      ): Value.ContractId => AbsoluteContractId = {
        case acoid: Value.AbsoluteContractId => acoid
        case rcoid: Value.RelativeContractId => Value.AbsoluteContractId(f(rcoid))
      }
    }

  private[lf] val AtomicNoCidResolver: NoCidResolver[Value.ContractId, Nothing] =
    new NoCidResolver[Value.ContractId, Nothing] {
      override private[value] def resolve(b: Ball[Value.ContractId]): Value.ContractId => Nothing =
        b.throws
    }

  private[lf] val AtomicNoRelCidResolver: NoRelCidResolver[Value.ContractId, AbsoluteContractId] =
    new NoRelCidResolver[Value.ContractId, AbsoluteContractId] {
      override private[value] def resolve(
          b: Ball[Value.RelativeContractId],
      ): Value.ContractId => AbsoluteContractId = {
        case acoid: Value.AbsoluteContractId => acoid
        case rcoid: Value.RelativeContractId => b.throws(rcoid)
      }
    }

  private[lf] def identityResolverInstance[A, X]: CidResolver[A, A, X] = new CidResolver[A, A, X] {
    override private[value] def resolve(f: X): A => A = identity
  }

}

trait CidResolver1[F[_]] {
  self =>

  import CidResolver._

  // We use name `map1` to avoid confusion with standard scala `map` method.
  private[lf] def map1[A, B](f: A => B): F[A] => F[B]

  private final def resolve[A, B, X](f: X)(
      implicit resolver: CidResolver[A, B, X],
  ): F[A] => F[B] =
    map1(resolver.resolve(f))

  final implicit def ResolverInstance[A, B, X](
      implicit resolver: CidResolver[A, B, X],
  ): CidResolver[F[A], F[B], X] = new CidResolver[F[A], F[B], X] {
    override private[value] def resolve(f: X): F[A] => F[B] =
      self.resolve(f)(resolver)
  }

  private final def resolveWithBall[A, B, X](x: F[A])(
      implicit resolver: CidResolver[A, B, Ball[X]],
  ): Either[X, F[B]] = {
    val ball = new Ball[X]
    ball.catches(resolve[A, B, Ball[X]](ball)(resolver)(x))
  }

  final def assertNoCid[A, B](x: F[A])(
      implicit resolver: CidResolver.NoCidResolver[A, B],
  ): Either[Value.ContractId, F[B]] =
    resolveWithBall(x)

  final def assertNoRelCid[A, B](x: F[A])(
      implicit resolver: CidResolver.NoRelCidResolver[A, B],
  ): Either[Value.RelativeContractId, F[B]] =
    resolveWithBall(x)

  final def resolveRelCid[A, B](f: Value.RelativeContractId => Ref.ContractIdString, x: F[A])(
      implicit resolver: CidResolver.RelCidResolver[A, B],
  ): F[B] =
    resolve(f)(resolver).apply(x)

}

trait CidResolver3[F[_, _, _]] {
  self =>

  import CidResolver._

  private[lf] def map3[A1, A2, A3, B1, B2, B3](
      f1: A1 => B1,
      f2: A2 => B2,
      f3: A3 => B3,
  ): F[A1, A2, A3] => F[B1, B2, B3]

  private[lf] final def resolve[A1, A2, A3, B1, B2, B3, X](f: X)(
      implicit resolver1: CidResolver[A1, B1, X],
      resolver2: CidResolver[A2, B2, X],
      resolve3: CidResolver[A3, B3, X],
  ): F[A1, A2, A3] => F[B1, B2, B3] =
    map3(resolver1.resolve(f), resolver2.resolve(f), resolve3.resolve(f))

  final implicit def ResolverInstance[A1, A2, A3, B1, B2, B3, X](
      implicit resolver1: CidResolver[A1, B1, X],
      resolver2: CidResolver[A2, B2, X],
      resolve3: CidResolver[A3, B3, X],
  ): CidResolver[F[A1, A2, A3], F[B1, B2, B3], X] =
    new CidResolver[F[A1, A2, A3], F[B1, B2, B3], X] {
      override private[value] def resolve(f: X): F[A1, A2, A3] => F[B1, B2, B3] =
        self.resolve(f)(resolver1, resolver2, resolve3)
    }

  private[lf] final def resolveWithBall[A1, A2, A3, B1, B2, B3, X](x: F[A1, A2, A3])(
      implicit resolver1: CidResolver[A1, B1, Ball[X]],
      resolver2: CidResolver[A2, B2, Ball[X]],
      resolve3: CidResolver[A3, B3, Ball[X]],
  ): Either[X, F[B1, B2, B3]] = {
    val ball = new Ball[X]
    ball.catches(resolve(ball)(resolver1, resolver2, resolve3)(x))
  }

  final def assertNoCid[A1, A2, A3, B1, B2, B3](x: F[A1, A2, A3])(
      implicit resolver1: NoCidResolver[A1, B1],
      resolver2: NoCidResolver[A2, B2],
      resolve3: NoCidResolver[A3, B3],
  ): Either[Value.ContractId, F[B1, B2, B3]] =
    resolveWithBall(x)

  final def assertNoRelCid[A1, A2, A3, B1, B2, B3](x: F[A1, A2, A3])(
      implicit resolver1: NoRelCidResolver[A1, B1],
      resolver2: NoRelCidResolver[A2, B2],
      resolve3: NoRelCidResolver[A3, B3],
  ): Either[Value.RelativeContractId, F[B1, B2, B3]] =
    resolveWithBall(x)

  final def resolveRelCid[A1, A2, A3, B1, B2, B3](
      f: Value.RelativeContractId => Ref.ContractIdString,
      x: F[A1, A2, A3],
  )(
      implicit resolver1: RelCidResolver[A1, B1],
      resolver2: RelCidResolver[A2, B2],
      resolve3: RelCidResolver[A3, B3],
  ): F[B1, B2, B3] =
    resolve(f)(resolver1, resolver2, resolve3).apply(x)

}
