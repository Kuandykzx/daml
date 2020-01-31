package com.digitalasset.daml.lf.value

import com.digitalasset.daml.lf.data.Ref
import com.digitalasset.daml.lf.value.Value.AbsoluteContractId

import scala.language.higherKinds
import scala.util.control.NoStackTrace

trait CidResolver[-A, +B, X] {
  private[value] def resolve(f: X): A => B
}

object CidResolver {

  sealed class Ball[X] {
    case class Ball(x: X) extends Throwable with NoStackTrace
    def throws(x: X) = throw Ball(x)
    def catches[A](a: => A, recover: X => A): A =
      try {
        a
      } catch {
        case Ball(x) => recover(x)
      }
  }

  type RelCidResolver[-A, +B] = CidResolver[A, B, Value.RelativeContractId => Ref.ContractIdString]
  type NoCidResolver[-A, +B] = CidResolver[A, B, Ball[Value.ContractId]]
  type NoRelCidResolver[-A, +B] = CidResolver[A, B, Ball[Value.RelativeContractId]]

  val AtomicRelCidResolver: RelCidResolver[Value.ContractId, Value.AbsoluteContractId] =
    f => {
      case acoid: Value.AbsoluteContractId =>
        acoid
      case rcoid: Value.RelativeContractId =>
        Value.AbsoluteContractId(f(rcoid))
    }

  val AtomicNoCidResolver: NoCidResolver[Value.ContractId, Nothing] =
    b => b.throws

  val AtomicNoRelCidResolver: NoRelCidResolver[Value.ContractId, AbsoluteContractId] =
    b => {
      case acoid: Value.AbsoluteContractId =>
        acoid
      case rcoid: Value.RelativeContractId =>
        b.throws(rcoid)
    }

  implicit def TrivialRelCidResolverInstance[A, X]: CidResolver[A, A, X] =
    _ => identity

}

trait CidResolver1[F[_]] {

  import CidResolver._

  // We use name `map1` to avoid confusion with standard scala `map` method.
  private[lf] def map1[A, B](f: A => B): F[A] => F[B]

  private[lf] final def resolve[A, B, X](f: X)(
      implicit resolver: CidResolver[A, B, X]
  ): F[A] => F[B] =
    map1(resolver.resolve(f))

  final implicit def ResolverInstance[A, B, X](
      implicit resolver: CidResolver[A, B, X]
  ): CidResolver[F[A], F[B], X] = resolve(_)(resolver)

  private[lf] final def resolveWithBall[A, B, X](x: F[A])(
      implicit resolver: CidResolver[A, B, Ball[X]]): Either[X, F[B]] = {
    val ball = new Ball[X]
    ball.catches(Right(resolve[A, B, Ball[X]](ball)(resolver)(x)), Left(_))
  }

  final def assertNoCid[A, B](x: F[A])(
      implicit resolver: CidResolver.NoCidResolver[A, B]
  ): Either[Value.ContractId, F[B]] =
    resolveWithBall(x)

  final def assertNoRelCid[A, B](x: F[A])(
      implicit resolver: CidResolver.NoRelCidResolver[A, B]
  ): Either[Value.RelativeContractId, F[B]] =
    resolveWithBall(x)

  final def resolveRelCid[A, B](f: Value.RelativeContractId => Ref.ContractIdString, x: F[A])(
      implicit resolver: CidResolver.RelCidResolver[A, B]
  ): F[B] =
    resolve(f)(resolver).apply(x)

}

trait CidResolver3[F[_, _, _]] {

  import CidResolver._

  // We use name `map1` to avoid confusion with standard scala `map` method.
  private[lf] def map3[A1, A2, A3, B1, B2, B3](
      f1: A1 => B1,
      f2: A2 => B2,
      f3: A3 => B3): F[A1, A2, A3] => F[B1, B2, B3]

  private[lf] final def resolve[A1, A2, A3, B1, B2, B3, X](f: X)(
      implicit resolver1: CidResolver[A1, B1, X],
      resolver2: CidResolver[A2, B2, X],
      resolve3: CidResolver[A3, B3, X]
  ): F[A1, A2, A3] => F[B1, B2, B3] =
    map3(resolver1.resolve(f), resolver2.resolve(f), resolve3.resolve(f))

  final implicit def ResolverInstance[A1, A2, A3, B1, B2, B3, X](
      implicit resolver1: CidResolver[A1, B1, X],
      resolver2: CidResolver[A2, B2, X],
      resolve3: CidResolver[A3, B3, X]
  ): CidResolver[F[A1, A2, A3], F[B1, B2, B3], X] = resolve(_)(resolver1, resolver2, resolve3)

  private[lf] final def resolveWithBall[A1, A2, A3, B1, B2, B3, X](x: F[A1, A2, A3])(
      implicit resolver1: CidResolver[A1, B1, Ball[X]],
      resolver2: CidResolver[A2, B2, Ball[X]],
      resolve3: CidResolver[A3, B3, Ball[X]]
  ): Either[X, F[B1, B2, B3]] = {
    val ball = new Ball[X]
    ball.catches(Right(resolve(ball)(resolver1, resolver2, resolve3)(x)), Left(_))
  }

  final def assertNoCid[A1, A2, A3, B1, B2, B3](x: F[A1, A2, A3])(
      implicit resolver1: NoCidResolver[A1, B1],
      resolver2: NoCidResolver[A2, B2],
      resolve3: NoCidResolver[A3, B3]
  ): Either[Value.ContractId, F[B1, B2, B3]] =
    resolveWithBall(x)

  final def assertNoRelCid[A1, A2, A3, B1, B2, B3](x: F[A1, A2, A3])(
      implicit resolver1: NoRelCidResolver[A1, B1],
      resolver2: NoRelCidResolver[A2, B2],
      resolve3: NoRelCidResolver[A3, B3]
  ): Either[Value.RelativeContractId, F[B1, B2, B3]] =
    resolveWithBall(x)

  final def resolveRelCid[A1, A2, A3, B1, B2, B3](
      f: Value.RelativeContractId => Ref.ContractIdString,
      x: F[A1, A2, A3])(
      implicit resolver1: RelCidResolver[A1, B1],
      resolver2: RelCidResolver[A2, B2],
      resolve3: RelCidResolver[A3, B3]
  ): F[B1, B2, B3] =
    resolve(f)(resolver1, resolver2, resolve3).apply(x)

}
//trait CidResolver[-A, +B] {
//  def resolveRelCid(f: Value.RelativeContractId => Ref.ContractIdString): A => B
//}

//trait NoCidChecker[-A, +B] extends {
//  def assert(a: A): B
//}
//trait CidResolver1[F[_]] extends Container1[F] {
//  final def resolveRelCid[A, B](f: Value.RelativeContractId => Ref.ContractIdString)(
//      implicit instance: CidResolver[A, B]
//  ): F[A] => F[B] =
//    map1(instance.resolveRelCid(f))
//
//  implicit final def resolverInstance[A, B](
//      implicit instance: CidResolver[A, B],
//  ): CidResolver[F[A], F[B]] =
//    resolveRelCid
//}

//trait NoCidChecker1[F[_]] extends Container1[F] {
//
//  final def assertNoCid[A, B](implicit assertNoCid: NoCidChecker[A, B]): F[A] => F[B] =
//    map1(assertNoCid.assert)
//
//  implicit def checkerInstance[A, B](
//      implicit checker: NoCidChecker[A, B]
//  ): NoCidChecker[F[A], F[B]] = assertNoCid.apply
//
//}

/*
trait CidContainer2[F[_, _]] {
  private[lf] def map[A0, A1, B0, B1](f0: A0 => B0, f1: A1 => B1): F[A0, A1] => F[B0, B1]

  final def resolveRelCid[A0, A1, B0, B1](f: Value.RelativeContractId => Ref.ContractIdString)(
      implicit instance0: RelCidResolver[A0, B0],
      instance1: RelCidResolver[A1, B1],
  ): F[A0, A1] => F[B0, B1] =
    map(instance0.resolve(f), instance1.resolve(f))

  final def assertNoCid[A0, A1, B0, B1](
      implicit instance0: NoCidChecker[A0, B0],
      instance1: NoCidChecker[A1, B1]
  ): F[A0, A1] => F[B0, B1] =
    map(instance0.assert, instance1.assert)

  implicit final def resolverInstance[A0, A1, B0, B1](
      implicit
      instance1: RelCidResolver[A0, B0],
      instance2: RelCidResolver[A1, B1],
  ): RelCidResolver[F[A0, A1], F[B0, B1]] =
    resolveRelCid

  def checkerInstance[A0, B0, A1, B1](
      implicit instance0: NoCidChecker[A0, B0],
      instance1: NoCidChecker[A1, B1]
  ): NoCidChecker[F[A0, A1], F[B0, B1]] = assertNoCid(instance0, instance1).apply

}

trait CidContainer3[F[_, _, _]] {
  private[lf] def map[A0, A1, A2, B0, B1, B2](
      f0: A0 => B0,
      f1: A1 => B1,
      f2: A2 => B2,
  ): F[A0, A1, A2] => F[B0, B1, B2]

  final def resolveRelCid[A0, A1, A2, B0, B1, B2](
      f: Value.RelativeContractId => Ref.ContractIdString,
  )(
      implicit instance0: RelCidResolver[A0, B0],
      instance1: RelCidResolver[A1, B1],
      instance2: RelCidResolver[A2, B2],
  ): F[A0, A1, A2] => F[B0, B1, B2] =
    map(
      instance0.resolve(f),
      instance1.resolve(f),
      instance2.resolve(f),
    )
  final def assertNoCid[A0, A1, A2, B0, B1, B2](
      implicit instance0: NoCidChecker[A0, B0],
      checker1: NoCidChecker[A1, B1],
      checker2: NoCidChecker[A2, B2]
  ): F[A0, A1, A2] => F[B0, B1, B2] =
    map(instance0.assert, checker1.assert, checker2.assert)

  implicit final def cidContainerInstance[A0, A1, A2, B0, B1, B2](
      implicit
      instance1: RelCidResolver[A0, B0],
      instance2: RelCidResolver[A1, B1],
      instance3: RelCidResolver[A2, B2],
  ): RelCidResolver[F[A0, A1, A2], F[B0, B1, B2]] =
    resolveRelCid

  def checkerInstance[A0, A1, A2, B0, B1, B2](
      implicit checker0: NoCidChecker[A0, B0],
      checker1: NoCidChecker[A1, B1],
      checker2: NoCidChecker[A2, B2],
  ): NoCidChecker[F[A0, A1, A2], F[B0, B1, B2]] =
    assertNoCid(checker0, checker1, checker2).apply

}
 */
