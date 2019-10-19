/** ***********************************************************************
 * ADOBE CONFIDENTIAL
 * ___________________
 *
 * Copyright 2018 Adobe Systems Incorporated
 * All Rights Reserved.
 *
 * NOTICE:  All information contained herein is, and remains
 * the property of Adobe Systems Incorporated and its suppliers,
 * if any.  The intellectual and technical concepts contained
 * herein are proprietary to Adobe Systems Incorporated and its
 * suppliers and are protected by all applicable intellectual property
 * laws, including trade secret and copyright laws.
 * Dissemination of this information or reproduction of this material
 * is strictly forbidden unless prior written permission is obtained
 * from Adobe Systems Incorporated.
 * *************************************************************************/
package com.btesila.tutorial1

import java.io.{File, FileInputStream, FileOutputStream, InputStream, OutputStream}

import cats.effect.concurrent.Semaphore
import cats.effect.{Concurrent, IO, Resource, Sync}
import com.btesila.tutorial1.Tutorial1.transmit
import com.btesila.tutorial1.Tutorial1WithCancellation.{inputOutputStreams, inputStream, outputStream, transfer}

/**
 * IO is able to encapsulate side-effects, but the ability to perform the effects sync or async, or
 * cancelable belong to [[cats.effect.Concurrent[IO] instance.
 *
 * Concurrent[F[_]] is a type class that, for an F carrying a side-effect, brings the ability to
 * cancel or start concurrently the side-effect in F.
 *
 * Concurrent also extends type class Async[F[_]] - allows to define synchronous/asynchronous
 * computations. Async[F[_]] extends type class Sync[F[_]], which can suspend the execution of
 * side effects in F.
 *
 * So well, Sync can suspend side effects (and so can Async and Concurrent as they extend Sync).
 * We have used IO so far mostly for that purpose. Now, going back to the code we created to copy
 * files, could have we coded its functions in terms of some F[_]: Sync instead of IO?
 *
 * Ref: type class implicits ->
 * http://www.lihaoyi.com/post/ImplicitDesignPatternsinScala.html#type-class-implicits
 *
 * Truth is we could and in fact it is recommendable in real world programs.
 */
object PolymorphicCode {

  import cats.implicits._

  /**
   * F is a sync-able effect.
   *
   * Sync[F].delay(a: A): F[A] - lifts any by-name parameter into the `F` context.
   */
  def transmit[F[_]: Sync](origin: InputStream, destination: OutputStream, buffer: Array[Byte], acc: Long): F[Long] =
    for {
      amount <- Sync[F].delay(origin.read(buffer, 0, buffer.length))
      // >> : defined with a by-name parameter, allowing this method to be used in cases where computing it is not stack safe
      count <- if (amount > -1) Sync[F].delay(destination.write(buffer, 0, amount)) >> transmit(origin, destination, buffer, acc + amount)
      else Sync[F].pure(acc) // End of read stream reached (by java.io.InputStream contract), nothing to write
    } yield count // Returns the actual amount of bytes transmitted


  def transfer[F[_]: Sync](origin: InputStream, destination: OutputStream): F[Long] =
    for {
      buffer <- Sync[F].delay(new Array[Byte](1024 * 10)) // Allocated only when the IO is evaluated
      total <- transmit(origin, destination, buffer, 0L)
    } yield total

  def inputOutputStreams[F[_]: Sync](in: File, out: File, guard: Semaphore[F]): Resource[F, (InputStream, OutputStream)] =
    for {
      inStream <- inputStream(in, guard)
      outStream <- outputStream(out, guard)
    } yield (inStream, outStream)

  def inputStream[F[_]: Sync](f: File, guard: Semaphore[F]): Resource[F, FileInputStream] =
    Resource.make {
      Sync[F].delay(new FileInputStream(f))
    } { inStream =>
      guard.withPermit {
        Sync[F].delay(inStream.close()).handleErrorWith(_ => Sync[F].unit)
      }
    }

  def outputStream[F[_]: Sync](f: File, guard: Semaphore[F]): Resource[F, FileOutputStream] =
    Resource.make {
      Sync[F].delay(new FileOutputStream(f))
    } { outStream =>
      guard.withPermit {
        Sync[F].delay(outStream.close()).handleErrorWith(_ => Sync[F].unit)
      }
    }

  /**
   * Concurrent is needed for the instantation of a Semaphore.
   */
  def copy[F[_]: Concurrent](origin: File, destination: File): F[Long] = {
    for {
      guard <- Semaphore[F](1)
      count <- inputOutputStreams(origin, destination, guard).use { case (in, out) =>
        guard.withPermit(transfer(in, out))
      }
    } yield count
  }
}
