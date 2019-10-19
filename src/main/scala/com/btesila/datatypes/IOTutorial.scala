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
package com.btesila.datatypes

import cats.effect.{ExitCode, IO, IOApp}

object IOTutorial extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = IO.pure(ExitCode.Success)

  /**
   * IO can describe asynchronous processes via the IO.async and IO.cancelable builders.
   *
   * {{{
   *    def async[A](k: (Either[Throwable, A] => Unit) => Unit): IO[A] = ???
   * }}}
   *
   * The provided registration function injects a callback that you can use to signal either
   * successful results (with Right(a)), or failures (with Left(error)).
   *
   * These async ops are not cancelable.
   */

  import scala.concurrent.{Future, ExecutionContext}
  import scala.util.{Success, Failure}

  // There is IO.fromFuture - but for the sake of the exercise
  def convert[A](fa: => Future[A])(implicit ec: ExecutionContext): IO[A] =
    IO.async { cb =>
      // This triggers evaluation of the by-name param and of onComplete,
      // so it's OK to have side effects in this callback
      fa.onComplete {
        case Success(a) => cb(Right(a))
        case Failure(e) => cb(Left(e))
      }
    }

  /**
   * For building cancelable IO tasks you need to use the IO.cancelable builder,
   * this being compliant with Concurrent#cancelable (see Concurrent) and has this signature:
   *
   * {{{
   *   def cancelable[A](k: (Either[Throwable, A] => Unit) => IO[Unit]): IO[A] = ???
   * }}}
   *
   * So it is similar with IO.async, but in that registration function the user is expected to
   * provide an IO[Unit] that captures the required cancellation logic.
   *
   * Important: cancellation is the ability to interrupt an IO task before completion,
   * possibly releasing any acquired resources, useful in race conditions to prevent leaks.
   *
   * As example suppose we want to describe a sleep operation that depends on
   * Java’s ScheduledExecutorService, delaying a tick for a certain time duration:
   */

  import java.util.concurrent.ScheduledExecutorService
  import scala.concurrent.duration._

  def delayedTick(d: FiniteDuration)
    (implicit sc: ScheduledExecutorService): IO[Unit] = {

    IO.cancelable { cb =>
      val r = new Runnable { def run(): Unit = cb(Right(())) }
      val f = sc.schedule(r, d.length, d.unit)

      // Returning the cancellation token needed to cancel
      // the scheduling and release resources early
      IO(f.cancel(false))
    }
  }

}
