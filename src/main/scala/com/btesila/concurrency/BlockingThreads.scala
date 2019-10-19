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
package com.btesila.concurrency

import java.io.File

import cats.effect.{ExitCode, IOApp}
import com.btesila.tutorial1.PolymorphicCode.copy

/**
 * As a rule we should never block threads, but sometimes we have to work with interface that does it.
 * Blocking a thread means that it is being wasted and nothing else can be scheduled to run on it.
 * As mentioned, this can be very dangerous and it’s best to use dedicated thread pool for blocking operations.
 * This way they won’t interfere with CPU-bound part of application.
 *
 * cats.effect.IO and monix.eval.Task provide shift operator which can switch computation to different
 * thread pool.
 *
 * If you need to execute blocking operation and come back consider using ContextShift.evalOn
 * which is meant for this use case.
 *
 * See more on https://github.com/ChristopherDavenport/linebacker - library that provides context
 * shifting pattern
 *
 * https://monix.io/docs/3x/best-practices/blocking.html
 */
object BlockingThreads extends IOApp {

  import java.util.concurrent.Executors
  import cats.effect.{ContextShift, IO}
  import scala.concurrent.ExecutionContext

  override implicit val contextShift: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  val blockingEC = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

  /**
   * {{{
   *    contextShift.evalOn(blockingEC)(blockingOp)
   * }}}
   * Evaluates blockingOp on the supplied execution context, blockingEC,  and shifts evaluation
   * back to the default execution environment of `IO` at the completion of `blockingOp`,
   * regardless of success or failure.
   */
  val prog =
    for {
      _ <- contextShift.evalOn(blockingEC)(blockingOp) // executes on blockingEC
      _ <- doSth() // executes on contextShift
    } yield ()

  def blockingOp: IO[Unit] = IO(println(s"doing something blocking on ${ Thread.currentThread().getName}"))

  def doSth(): IO[Unit] = IO(println(s"doing something on ${ Thread.currentThread().getName}"))


  override def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- prog
    } yield ExitCode.Success

}
