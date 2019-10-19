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

import java.util.concurrent.ExecutorService

import cats.effect.{ExitCode, IO, IOApp}
import com.btesila.concurrency.BlockingThreads.prog

import scala.concurrent.ExecutionContextExecutor

object ThreadShiftingWithIO extends IOApp {

  import java.util.concurrent.Executors

  import scala.concurrent.ExecutionContext

  val cachedThreadPool: ExecutorService = Executors.newCachedThreadPool()
  val BlockingFileIO: ExecutionContextExecutor = ExecutionContext.fromExecutor(cachedThreadPool)

  implicit val Main: ExecutionContextExecutor = ExecutionContext.global

  /**
   * We start by asking the user to enter its name and next we thread-shift to the BlockingFileIO
   * execution context because we expect the following action to block on the thread for a long time
   * and we don’t want that to happen in the main thread of execution.
   */
  override def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- IO(println(s"${ Thread.currentThread().getName} - Enter your name: "))
      _ <- IO.shift(BlockingFileIO)
      _ <- IO(println(s"${ Thread.currentThread().getName} - Still waiting for your name: "))
      name <- IO(scala.io.StdIn.readLine())
      _ <- IO.shift
      _ <- IO(println(s" ${ Thread.currentThread().getName} Welcome $name!"))
      _ <- IO(cachedThreadPool.shutdown())
    } yield (ExitCode.Success)

}
