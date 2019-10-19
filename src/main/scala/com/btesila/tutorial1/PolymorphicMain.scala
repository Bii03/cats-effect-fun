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

import java.io.File

import cats.effect.{ExitCode, IO, IOApp}

import PolymorphicCode._

object PolymorphicMain extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- if (args.length < 2) IO.raiseError(new IllegalArgumentException("Need origin and destination files"))
      else IO.unit
      orig = new File(args(0))
      dest = new File(args(1))
      count <- copy[IO](orig, dest)
      _ <- IO(println(s"$count bytes copied from ${orig.getPath} to ${dest.getPath}"))
    } yield ExitCode.Success
}
