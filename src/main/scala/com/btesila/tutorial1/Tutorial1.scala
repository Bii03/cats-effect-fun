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


/**
 * Our goal is to create a program that copies files. First we will work on a function that carries
 * such task, and then we will create a program that can be invoked from the shell and uses that function.
 *
 * First of all we must code the function that copies the content from a file to another file.
 * The function takes the source and destination files as parameters.
 *
 * But this is functional programming! So invoking the function shall not copy anything, instead it
 * will return an IO instance that encapsulates all the side effects involved (opening/closing files,
 * reading/writing content), that way purity is kept.
 *
 * Only when that IO instance is evaluated all those side-effectful actions will be run.
 *
 * Of course errors can occur, but when working with any IO those should be embedded in the IO
 * instance. That is, no exception is raised outside the IO and so no try (or the like) needs to be
 * used when using the function, instead the IO evaluation will fail and the IO instance will carry
 * the error raised.
 */

object Tutorial1 {

  import cats.effect.{IO, Resource}
  import cats.implicits._
  import java.io._

  /**
   * IO - a pure abstraction representing the intention to perform a
   * side effect, where the result of that side effect may be obtained
   * synchronously (via return) or asynchronously (via callback).
   *
   * The method transfer will perform the actual copying of data, once the resources (the streams)
   * are obtained. When they are not needed anymore, whatever the outcome of transfer (success
   * or failure) both streams will be closed.
   *
   * If any of the streams could not be obtained, then transfer will not be run. Even better, because
   * of Resource semantics, if there is any problem opening the input file then the output file will
   * not be opened. On the other hand, if there is any issue opening the output file, then the input
   * stream will be closed.
   */
  def copy(origin: File, destination: File): IO[Long] = {
    val ioStreams: Resource[IO, (InputStream, OutputStream)] = inputOutputStreams(origin, destination)

    // use - allocates a resource and supplies it to the given function.  The
    // resource is released as soon as the resulting `F[B]` is
    // completed, whether normally or as a raised error.
    ioStreams.use { case (in, out) => transfer(in, out) }
  }

  /**
   * This does the real work.
   *
   * That function will have to define a loop that at each iteration reads data from the input stream
   * into a buffer, and then writes the buffer contents into the output stream.
   *
   * At the same time, the loop will keep a counter of the bytes transferred. To reuse the same
   * buffer we should define it outside the main loop, and leave the actual transmission of data to
   * another function transmit that uses that loop.
   */
  def transfer(origin: InputStream, destination: OutputStream): IO[Long] =
    for {
      buffer <- IO(new Array[Byte](1024 * 10)) // Allocated only when the IO is evaluated
      total <- transmit(origin, destination, buffer, 0L)
    } yield total

  def transmit(origin: InputStream, destination: OutputStream, buffer: Array[Byte], acc: Long): IO[Long] =
    for {
      amount <- IO(origin.read(buffer, 0, buffer.size))
      // >> : defined with a by-name parameter, allowing this method to be used in cases where computing it is not stack safe
      count <- if (amount > -1) IO(destination.write(buffer, 0, amount)) >> transmit(origin, destination, buffer, acc + amount)
      else IO.pure(acc) // End of read stream reached (by java.io.InputStream contract), nothing to write
    } yield count // Returns the actual amount of bytes transmitted

  /**
   * Resource instances can be combined in for-comprehensions as they implement flatMap.
   */
  def inputOutputStreams(in: File, out: File): Resource[IO, (InputStream, OutputStream)] =
    for {
      inStream <- inputStream(in)
      outStream <- outputStream(out)
    } yield (inStream, outStream)


  /**
   * We consider opening an stream to be a side-effect action, so we have to encapsulate those
   * actions in their own IO instances.
   *
   * Resource -  a data structure that captures the effectful allocation of a resource, along with
   * its finalizer; it receives two functions:
   * - acquire: [F[A]]
   * - release: A => F[Unit]
   *
   * F is IO, in our case
   *
   * Resource[F[_], A], where:
   * - F the effect type in which the resource is allocated and released
   * - A the type of resource
   */
  def inputStream(f: File): Resource[IO, FileInputStream] =
    Resource.make {
      IO(new FileInputStream(f)) // build
    } { inStream =>
      IO(inStream.close()).handleErrorWith(_ => IO.unit) // release
    }

  def outputStream(f: File): Resource[IO, FileOutputStream] =
    Resource.make {
      IO(new FileOutputStream(f)) // build
    } { outStream =>
      IO(outStream.close()).handleErrorWith(_ => IO.unit) // release
    }
}
