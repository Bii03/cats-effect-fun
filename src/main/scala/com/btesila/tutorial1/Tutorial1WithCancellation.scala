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

import cats.effect.{Concurrent, IO, Resource}
import cats.effect.concurrent.Semaphore
import com.btesila.tutorial1.Tutorial1.transmit

/**
 * Cancellation is a powerful but non-trivial cats-effect feature. In cats-effect, some IO instances
 * can be cancelable, meaning that their evaluation will be aborted. If the programmer is careful,
 * an alternative IO task will be run under cancellation, for example to deal with potential cleaning
 * up activities. We will see how an IO can be actually canceled at the end of the Fibers are not
 * threads! section later on, but for now we will just keep in mind that during the execution of the
 * IO returned by the copy method a cancellation could be requested at any time.
 *
 * Now, IOs created with Resource.use can be canceled. The cancellation will trigger the execution of
 * the code that handles the closing of the resource. In our case, that would close both streams.
 * So far so good! But what happens if cancellation happens while the streams are being used? This
 * could lead to data corruption as a stream where some thread is writing to is at the same time
 * being closed by another thread.
 *
 * To prevent such data corruption we must use some concurrency control mechanism that ensures that
 * no stream will be closed while the IO returned by transfer is being evaluated. Cats-effect provides
 * several constructs for controlling concurrency, for this case we will use a semaphore. A semaphore
 * has a number of permits, its method .acquire ‘blocks’ if no permit is available until release is
 * called on the same semaphore. It is important to remark that there is no actual thread being really
 * blocked, the thread that finds the .acquire call will be immediately recycled by cats-effect.
 * When the release method is invoked then cats-effect will look for some available thread to resume
 * the execution of the code after .acquire.
 *
 * We will use a semaphore with a single permit. The .withPermit method acquires one permit,
 * runs the IO given and then releases the permit.
 *
 * We could also use .acquire and then .release on the semaphore explicitly, but .withPermit is more
 * idiomatic and ensures that the permit is released even if the effect run fails.
 */
object Tutorial1WithCancellation {

  /**
   * It can be argued that using IO{java.nio.file.Files.copy(...)} would get an IO with the same
   * characteristics of purity as our function. But there is a difference: our IO is safely cancelable!
   * So the user can stop the running code at any time for example by pressing Ctrl-c, our code will
   * deal with safe resource release (streams closing) even under such circumstances. The same will
   * apply if the copy function is run from other modules that require its functionality. If the IO
   * returned by this function is canceled while being run, still resources will be properly released.
   * But recall what we commented before: this is because use returns IO instances that are cancelable,
   * in contrast our transfer function is not cancelable.
   */
  def copy(origin: File, destination: File)(implicit concurrent: Concurrent[IO]): IO[Long] = {
    for {
      guard <- Semaphore[IO](1)
      count <- inputOutputStreams(origin, destination, guard).use { case (in, out) =>
        guard.withPermit(transfer(in, out))
      }
    } yield count
  }

  // transfer and transmit methods as defined before
  def transfer(origin: InputStream, destination: OutputStream): IO[Long] =
    for {
      buffer <- IO(new Array[Byte](1024 * 10)) // Allocated only when the IO is evaluated
      total <- transmit(origin, destination, buffer, 0L)
    } yield total

  def inputOutputStreams(in: File, out: File, guard: Semaphore[IO]): Resource[IO, (InputStream, OutputStream)] =
    for {
      inStream <- inputStream(in, guard)
      outStream <- outputStream(out, guard)
    } yield (inStream, outStream)

  def inputStream(f: File, guard: Semaphore[IO]): Resource[IO, FileInputStream] =
    Resource.make {
      IO(new FileInputStream(f))
    } { inStream =>
      guard.withPermit {
        IO(inStream.close()).handleErrorWith(_ => IO.unit)
      }
    }

  def outputStream(f: File, guard: Semaphore[IO]): Resource[IO, FileOutputStream] =
    Resource.make {
      IO(new FileOutputStream(f))
    } { outStream =>
      guard.withPermit {
        IO(outStream.close()).handleErrorWith(_ => IO.unit)
      }
    }
}
