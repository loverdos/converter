/*
 * Copyright 2011 Christos KK Loverdos
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.ckkloverdos.convert

import java.util.concurrent.locks.ReentrantLock

/**
 * 
 * @author Christos KK Loverdos <loverdos@gmail.com>.
 */
object ConverterHelpers {
  /**
   * TODO: This must go somewhere else...
   */
  def lock[T](lock: ReentrantLock)(f: => T): Unit = {
    lock.lock()
    try f
    finally lock.unlock()
  }

}