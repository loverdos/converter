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

/**
 * 
 * @author Christos KK Loverdos <loverdos@gmail.com>.
 */
final class ConverterException(cause: Throwable, msg: String, args: Any*) extends Exception(msg.format(args: _*), cause) {
  def this(msg: String, args: Any*) = this(null: Throwable, msg, args: _*)
}


/**
 *
 * @author Christos KK Loverdos <loverdos@gmail.com>.
 */
object ConverterException {
  def apply(msg: String, args: Any*): Nothing = throw new ConverterException(msg, args: _*)
  def apply(cause: Throwable, msg: String, args: Any*): Nothing = throw new ConverterException(cause, msg, args: _*)
}