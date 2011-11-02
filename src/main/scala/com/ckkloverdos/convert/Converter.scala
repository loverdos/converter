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

import com.ckkloverdos.maybe.Maybe

/**
 *
 * @author Christos KK Loverdos <loverdos@gmail.com>.
 */
trait Converter[S, T] extends CanConvert {
  def sourceType: Manifest[S]
  def targetType: Manifest[T]

  def convert(sourceValue: S): Maybe[T]
  def apply(sourceValue: S): Maybe[T] = convert(sourceValue)

  def toFunction: Function1[S,  Maybe[T]] = (s) => convert(s)
}

/**
 * @author Christos KK Loverdos <loverdos@gmail.com>.
 */
object Converter {
  type AnyConverter = Converter[_, _]
  type AnyManifest  = Manifest[_]

  def newConverter[S: Manifest, T: Manifest](strictSource: Boolean)(f: S => T): Converter[S, T] = {
    new ConverterImpl(manifest[S], manifest[T], strictSource, f)
  }
}

