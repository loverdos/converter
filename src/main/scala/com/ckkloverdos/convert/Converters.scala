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
import Converter.AnyConverter

/**
 * An immutable registry for converters.
 *
 * @author Christos KK Loverdos <loverdos@gmail.com>.
 */
trait Converters extends CanConvert {
  def converters: Vector[AnyConverter]

  def converterFor[S: Manifest, T: Manifest](sm: Manifest[S], tm: Manifest[T], cacheResult: Boolean = true): Maybe[Converter[S, T]]

  def convertValue[S: Manifest, T: Manifest](sourceValue: S, tm: Manifest[T]): Maybe[T] = {
    (for(converter <- converterFor(manifest[S], tm))
       yield converter.convert(sourceValue)).flatten1
  }
}