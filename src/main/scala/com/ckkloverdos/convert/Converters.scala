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

import select.ConverterSelectionStrategy
import com.ckkloverdos.maybe._

/**
 * An immutable registry for converters.
 *
 * @author Christos KK Loverdos <loverdos@gmail.com>.
 */
class Converters(selector: ConverterSelectionStrategy) extends CanConvert {
  def canConvertType(sm: Converter.AnyManifest, tm: Converter.AnyManifest) = {
    selector.find(sm, tm).isJust
  }

  def findConverter[S: Manifest, T: Manifest](sm: Manifest[S], tm: Manifest[T]): Maybe[Converter[S, T]] = {
    selector.find(sm, tm).asInstanceOf[Maybe[Converter[S, T]]]
  }

  /**
   * Converts a value or throws an exception if the value cannot be converted.
   */
  @throws(classOf[ConverterException])
  def convertValueEx[S: Manifest, T: Manifest](sourceValue: S, tm: Manifest[T]): T = {
    val sm = manifest[S]
    findConverter(sm, tm) match {
      case Just(cv) =>
        cv.convertEx(sourceValue)
      case NoVal =>
        ConverterException("Could not find converter from %s -> %s for value %s", sm, tm, sourceValue)
      case Failed(exception, explanation) =>
        ConverterException(exception, "Error [%s] trying to find converter from %s -> %s for value %s", explanation, sm, tm, sourceValue)
    }
  }

  def convertValue[S: Manifest, T: Manifest](sourceValue: S, tm: Manifest[T]): Maybe[T] = {
    (for(converter <- findConverter(manifest[S], tm))
       yield converter.convert(sourceValue)).flatten1
  }

  def convertValueToInt[S: Manifest](sourceValue: S): Maybe[Int] = convertValue(sourceValue, Manifest.Int)

  def convertValueToLong[S: Manifest](sourceValue: S): Maybe[Long] = convertValue(sourceValue, Manifest.Long)

  def convertValueToBoolean[S: Manifest](sourceValue: S): Maybe[Boolean] = convertValue(sourceValue, Manifest.Boolean)
}