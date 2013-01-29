/*
 * Copyright 2011-2012 Christos KK Loverdos
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

import org.slf4j.LoggerFactory
import com.ckkloverdos.maybe.MaybeEither

/**
 * 
 * @author Christos KK Loverdos <loverdos@gmail.com>
 */

trait ConverterBase {
  protected val logger = LoggerFactory.getLogger(getClass)
  
  def canConvertValueToType[S: Type, T: Type](sourceValue: S, hint: AnyRef = EmptyHint): Boolean =
    canConvertType[S, T](hint)

  def canConvertValueToValue[S: Type, T: Type](sourceValue: S, targetValue: T, hint: AnyRef = EmptyHint): Boolean =
    canConvertType[S, T](hint)

  def canConvertType[S: Type, T: Type](hint: AnyRef = EmptyHint): Boolean

  /**
   * Convert or throw an exception.
   *
   * This is a low-level function.
   */
  @throws(classOf[ConverterException])
  def convertEx[T: Type](sourceValue: Any, hint: AnyRef = EmptyHint): T

  def convert[T: Type](sourceValue: Any, hint: AnyRef = EmptyHint): MaybeEither[T] = MaybeEither {
//    logger.debug("ConverterBase::convert(%s: %s): %s".format(sourceValue, typeOfAny(sourceValue), manifest[T]))
    convertEx[T](sourceValue, hint)
  }

  def convertOpt[T: Type](sourceValue: Any, hint: AnyRef = EmptyHint): Option[T] = {
    try Some(convertEx[T](sourceValue, hint))
    catch {
      case e: Error ⇒
        throw e

      case e: Throwable ⇒
        None
    }
  }

  def convertToByte[S: Type](sourceValue: S, hint: AnyRef = EmptyHint): MaybeEither[Byte] = convert[Byte](sourceValue, hint)

  def convertToByteOpt[S: Type](sourceValue: S, hint: AnyRef = EmptyHint): Option[Byte] = convertOpt[Byte](sourceValue, hint)

  def convertToByteEx[S: Type](sourceValue: S, hint: AnyRef = EmptyHint): Byte = convertEx[Byte](sourceValue, hint)

  def convertToBoolean[S: Type](sourceValue: S, hint: AnyRef = EmptyHint): MaybeEither[Boolean] = convert[Boolean](sourceValue, hint)

  def convertToBooleanOpt[S: Type](sourceValue: S, hint: AnyRef = EmptyHint): Option[Boolean] = convertOpt[Boolean](sourceValue, hint)

  def convertToBooleanEx[S: Type](sourceValue: S, hint: AnyRef = EmptyHint): Boolean = convertEx[Boolean](sourceValue, hint)

  def convertToShort[S: Type](sourceValue: S, hint: AnyRef = EmptyHint): MaybeEither[Short] = convert[Short](sourceValue, hint)

  def convertToShortOpt[S: Type](sourceValue: S, hint: AnyRef = EmptyHint): Option[Short] = convertOpt[Short](sourceValue, hint)

  def convertToShortEx[S: Type](sourceValue: S, hint: AnyRef = EmptyHint): Short = convertEx[Short](sourceValue, hint)

  def convertToChar[S: Type](sourceValue: S, hint: AnyRef = EmptyHint): MaybeEither[Char] = convert[Char](sourceValue, hint)

  def convertToCharOpt[S: Type](sourceValue: S, hint: AnyRef = EmptyHint): Option[Char] = convertOpt[Char](sourceValue, hint)

  def convertToCharEx[S: Type](sourceValue: S, hint: AnyRef = EmptyHint): Char = convertEx[Char](sourceValue, hint)

  def convertToInt[S: Type](sourceValue: S, hint: AnyRef = EmptyHint): MaybeEither[Int] = convert[Int](sourceValue, hint)

  def convertToIntOpt[S: Type](sourceValue: S, hint: AnyRef = EmptyHint): Option[Int] = convertOpt[Int](sourceValue, hint)

  def convertToIntEx[S: Type](sourceValue: S, hint: AnyRef = EmptyHint): Int = convertEx[Int](sourceValue, hint)

  def convertToLong[S: Type](sourceValue: S, hint: AnyRef = EmptyHint): MaybeEither[Long] = convert[Long](sourceValue, hint)

  def convertToLongOpt[S: Type](sourceValue: S, hint: AnyRef = EmptyHint): Option[Long] = convertOpt[Long](sourceValue, hint)

  def convertToLongEx[S: Type](sourceValue: S, hint: AnyRef = EmptyHint): Long = convertEx[Long](sourceValue, hint)

  def convertToFloat[S: Type](sourceValue: S, hint: AnyRef = EmptyHint): MaybeEither[Float] = convert[Float](sourceValue, hint)

  def convertToFloatOpt[S: Type](sourceValue: S, hint: AnyRef = EmptyHint): Option[Float] = convertOpt[Float](sourceValue, hint)

  def convertToFloatEx[S: Type](sourceValue: S, hint: AnyRef = EmptyHint): Float = convertEx[Float](sourceValue, hint)

  def convertToDouble[S: Type](sourceValue: S, hint: AnyRef = EmptyHint): MaybeEither[Double] = convert[Double](sourceValue, hint)

  def convertToDoubleOpt[S: Type](sourceValue: S, hint: AnyRef = EmptyHint): Option[Double] = convertOpt[Double](sourceValue, hint)

  def convertToDoubleEx[S: Type](sourceValue: S, hint: AnyRef = EmptyHint): Double = convertEx[Double](sourceValue, hint)
}
