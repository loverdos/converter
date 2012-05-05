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

import com.ckkloverdos.maybe.Maybe
import org.slf4j.LoggerFactory

/**
 * 
 * @author Christos KK Loverdos <loverdos@gmail.com>
 */

trait ConverterBase {
  protected val logger = LoggerFactory.getLogger(getClass)
  
  def canConvertValueToType[S: Type, T: Type](sourceValue: S): Boolean =
    canConvertType[S, T]

  def canConvertValueToValue[S: Type, T: Type](sourceValue: S, targetValue: T): Boolean =
    canConvertType[S, T]

  def canConvertType[S: Type, T: Type]: Boolean

  /**
   * Convert or throw an exception.
   *
   * This is a low-level function.
   */
  @throws(classOf[ConverterException])
  def convertEx[T: Type](sourceValue: Any): T

  def convert[T: Type](sourceValue: Any): Maybe[T] = Maybe {
//    logger.debug("ConverterBase::convert(%s: %s): %s".format(sourceValue, sourceValue.getClass, manifest[T]))
    convertEx[T](sourceValue)
  }

  def convertOpt[T: Type](sourceValue: Any): Option[T] = {
    try Some(convertEx[T](sourceValue))
    catch {
      case e: Error ⇒
        throw e

      case e: Throwable ⇒
        None
    }
  }

  def convertToByte[S: Type](sourceValue: S): Maybe[Byte] = convert[Byte](sourceValue)

  def convertToByteOpt[S: Type](sourceValue: S): Option[Byte] = convertOpt[Byte](sourceValue)

  def convertToByteEx[S: Type](sourceValue: S): Byte = convertEx[Byte](sourceValue)

  def convertToBoolean[S: Type](sourceValue: S): Maybe[Boolean] = convert[Boolean](sourceValue)

  def convertToBooleanOpt[S: Type](sourceValue: S): Option[Boolean] = convertOpt[Boolean](sourceValue)

  def convertToBooleanEx[S: Type](sourceValue: S): Boolean = convertEx[Boolean](sourceValue)

  def convertToShort[S: Type](sourceValue: S): Maybe[Short] = convert[Short](sourceValue)

  def convertToShortOpt[S: Type](sourceValue: S): Option[Short] = convertOpt[Short](sourceValue)

  def convertToShortEx[S: Type](sourceValue: S): Short = convertEx[Short](sourceValue)

  def convertToChar[S: Type](sourceValue: S): Maybe[Char] = convert[Char](sourceValue)

  def convertToCharOpt[S: Type](sourceValue: S): Option[Char] = convertOpt[Char](sourceValue)

  def convertToCharEx[S: Type](sourceValue: S): Char = convertEx[Char](sourceValue)

  def convertToInt[S: Type](sourceValue: S): Maybe[Int] = convert[Int](sourceValue)

  def convertToIntOpt[S: Type](sourceValue: S): Option[Int] = convertOpt[Int](sourceValue)

  def convertToIntEx[S: Type](sourceValue: S): Int = convertEx[Int](sourceValue)

  def convertToLong[S: Type](sourceValue: S): Maybe[Long] = convert[Long](sourceValue)

  def convertToLongOpt[S: Type](sourceValue: S): Option[Long] = convertOpt[Long](sourceValue)

  def convertToLongEx[S: Type](sourceValue: S): Long = convertEx[Long](sourceValue)

  def convertToFloat[S: Type](sourceValue: S): Maybe[Float] = convert[Float](sourceValue)

  def convertToFloatOpt[S: Type](sourceValue: S): Option[Float] = convertOpt[Float](sourceValue)

  def convertToFloatEx[S: Type](sourceValue: S): Float = convertEx[Float](sourceValue)

  def convertToDouble[S: Type](sourceValue: S): Maybe[Double] = convert[Double](sourceValue)

  def convertToDoubleOpt[S: Type](sourceValue: S): Option[Double] = convertOpt[Double](sourceValue)

  def convertToDoubleEx[S: Type](sourceValue: S): Double = convertEx[Double](sourceValue)
}
