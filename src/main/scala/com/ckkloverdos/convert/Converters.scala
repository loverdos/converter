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

import select.ConverterSelectionStrategy
import com.ckkloverdos.maybe._

/**
 * An immutable registry for converters.
 *
 * @author Christos KK Loverdos <loverdos@gmail.com>.
 */
class Converters(selector: ConverterSelectionStrategy) extends ConverterBase {
  def canConvertType[S: Type, T: Type]: Boolean = selector.canConvertType[S, T]

  def findConverter[S : Type, T : Type]: Maybe[Converter] = {
    val sm = typeOf[S]
    val tm = typeOf[T]
//    logger.debug("findConverter(%s, %s)".format(sm, tm))
    selector.find(sm, tm)
  }

  /**
   * Converts a value or throws an exception if the value cannot be converted.
   */
  @throws(classOf[ConverterException])
  def convertEx[T: Type](sourceValue: Any): T = {
    val sm = typeOfAny(sourceValue)
    val tm = typeOf[T]
//    logger.debug("[1] Converters::convertEx(%s: %s)(tm=%s)".format(sourceValue, if(null eq sourceValue.asInstanceOf[AnyRef]) "Null" else sourceValue.getClass, tm))
//    logger.debug("[2] Converters::convertEx(%s: %s): %s".format(sourceValue, sm, tm))
    findConverter(sm, tm) match {
      case Just(cv) ⇒
        try {
//          logger.debug("Converters::convertEx:: found %s".format(cv))
          cv.convertEx[T](sourceValue)
        } catch {
          case e: Exception =>
            val errMsg = "Error converting %s -> %s for value %s".format(sm, tm, sourceValue)
//            logger.error("Converters::convertEx:: " + errMsg)
            ConverterException(e, errMsg)
        }
      case NoVal ⇒
        val errMsg = "Could not find converter %s -> %s for value %s".format(sm, tm, sourceValue)
//        logger.error("Converters::convertEx:: " + errMsg)
        throw new ConverterException(errMsg)
      case Failed(exception) =>
        val errMsg = "Could not find converter %s -> %s for value %s".format(sm, tm, sourceValue)
//        logger.error("Converters::convertEx:: " + errMsg)
        throw new ConverterException(errMsg, exception)
    }
  }

  def convertToByte[S: Type](sourceValue: S): Maybe[Byte] = convert[Byte](sourceValue)

  def convertToByteEx[S: Type](sourceValue: S): Byte = convertEx[Byte](sourceValue)

  def convertToBoolean[S: Type](sourceValue: S): Maybe[Boolean] = convert[Boolean](sourceValue)

  def convertToBooleanEx[S: Type](sourceValue: S): Boolean = convertEx[Boolean](sourceValue)

  def convertToShort[S: Type](sourceValue: S): Maybe[Short] = convert[Short](sourceValue)

  def convertToShortEx[S: Type](sourceValue: S): Short = convertEx[Short](sourceValue)

  def convertToChar[S: Type](sourceValue: S): Maybe[Char] = convert[Char](sourceValue)

  def convertToCharEx[S: Type](sourceValue: S): Char = convertEx[Char](sourceValue)

  def convertToInt[S: Type](sourceValue: S): Maybe[Int] = convert[Int](sourceValue)

  def convertToIntEx[S: Type](sourceValue: S): Int = convertEx[Int](sourceValue)

  def convertToLong[S: Type](sourceValue: S): Maybe[Long] = convert[Long](sourceValue)

  def convertToLongEx[S: Type](sourceValue: S): Long = convertEx[Long](sourceValue)

  def convertToFloat[S: Type](sourceValue: S): Maybe[Float] = convert[Float](sourceValue)

  def convertToFloatEx[S: Type](sourceValue: S): Float = convertEx[Float](sourceValue)

  def convertToDouble[S: Type](sourceValue: S): Maybe[Double] = convert[Double](sourceValue)

  def convertToDoubleEx[S: Type](sourceValue: S): Double = convertEx[Double](sourceValue)
}

object Converters {
  final val DefaultConverters = new StdConvertersBuilder().registerDefaultConversions().build
  
  final def identityConverter: IdentityConverter.type =
    IdentityConverter
  
  final def justIdentityConverter: Just[IdentityConverter.type] =
    Just(IdentityConverter)

  final def newSourceTargetConverter[S, T](sm: Type[S], tm: Type[T], strictSource: Boolean)(f: S ⇒ T): SourceTargetConverter[S, T] =
    new SourceTargetConverter[S, T](sm, tm, strictSource, f)
}