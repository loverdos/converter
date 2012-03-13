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
import com.ckkloverdos.manifest.ManifestHelpers
import org.slf4j.LoggerFactory

/**
 * An immutable registry for converters.
 *
 * @author Christos KK Loverdos <loverdos@gmail.com>.
 */
class Converters(selector: ConverterSelectionStrategy) extends ConverterBase {
  def canConvertType[S: Manifest, T: Manifest]: Boolean = selector.canConvertType[S, T]

  def findConverter[S : Manifest, T : Manifest]: Maybe[Converter] = {
    val sm = manifest[S]
    val tm = manifest[T]
//    logger.debug("findConverter(%s, %s)".format(sm, tm))
    selector.find(sm, tm)
  }

  /**
   * Converts a value or throws an exception if the value cannot be converted.
   */
  @throws(classOf[ConverterException])
  def convertEx[T: Manifest](sourceValue: Any): T = {
    val sm = ManifestHelpers.manifestOfAny(sourceValue)
    val tm = manifest[T]
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

  def convertToByte[S: Manifest](sourceValue: S): Maybe[Byte] = convert[Byte](sourceValue)
  def convertToBoolean[S: Manifest](sourceValue: S): Maybe[Boolean] = convert[Boolean](sourceValue)
  def convertToShort[S: Manifest](sourceValue: S): Maybe[Short] = convert[Short](sourceValue)
  def convertToChar[S: Manifest](sourceValue: S): Maybe[Char] = convert[Char](sourceValue)
  def convertToInt[S: Manifest](sourceValue: S): Maybe[Int] = convert[Int](sourceValue)
  def convertToLong[S: Manifest](sourceValue: S): Maybe[Long] = convert[Long](sourceValue)
  def convertToFloat[S: Manifest](sourceValue: S): Maybe[Float] = convert[Float](sourceValue)
  def convertToDouble[S: Manifest](sourceValue: S): Maybe[Double] = convert[Double](sourceValue)
}

object Converters {
  final val DefaultConverters = new StdConvertersBuilder().registerDefaultConversions().build
  
  final def identityConverter: IdentityConverter.type =
    IdentityConverter
  
  final def justIdentityConverter: Just[IdentityConverter.type] =
    Just(IdentityConverter)

  final def newSourceTargetConverter[S, T](sm: Manifest[S], tm: Manifest[T], strictSource: Boolean)(f: S => T): SourceTargetConverter[S, T] =
    new SourceTargetConverter[S, T](sm, tm, strictSource, f)
}