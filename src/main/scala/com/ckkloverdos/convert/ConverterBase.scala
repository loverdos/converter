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
}
