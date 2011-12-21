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
package select

import com.ckkloverdos.maybe.{NoVal, Just, Maybe}
import org.slf4j.LoggerFactory

/**
 * An algorithm that selects the appropriate converter.
 *
 * The implementations are supposed to store some related state and probably cache results.
 *
 * @author Christos KK Loverdos <loverdos@gmail.com>.
 */
trait ConverterSelectionStrategy {
  protected val logger = LoggerFactory.getLogger(getClass)

  def isCaching: Boolean

  def canConvertType[S: Manifest, T: Manifest]: Boolean = {
    this.find(manifest[S], manifest[T]).isJust
  }

  def shouldCache(sm: Manifest[_], tm: Manifest[_], cv: Converter): Boolean = isCaching

  def findCached[S, T](sm: Manifest[S], tm: Manifest[T]): Maybe[Converter]

  def findNonCached[S, T](sm: Manifest[S], tm: Manifest[T]): Maybe[Converter]

  def find[S, T](sm: Manifest[S], tm: Manifest[T]): Maybe[Converter] = {
//    logger.debug("find(%s, %s)".format(sm, tm))
    if(sm == tm) {
      val justIdentityConverter = Converters.justIdentityConverter
//      logger.debug("Found %s".format(justIdentityConverter))
      justIdentityConverter
    } else if(isCaching) {
      findCached[S, T](sm, tm) match {
        case j@Just(cv) =>
          j
        case NoVal =>
          findNonCached[S, T](sm, tm) match {
            case j@Just(cv) =>
              if(shouldCache(sm, tm, cv)) addToCache(sm, tm, cv)
              j
            case other =>
              other
          }
        case failed =>
          failed
      }
    } else {
      findNonCached[S, T](sm, tm)
    }
  }

  def addToCache(sm: Manifest[_], tm: Manifest[_], cv: Converter): Unit
}
