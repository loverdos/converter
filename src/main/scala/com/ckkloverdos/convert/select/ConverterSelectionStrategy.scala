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

import Converter.{AnyManifest, AnyConverter}
import com.ckkloverdos.maybe.{NoVal, Just, Maybe}

/**
 * An algorithm that selects the appropriate converter.
 *
 * The implementations are supposed to store some related state and probably cache results.
 *
 * @author Christos KK Loverdos <loverdos@gmail.com>.
 */
trait ConverterSelectionStrategy {
  def isCaching: Boolean

  def shouldCache(sm: AnyManifest, tm: AnyManifest, cv: AnyConverter): Boolean = isCaching

  def findCached(sm: AnyManifest, tm: AnyManifest): Maybe[AnyConverter]

  def findNonCached(sm: AnyManifest, tm: AnyManifest): Maybe[AnyConverter]

  def find(sm: AnyManifest, tm: AnyManifest): Maybe[AnyConverter] = {
    if(isCaching) {
      findCached(sm, tm) match {
        case j@Just(cv) =>
          j
        case NoVal =>
          findNonCached(sm, tm) match {
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
      findNonCached(sm, tm)
    }
  }

  def addToCache(sm: AnyManifest, tm: AnyManifest, cv: AnyConverter): Unit
}
