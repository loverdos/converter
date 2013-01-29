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
package select

import java.util.concurrent.locks.ReentrantLock
import com.ckkloverdos.convert.ConverterHelpers._
import com.ckkloverdos.maybe.{NoVal, Maybe, Just}

/**
 *
 * @author Christos KK Loverdos <loverdos@gmail.com>.
 */
final class CachedMostSpecificTypeFirstSelection(converters: Traversable[Converter]) extends ConverterSelectionStrategy {
  private[this] val _lock = new ReentrantLock()
  private[this] val (_strictSourceConverters, _nonStrictSourceConverters) = converters.map(Just(_)).partition(_.get.isStrictSource)
  private[this] var _cache: Map[(Manifest[_], Manifest[_], AnyRef), Just[Converter]] = Map()

  def isCaching = true

  def addToCache(sm: Type[_], tm: Type[_], hint: AnyRef, cv: Converter) = {
    lock(_lock) {
//      logger.debug("addToCache(%s, %s, %s, %s)".format(sm, tm, hint, cv))
      _cache += ((sm, tm, hint) -> Just(cv))
    }
  }

  override def findCached[S, T](sm: Type[S], tm: Type[T], hint: AnyRef) = {
    _cache.get((sm, tm, hint)) match {
      case Some(jcv) =>
//        logger.debug("findCached(%s, %s, %s) => %s".format(sm, tm, hint, jcv))
        jcv.asInstanceOf[Maybe[Converter]]
      case None =>
//        logger.debug("findCached(%s, %s, %s) => %s".format(sm, tm, hint, None))
        NoVal
    }
  }

  def findNonCached[S, T](sm: Type[S], tm: Type[T], hint: AnyRef): Maybe[Converter] = {
    _strictSourceConverters.find(_.get.canConvertType(hint)(sm, tm)) match {
      case Some(jcv) =>
//        logger.debug("findNonCached(%s, %s, %s) => STRICT: %s".format(sm, tm, hint, jcv))
        jcv.asInstanceOf[Maybe[Converter]]
      case None =>
        _nonStrictSourceConverters.find { converterJust ⇒
          val converter = converterJust.get
          val canConvert = converter.canConvertType(hint)(sm, tm)
//          logger.debug("%s: canConvertType(%s, %s, %s) by %s".format(canConvert, sm, tm, hint, converter))
          canConvert
        } match {
          case Some(jcv) =>
//            logger.debug("findNonCached(%s, %s, %s) => NON-STRICT: %s".format(sm, tm, hint, jcv))
            jcv.asInstanceOf[Maybe[Converter]]
          case None =>
//            logger.debug("findNonCached(%s, %s, %s) => %s".format(sm, tm, hint, None))
            NoVal
        }
    }
  }
}