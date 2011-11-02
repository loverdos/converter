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

import com.ckkloverdos.maybe.{NoVal, Just, Maybe}
import java.lang.String
import reflect.Manifest
import Converter.{AnyConverter, AnyManifest}
import ConverterHelpers.{lock}
import java.util.concurrent.locks.ReentrantLock
import org.slf4j.LoggerFactory


/**
 * An registry for converters implementation.
 *
 * This is thread-safe.
 * 
 * @author Christos KK Loverdos <loverdos@gmail.com>.
 */

class ConvertersImpl private[convert](val converters: Vector[AnyConverter]) extends Converters {
  protected val logger = LoggerFactory.getLogger(getClass)
  private[this] val _lock = new ReentrantLock()
  private[this] var _cache: Map[(AnyManifest, AnyManifest), AnyConverter] = Map()

  private[this] def _addToCache[S, T](converter: AnyConverter): Unit = {
    val sm = converter.sourceType
    val tm = converter.targetType
    lock(_lock) {
      _cache += ((sm, tm) -> converter)
    }
  }

  def converterFor[S: Manifest, T: Manifest](sm: Manifest[S], tm: Manifest[T], cacheResult: Boolean = true): Maybe[Converter[S, T]] = {
    val pair = (sm, tm)
    _cache get pair match {
      case Some(converter) =>
        logger.debug("Found cached converter for %s -> %s".format(sm, tm))
        Just(converter.asInstanceOf[Converter[S, T]])
      case None =>
        converters.find(_.canConvertType(sm, tm)) match {
          case Some(converter) =>
            if(cacheResult) {
              logger.debug("Caching converter for %s -> %s".format(sm, tm))
              _addToCache(converter)
            }
            Just(converter.asInstanceOf[Converter[S, T]])
          case None =>
            logger.warn("No converter found for %s -> %s".format(sm, tm))
            NoVal
        }
    }
  }

  def clearCache: Unit = {
    lock(_lock) { _cache = Map() }
  }

  //+ CanConvert
  def canConvertType(sm: Manifest[_], tm: Manifest[_]) =
    converterFor(sm, tm).isJust
  //- CanConvert

  override def toString: String =
    "Converters(" + this.converters.mkString("\n") + ")"


}
