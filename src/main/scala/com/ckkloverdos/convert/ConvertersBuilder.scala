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

import java.util.concurrent.locks.ReentrantLock
import Converter.{AnyConverter}
import ConverterHelpers.{lock}
import org.slf4j.LoggerFactory
import select.{ConverterSelectionStrategy, CachedMostSpecificTypeFirstSelection}

/**
 * A builder for converter registries.
 *
 * This is thread-safe.
 *
 * @author Christos KK Loverdos <loverdos@gmail.com>.
 */
class ConvertersBuilder {
  protected val logger = LoggerFactory.getLogger(getClass)
  private[this] val _lock = new ReentrantLock()
  private[this] var _converters: Vector[AnyConverter] = Vector()

  def registerConverter(converter: AnyConverter): this.type = {
    lock(_lock) {
      logger.trace("Adding converter %s".format(converter))
      _converters = converter +: _converters
    }
    this
  }

  def +=[S: Manifest, T: Manifest](cw: Converter[S, T]): this.type = {
    this.registerConverter(cw)
  }

  def ++=(cw1: AnyConverter, cw2: AnyConverter, cwSeq: AnyConverter*): this.type = {
    this += cw1 += cw2
    for(cw <- cwSeq) { this += cw }
    this
  }

  def ++=(cwTrav: Traversable[AnyConverter]): this.type = {
    for(cw <- cwTrav) { this += cw }
    this
  }

  def register[S: Manifest, T: Manifest](sm: Manifest[S], tm: Manifest[T], ss: Boolean = true)(f: (S) => T): this.type = {
    this += Converter.newConverter[S, T](ss)(f)
    this
  }

  def register[S: Manifest, T: Manifest](ss: Boolean)(f: (S) => T): this.type = {
    this += Converter.newConverter[S, T](ss)(f)
    this
  }

  def build: Converters =
    buildWithStrategy(new CachedMostSpecificTypeFirstSelection(_))

  def buildWithStrategy(f: (Traversable[AnyConverter]) => ConverterSelectionStrategy): Converters =
    new Converters(f(_converters))
}