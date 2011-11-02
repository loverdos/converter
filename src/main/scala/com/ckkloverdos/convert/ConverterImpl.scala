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

import com.ckkloverdos.maybe.Maybe
import com.ckkloverdos.maybe.Maybe._
import Converter.{AnyManifest}
/**
 * 
 * @author Christos KK Loverdos <loverdos@gmail.com>.
 */
class ConverterImpl[S: Manifest, T: Manifest](
    val sourceType: Manifest[S],
    val targetType: Manifest[T],
    val strictSource: Boolean,
    val function: S => T)
  extends Converter[S, T] {

  def canConvertType(sm: AnyManifest, tm: AnyManifest) = {
    if(strictSource)
      canConvertStrictSource(sm, tm)
    else
      canConvertNonStrictSource(sm, tm)
  }

  private def canConvertStrictSource(sm: AnyManifest, tm: AnyManifest) = {
    val from = sm.erasure
    val to = tm.erasure

    sourceType.erasure.equals(from) && targetType.erasure.equals(to)
  }

  private def canConvertNonStrictSource(sm: AnyManifest, tm: AnyManifest) = {
    val from = sm.erasure
    val to = tm.erasure

    sourceType.erasure.isAssignableFrom(from) && targetType.erasure.equals(to)
  }

  def convert(sourceValue: S): Maybe[T] = Maybe(function(sourceValue))

  override def toString() =
    "Converter(" + List(sourceType, targetType, strictSource, function).mkString(",") + ")"
}
