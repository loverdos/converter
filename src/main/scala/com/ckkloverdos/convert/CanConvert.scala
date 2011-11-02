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

import Converter.{AnyManifest}

/**
 * @author Christos KK Loverdos <loverdos@gmail.com>.
 */
trait CanConvert {
  def canConvertValueToType[S: Manifest](sourceValue: S, tm: AnyManifest): Boolean =
    canConvertType(manifest[S], tm)

  def canConvertValue[S: Manifest, T: Manifest](sourceValue: S, targetValue: T): Boolean =
    canConvertType(manifest[S], manifest[T])

  def canConvertType(sm: AnyManifest, tm: AnyManifest): Boolean
}