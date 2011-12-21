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

package com.ckkloverdos.manifest

/**
 * Work in progress for obtaining a manifest dynamically.
 *
 * @author Christos KK Loverdos <loverdos@gmail.com>.
 */
object ManifestHelpers {
  def manifestOfClass[T](clazz: Class[T]): Manifest[_ <: AnyRef] = {
    if(clazz.isArray) {
      Manifest.arrayType(manifestOfClass(clazz.getComponentType))
    } else {
      Manifest.classType(clazz)
    }
  }

  def manifestOfAny[A](any: A): Manifest[_ <: Any] = {
    any match {
      case byte: Byte =>
        Manifest.Byte
      case byte: Boolean =>
        Manifest.Boolean
      case short: Short =>
        Manifest.Short
      case byte: Char =>
        Manifest.Char
      case any: Int =>
        Manifest.Int
      case long: Long =>
        Manifest.Long
      case float: Float =>
        Manifest.Float
      case double: Double =>
        Manifest.Double
      case unit: Unit =>
        Manifest.Unit
      
//      case byte: java.lang.Byte =>
//        Manifest.Byte
//      case byte: java.lang.Boolean =>
//        Manifest.Boolean
//      case short: java.lang.Short =>
//        Manifest.Short
//      case byte: java.lang.Character =>
//        Manifest.Char
//      case any: java.lang.Integer =>
//        Manifest.Int
//      case long: java.lang.Long =>
//        Manifest.Long
//      case float: java.lang.Float =>
//        Manifest.Float
//      case double: java.lang.Double =>
//        Manifest.Double

      case null =>
        Manifest.Null

      case anyRef: AnyRef =>
        manifestOfClass(anyRef.getClass)
    }
  }
}