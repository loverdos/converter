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

/**
 * Convert a given value to the same type by returning the value itself.
 *
 * @author Christos KK Loverdos <loverdos@gmail.com>
 */

object IdentityConverter extends Converter {
  def isStrictSource = false

  def canConvertType[S: Type, T: Type]: Boolean = {
    typeOf[S] == typeOf[T]
  }

  @throws(classOf[ConverterException])
  def convertEx[T: Type](sourceValue: Any): T = {
    val tm = typeOf[T]
    try sourceValue.asInstanceOf[T]
    catch {
      case e: ClassCastException ⇒
        ConverterException(e, "Unexpected failure for identity conversion %s -> %s for value %s".format(sourceValue.getClass, erasureOf[T], sourceValue))
    }
  }

  override def toString = "IdentityConverter"
}