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

/**
 *
 * @author Christos KK Loverdos <loverdos@gmail.com>.
 */
class SourceTargetConverter[SS, TT](
    val sourceType: Manifest[SS],
    val targetType: Manifest[TT],
    val isStrictSource: Boolean,
    val function: SS => TT)
  extends Converter {

  def canConvertType[S: Manifest, T: Manifest]: Boolean = {
    val sm = manifest[S]
    val tm = manifest[T]

//    logger.debug("canConvertType(%s, %s), sourceType=%s, targetType=%s".format(sm, tm, sourceType, targetType))

    if(isStrictSource)
      canConvertStrictSource(sm, tm)
    else
      canConvertNonStrictSource(sm, tm)
  }

  private[this] def canConvertStrictSource(sm: Manifest[_], tm: Manifest[_]) = {
    SourceTargetConverter.canConvertWithStrictSource(
      sourceType,
      targetType,
      sm,
      tm
    )
  }

  private[this] def canConvertNonStrictSource(sm: Manifest[_], tm: Manifest[_]) = {
    SourceTargetConverter.canConvertWithNonStrictSource(
      sourceType,
      targetType,
      sm,
      tm
    )
  }


  @throws(classOf[ConverterException])
  def convertEx[T: Manifest](sourceValue: Any): T = {
    val tm = manifest[T]
    if(targetType != tm) {
      ConverterException("Unexpeced target type %s. It should have been %s".format(tm, targetType))
    }
    try function(sourceType.erasure.cast(sourceValue).asInstanceOf[SS]).asInstanceOf[T]
    catch {
      case e: ClassCastException ⇒
        val msg = "tm=%s, targetType=%s, sourceType=%s".format(tm, targetType, sourceType)
        ConverterException(e, "[%s] Unexpected failure converting %s -> %s for value %s".format(msg, sourceValue.getClass, tm.erasure, sourceValue))
      case e: Exception ⇒
        ConverterException(e, "Error converting %s -> %s for value %s".format(sourceValue.getClass, tm, sourceValue))
    }
  }

  override def toString() =
    "STConverter(" + List(sourceType, targetType, isStrictSource, function).mkString(",") + ")"
}

object SourceTargetConverter {
  def canConvertWithStrictSource(sourceType: Manifest[_],
                                 targetType: Manifest[_],
                                 givenSourceType: Manifest[_],
                                 givenTargetType: Manifest[_]): Boolean = {

    canConvertWithStrictSource(sourceType.erasure, targetType.erasure, givenSourceType, givenTargetType)
  }

  def canConvertWithStrictSource(sourceType: Class[_],
                                 targetType: Class[_],
                                 givenSourceType: Manifest[_],
                                 givenTargetType: Manifest[_]): Boolean = {

    sourceType.equals(givenSourceType.erasure) && targetType.equals(givenTargetType.erasure)
  }

  def canConvertWithNonStrictSource(sourceType: Manifest[_],
                                    targetType: Manifest[_],
                                    givenSourceType: Manifest[_],
                                    givenTargetType: Manifest[_]): Boolean = {

    canConvertWithNonStrictSource(sourceType.erasure, targetType.erasure, givenSourceType, givenTargetType)
  }

  def canConvertWithNonStrictSource(sourceType: Class[_],
                                    targetType: Class[_],
                                    givenSourceType: Manifest[_],
                                    givenTargetType: Manifest[_]): Boolean = {

    sourceType.isAssignableFrom(givenSourceType.erasure) && targetType.equals(givenTargetType.erasure)
  }
}