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
 *
 * @author Christos KK Loverdos <loverdos@gmail.com>.
 */
class SourceTargetConverter[SS, TT](
    val sourceType: Type[SS],
    val targetType: Type[TT],
    val isStrictSource: Boolean,
    val function: SS => TT,
    val theHint: AnyRef = EmptyHint
) extends Converter {

  def canConvertType[S: Type, T: Type](hint: AnyRef = EmptyHint): Boolean = {
//    logger.debug("canConvertType(): theHint = %s, hint = %s".format(theHint, hint))
    (theHint == hint) && {
      val sm = typeOf[S]
      val tm = typeOf[T]

//      logger.debug("canConvertType(%s, %s), sourceType=%s, targetType=%s".format(sm, tm, sourceType, targetType))

      if(isStrictSource)
        canConvertStrictSource(sm, tm)
      else
        canConvertNonStrictSource(sm, tm)
    }
  }

  private[this] def canConvertStrictSource(sm: Type[_], tm: Type[_]) = {
    SourceTargetConverter.canConvertWithStrictSource(
      sourceType,
      targetType,
      sm,
      tm
    )
  }

  private[this] def canConvertNonStrictSource(sm: Type[_], tm: Type[_]) = {
    SourceTargetConverter.canConvertWithNonStrictSource(
      sourceType,
      targetType,
      sm,
      tm
    )
  }


  @throws(classOf[ConverterException])
  def convertEx[T: Type](sourceValue: Any, hint: AnyRef = EmptyHint): T = {
    val tm = typeOf[T]
    if(theHint == hint) {
      if(targetType != tm) {
        ConverterException("Unexpeced target type %s. It should have been %s".format(tm, targetType))
      }
      try function(sourceType.erasure.cast(sourceValue).asInstanceOf[SS]).asInstanceOf[T]
      catch {
        case e: Error ⇒
          throw e

        case e: ClassCastException ⇒
          val msg = "tm=%s, targetType=%s, sourceType=%s".format(tm, targetType, sourceType)
          ConverterException(e, "[%s] Unexpected failure converting %s -> %s for value %s".format(msg, sourceValue.getClass, tm.erasure, sourceValue))

        case e: Exception ⇒
          ConverterException(e, "Error converting %s -> %s for value %s".format(sourceValue.getClass, tm, sourceValue))
      }
    } else {
      ConverterException("Error converting %s -> %s for value %s. Uknown hint %s".format(sourceValue.getClass, tm, sourceValue, hint))
    }
  }

  override def toString() =
    "STConverter(" + List(sourceType, targetType, isStrictSource, function, theHint).mkString(",") + ")"
}

object SourceTargetConverter {
  def canConvertWithStrictSource[SS, TT, S, T](sourceType: Type[SS],
                                               targetType: Type[TT],
                                               givenSourceType: Type[S],
                                               givenTargetType: Type[T]): Boolean = {

    canConvertWithStrictSource(sourceType.erasure, targetType.erasure, givenSourceType, givenTargetType)
  }

  def canConvertWithStrictSource[SS, TT, S, T](sourceType: Class[SS],
                                               targetType: Class[TT],
                                               givenSourceType: Type[S],
                                               givenTargetType: Type[T]): Boolean = {

    sourceType.equals(givenSourceType.erasure) && targetType.equals(givenTargetType.erasure)
  }

  def canConvertWithNonStrictSource[SS, TT, S, T](sourceType: Type[SS],
                                                  targetType: Type[TT],
                                                  givenSourceType: Type[S],
                                                  givenTargetType: Type[T]): Boolean = {

    canConvertWithNonStrictSource(sourceType.erasure, targetType.erasure, givenSourceType, givenTargetType)
  }

  def canConvertWithNonStrictSource[SS, TT, S, T](sourceType: Class[SS],
                                                  targetType: Class[TT],
                                                  givenSourceType: Type[S],
                                                  givenTargetType: Type[T]): Boolean = {

    sourceType.isAssignableFrom(givenSourceType.erasure) && targetType.equals(givenTargetType.erasure)
  }
}

abstract class StrictSourceConverterSkeleton[SS: Type, TT: Type](theHint: AnyRef = EmptyHint) extends Converter {
  final def canConvertType[S: Type, T: Type](hint: AnyRef = EmptyHint): Boolean = {
    (theHint == hint) &&
    SourceTargetConverter.canConvertWithStrictSource(typeOf[SS], typeOf[TT], typeOf[S], typeOf[T])
  }

  final def isStrictSource = true

  /**
   * Convert or throw an exception.
   *
   * This is a low-level function.
   */
  @throws(classOf[ConverterException])
  final def convertEx[T: Type](sourceValue: Any, hint: AnyRef = EmptyHint) = {
    convertEx_(sourceValue.asInstanceOf[SS], hint).asInstanceOf[T]
  }

  @throws(classOf[ConverterException])
  protected def convertEx_(sourceValue: SS, hint: AnyRef = EmptyHint): TT
}

abstract class NonStrictSourceConverterSkeleton[SS: Type, TT: Type](theHint: AnyRef = EmptyHint) extends Converter {
  final def canConvertType[S: Type, T: Type](hint: AnyRef = EmptyHint): Boolean = {
    (theHint == hint) &&
    SourceTargetConverter.canConvertWithNonStrictSource(typeOf[SS], typeOf[TT], typeOf[S], typeOf[T])
  }

  final def isStrictSource = false

  /**
   * Convert or throw an exception.
   *
   * This is a low-level function.
   */
  final def convertEx[T: Type](sourceValue: Any, hint: AnyRef = EmptyHint) = {
    convertEx_(sourceValue.asInstanceOf[SS], hint).asInstanceOf[T]
  }

  protected def convertEx_(sourceValue: SS, hint: AnyRef = EmptyHint): TT

}