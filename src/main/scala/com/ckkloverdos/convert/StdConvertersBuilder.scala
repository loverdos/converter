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
 * @author Christos KK Loverdos <loverdos@gmail.com>.
 */

class StdConvertersBuilder extends ConvertersBuilder {
  def registerDefaultConversions(): this.type = {
    registerNumberConversions()
    registerStringNumberConversions()

    this
  }

  def registerNumberConversions(): this.type = {
    register[Number, Byte]   (false) (_.byteValue)
    register[Number, Short]  (false) (_.shortValue)
    register[Number, Char]   (false) (_.intValue.toChar)
    register[Number, Int]    (false) (_.intValue)
    register[Number, Long]   (false) (_.longValue)
    register[Number, Float]  (false) (_.floatValue)
    register[Number, Double] (false) (_.doubleValue)
    register[Number, Boolean](false) (0 != _.intValue)
    
    this
  }
  
  def registerStringNumberConversions(): this.type = {
    register[CharSequence, Byte]   (false) (_.toString.toByte)
    register[CharSequence, Short]  (false) (_.toString.toShort)
    register[CharSequence, Int]    (false) (_.toString.toInt)
    register[CharSequence, Long]   (false) (_.toString.toLong)
    register[CharSequence, Float]  (false) (_.toString.toFloat)
    register[CharSequence, Double] (false) (_.toString.toDouble)

    this
  }

  def registerStringBooleanConversion(trueValuesLowercase: List[String]): this.type = {
    register[CharSequence, Boolean](false)(_.toString.toLowerCase match {
      case x if(trueValuesLowercase.contains(x)) => true
      case _ => false
    })
    this
  }
}
