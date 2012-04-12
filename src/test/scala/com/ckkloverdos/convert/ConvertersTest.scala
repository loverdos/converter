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

import org.junit.Assert
import org.junit.Test
import Assert.{assertEquals, assertTrue, fail}
import com.ckkloverdos.maybe.{Maybe, Just, NoVal}
import java.nio.CharBuffer

/**
 * 
 * @author Christos KK Loverdos <loverdos@gmail.com>.
 */
class ConvertersTest {
  val converters = new StdConvertersBuilder().registerDefaultConversions().build

  @Test
  def testNoDoubleToInt: Unit = {
    val value = converters.convert[Int](1.0)
    assertTrue(value.isFailed)
  }

  @Test
  def testIdentityForInt: Unit = {
    val value = 1
    assertEquals(Just(value), converters.convertToInt(value))
  }

  @Test
  def testStringToDouble: Unit = {
    val from = "0.1"
    val to   = 0.1
    assertEquals(Just(to), converters.convert[Double](from))
    assertEquals(Just(to), converters.convertToDouble(from))
  }

  @Test
  def testNoStringToInt: Unit = {
    val from = "0.1"
    val value = converters.convert[Int](from)
    assertTrue(value.isFailed)
  }

  @Test
  def testStringToInt: Unit = {
    val from: CharSequence = "2011"
    val value = converters.convertToInt(from)
    assertEquals(Just(from.toString.toInt), value)
  }

  @Test
  def testNoStringToInt2: Unit = {
    val from: CharSequence = "2011 "
    val value = converters.convertToInt(from)
    assertTrue(value.isFailed)
  }

  @Test
  def testNullToNoVal: Unit = {
    val registry = new StdConvertersBuilder().register[AnyRef, Maybe[AnyRef]](true)((x) => Maybe(x)).build
    val value = registry.convert[Maybe[AnyRef]](null)
    assertEquals(Just(NoVal), value)
  }

  @Test
  def testMostSpecificConverter: Unit = {
    val builder = new ConvertersBuilder
    // All CharSequences go to Int via this one
    builder.register[CharSequence, Int](false) { _.toString.toInt }
    // But Strings (though they are CharSequences) are handled differently
    builder.register[String, Int](true) { cb => 555 /* always return 555 for strings */}

    val converters = builder.build
    val _value1   = converters.convertToInt(CharBuffer.wrap("1"))
    val _value555 = converters.convertToInt("1")

    assertEquals(Just(1), _value1)
    assertEquals(Just(555), _value555)
  }

  @Test
  def testConversionException: Unit = {
    try {
      converters.convertEx[ConvertersTest](1)
      fail("Should have failed for conversion of Int -> %s".format(typeOf[ConvertersTest].erasure.getName))
    } catch {
      case _: Exception =>
    }
  }
  
  @Test
  def testPrimitiveArray: Unit = {
    val convertersB = new StdConvertersBuilder()
    convertersB.register[Array[Int], Int](true) { array ⇒ (0 /: array)(_+_)}
    val converters = convertersB.build
    val theArray: Array[Int] = Array(1, 2, 3)
    val theArraySum: Int = theArray sum
    val convertedSum = converters.convertEx[Int](theArray)
    // by the way, also test the sum, though we shouldn't for several reasons
    assertEquals(theArraySum, convertedSum)
  }
}