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
import Assert.{assertEquals, assertTrue}
import com.ckkloverdos.maybe.{Maybe, Failed, Just, NoVal}
import java.nio.CharBuffer

/**
 * 
 * @author Christos KK Loverdos <loverdos@gmail.com>.
 */
class ConvertersTest {
  val converters = new StdConvertersBuilder().registerDefaultConversions().build

  @Test
  def testNoDoubleToInt: Unit = {
    val value = converters.convertValue(1.0, Manifest.Int)
    assertEquals(NoVal, value)
  }

  @Test
  def testStringToDouble: Unit = {
    val from = "0.1"
    val value = converters.convertValue(from, Manifest.Double)
    assertEquals(Just(0.1), value)
  }

  @Test
  def testNoStringToInt: Unit = {
    val from = "0.1"
    val value = converters.convertValue(from, Manifest.Int)
    assertTrue(value.isFailed)
  }
  
  @Test
  def testNullToNoVal: Unit = {
    val registry = new StdConvertersBuilder().register[AnyRef, Maybe[AnyRef]](true)((x) => Maybe(x)).build
    val value = registry.convertValue(null, manifest[Maybe[AnyRef]])
    assertEquals(Just(NoVal), value)
  }

  @Test
  def testMostSpecificConverter: Unit = {
    val builder = new ConvertersBuilder
    // All CharSequences go to Int via this one
    builder.register[CharSequence, Int](false) { _.toString.toInt }
    // But CharBuffers (though they are CharSequences) are handled differently
    builder.register[CharBuffer, Int](true) { cb => 555 }

    val converters = builder.build
    val _value1   = converters.convertValueToInt("1")
    val _value555 = converters.convertValueToInt(CharBuffer.wrap("1"))

    assertEquals(Just(1), _value1)
    assertEquals(Just(555), _value555)
  }
}