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

/**
 * 
 * @author Christos KK Loverdos <loverdos@gmail.com>.
 */
class ConvertersTest {
  val registry = new StdConvertersBuilder().registerDefaultConversions().build

  @Test
  def testNoDoubleToInt: Unit = {
    val value = registry.convertValue(1.0, Manifest.Int)
    assertEquals(NoVal, value)
  }

  @Test
  def testStringToDouble: Unit = {
    val from = "0.1"
    val value = registry.convertValue(from, Manifest.Double)
    assertEquals(Just(0.1), value)
  }

  @Test
  def testNoStringToInt: Unit = {
    val from = "0.1"
    val value = registry.convertValue(from, Manifest.Int)
    assertTrue(value.isFailed)
  }
  
  @Test
  def testNullToNoVal: Unit = {
    val registry = new StdConvertersBuilder().register[AnyRef, Maybe[AnyRef]](true)((x) => Maybe(x)).build
    val value = registry.convertValue(null, manifest[Maybe[AnyRef]])
    assertEquals(Just(NoVal), value)
  }
}