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


import org.junit.Assert
import org.junit.Test

/**
 *
 * @author Christos KK Loverdos <loverdos@gmail.com>.
 */
class TypeTest {

  val infoList = List(
    (1: Byte, Manifest.Byte, Array[Byte](), typeOf[Array[Byte]]),
    (true, Manifest.Boolean, Array[Boolean](), typeOf[Array[Boolean]]),
    (1: Short, Manifest.Short, Array[Short](), typeOf[Array[Short]]),
    (' ', Manifest.Char, Array[Char](), typeOf[Array[Char]]),
    (1: Int, Manifest.Int, Array[Int](), typeOf[Array[Int]]),
    (1: Long, Manifest.Long, Array[Long](), typeOf[Array[Long]]),
    (1: Float, Manifest.Float, Array[Float](), typeOf[Array[Float]]),
    (1: Double, Manifest.Double, Array[Double](), typeOf[Array[Double]]) //,
    // ((),        Manifest.Unit,    Array[Unit](),    manifest[Array[Unit]]) // we have a bug in scalac here...
  )

  val valueList = infoList map {
    case (v, m, _, _) ⇒ (v, m)
  }
  val arrayList = infoList map {
    case (_, _, av, am) ⇒ (av, am)
  }

  def id[A](a: A) = a

  def _checkValue[T](value: T, expectedType: Type[_], info: String = ""): Unit = {
    val computedType = typeOfAny(value)
    Assert.assertEquals("%sType for value %s".format(info, value), expectedType, computedType)
  }

  @Test
  def testValues: Unit = {
    for((value, tpe) <- valueList) {
      //      println("Testing value %s of manifest %s".format(value, manifest))
      _checkValue(value, tpe)
    }
  }

  @Test
  def testValues2: Unit = {
    val v2 = valueList map {
      case (v, m) => (id(v), m)
    }
    for((value, tpe) <- v2) {
      //      println("Testing(2) value %s of manifest %s".format(value, manifest))
      _checkValue(value, tpe)
    }
  }

  @Test
  def testNull: Unit = {
    _checkValue(null, typeOf[Null])
  }

  @Test
  def testArray: Unit = {
    for((av, am) <- arrayList) {
      _checkValue(av, am)
    }
  }
}