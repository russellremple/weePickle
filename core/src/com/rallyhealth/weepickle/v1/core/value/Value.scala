package com.rallyhealth.weepickle.v1.core.value

import java.time.Instant

import com.rallyhealth.weepickle.v1.core.{ArrVisitor, FromInput, ObjVisitor, Visitor}

import scala.collection.compat._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Eschew a JSON-centric view of data. This takes a WeePickle-centric view of things.
  *
  * This Value is (mostly) source-compatible with com.rallyhealth.weejson.v1.Value, but is faithful
  * to all data types that can be represented in WeePickle visit* methods, not just data as represented
  * in JSON, without adding or loosing precision (except perhaps objects with non-string keys).
  */
object Value extends Visitor[Value, Value] {

  sealed trait Selector {
    def apply(x: Value): Value
    def update(x: Value, y: Value): Unit
  }

  object Selector {
    implicit class IntSelector(i: Int) extends Selector {
      def apply(x: Value): Value = x.arr(i)
      def update(x: Value, y: Value) = x.arr(i) = y
    }

    implicit class StringSelector(i: String) extends Selector {
      def apply(x: Value): Value = x.obj(i)
      def update(x: Value, y: Value) = x.obj(i) = y
    }
  }

  def transform[T](i: Value, to: Visitor[_, T]): T = i match {
    case Arr(items) =>
      val ctx: ArrVisitor[Any, T] = to.visitArray(items.size).narrow
      items.foreach { item =>
        ctx.visitValue(transform(item, ctx.subVisitor))
      }
      ctx.visitEnd()

    case Obj(items) =>
      val ctx: ObjVisitor[Any, T] = to.visitObject(items.size).narrow
      items.foreach { case (key, value) =>
        val keyVisitor = ctx.visitKey()
        ctx.visitKeyValue(keyVisitor.visitString(key))
        ctx.visitValue(transform(value, ctx.subVisitor))
      }
      ctx.visitEnd()

    case Null => to.visitNull()
    case True => to.visitTrue()
    case False => to.visitFalse()
    case Str(cs) => to.visitString(cs)
    case Chr(c: Char) => to.visitChar(c)
    case Int32(i: Int) => to.visitInt32(i)
    case Int64(l: Long) => to.visitInt64(l)
    case UInt64(ul: Long) => to.visitUInt64(ul)
    case Float32(f: Float) => to.visitFloat32(f)
    case Float64(d: Double) => to.visitFloat64(d)
    case Float64String(s: String) => to.visitFloat64String(s)
    case Float64StringParts(cs: CharSequence, decIndex: Int, expIndex: Int) => to.visitFloat64StringParts(cs, decIndex, expIndex)
    case Binary(bytes: Array[Byte], offset: Int, len: Int) => to.visitBinary(bytes, offset, len)
    case Ext(tag: Byte, bytes: Array[Byte], offset: Int, len: Int) => to.visitExt(tag, bytes, offset, len)
    case Timestamp(instant: Instant) => to.visitTimestamp(instant)
  }

  override def visitArray(length: Int): ArrVisitor[Value, Value] = new ArrVisitor[Value, Value] {
    private[this] val vs = ArrayBuffer.newBuilder[Value]
    override def subVisitor: Visitor[_, _] = Value.this
    override def visitValue(v: Value): Unit = vs += v
    override def visitEnd(): Value = Arr.from(vs.result())
  }

  override def visitObject(length: Int): ObjVisitor[Value, Value] = new ObjVisitor[Value, Value] {
    private[this] var key: String = null
    private[this] val vs = ArrayBuffer.newBuilder[(String, Value)]
    override def subVisitor: Visitor[_, _] = Value.this
    override def visitKey(): Visitor[_, _] = com.rallyhealth.weepickle.v1.core.StringVisitor
    override def visitKeyValue(s: Any): Unit = key = s.toString
    override def visitValue(v: Value): Unit = vs += (key -> v)
    override def visitEnd(): Value = Obj.from(vs.result())
  }

  override def visitNull(): Value = Null
  override def visitTrue(): Value = True
  override def visitFalse(): Value = False
  override def visitString(cs: CharSequence): Value = Str(cs.toString)
  override def visitChar(c: Char): Value = Chr(c: Char)
  override def visitInt32(i: Int): Value = Int32(i)
  override def visitInt64(l: Long): Value = Int64(l)
  override def visitUInt64(l: Long): Value = UInt64(l)
  override def visitFloat32(f: Float): Value = Float32(f)
  override def visitFloat64(d: Double): Value = Float64(d)
  override def visitFloat64String(s: String): Value = Float64String(s)
  override def visitFloat64StringParts(cs: CharSequence, decIndex: Int, expIndex: Int): Value = Float64StringParts(cs, decIndex, expIndex)
  override def visitBinary(bytes: Array[Byte], offset: Int, len: Int): Value = Binary(bytes, offset, len)
  override def visitExt(tag: Byte, bytes: Array[Byte], offset: Int, len: Int): Value = Ext(tag, bytes, offset, len)
  override def visitTimestamp(instant: Instant): Value = Timestamp(instant)

  /**
    * Thrown when weepickle tries to convert a Value into a given data
    * structure but fails because part the blob is invalid
    *
    * @param data The section of the Value that weepickle tried to convert.
    *             This could be the entire blob, or it could be some subtree.
    * @param msg  Human-readable text saying what went wrong
    */
  case class InvalidData(data: Value, msg: String) extends Exception(s"$msg (data: $data)")

}

sealed trait Value extends FromInput {

  /**
    * Returns the `String` value of this [[Value]], fails if it is not
    * a [[Str]]
    */
  def str: CharSequence = this match {
    case Str(value) => value
    case _ => throw Value.InvalidData(this, "Expected Str")
  }

  /**
    * Returns an Optional `String` value of this [[Value]] in case this [[Value]] is a 'String'.
    */
  def strOpt: Option[CharSequence] = this match {
    case Str(value) => Some(value)
    case _ => None
  }

  /**
    * Returns the key/value map of this [[Value]], fails if it is not
    * a [[Obj]]
    */
  def obj: mutable.Map[String, Value] = this match {
    case Obj(value) => value
    case _ => throw Value.InvalidData(this, "Expected Obj")
  }

  /**
    * Returns an Optional key/value map of this [[Value]] in case this [[Value]] is a 'Obj'.
    */
  def objOpt: Option[mutable.Map[String, Value]] = this match {
    case Obj(value) => Some(value)
    case _ => None
  }

  /**
    * Returns the elements of this [[Value]], fails if it is not
    * a [[Arr]]
    */
  def arr: ArrayBuffer[Value] = this match {
    case Arr(value) => value
    case _ => throw Value.InvalidData(this, "Expected Arr")
  }

  /**
    * Returns The optional elements of this [[Value]] in case this [[Value]] is a 'Arr'.
    */
  def arrOpt: Option[ArrayBuffer[Value]] = this match {
    case Arr(value) => Some(value)
    case _ => None
  }

  /**
    * Returns the `BigDecimal` value of this [[Value]], fails if it is not
    * a numeric type.
    */
  def num: BigDecimal = this.numOpt match {
    case Some(value) => value
    case None => throw Value.InvalidData(this, "Expected numeric type")
  }

  /**
    * Returns an Option[BigDecimal] in case this [[Value]] is some numeric type.
    */
  def numOpt: Option[BigDecimal] = this match {
    case num: IsNumeric => Some(num.asBigDecimal)
    case _ => None
  }

  /**
    * Returns the `Long` value of this [[Value]], fails if it is not
    * a integer numeric type.
    */
  def int: Long = this.intOpt match {
    case Some(value) => value
    case None => throw Value.InvalidData(this, "Expected numeric type")
  }

  /**
    * Returns an Option[Long] in case this [[Value]] is some integer numeric type.
    */
  def intOpt: Option[Long] = this match {
    case num: IsInteger => Some(num.asLong)
    case _ => None
  }

  /**
    * Returns the `Boolean` value of this [[Value]], fails if it is not
    * a [[Bool]]
    */
  def bool: Boolean = this match {
    case Bool(value) => value
    case _ => throw Value.InvalidData(this, "Expected Bool")
  }

  /**
    * Returns an Optional `Boolean` value of this [[Value]] in case this [[Value]] is a 'Bool'.
    */
  def boolOpt: Option[Boolean] = this match {
    case Bool(value) => Some(value)
    case _ => None
  }

  /**
    * Returns true if the value of this [[Value]] is Null, false otherwise
    */
  def isNull: Boolean = this match {
    case Null => true
    case _ => false
  }

  def transform[T](to: Visitor[_, T]): T = Value.transform(this, to)

  def apply(s: Value.Selector): Value = s(this)

  def update(s: Value.Selector, v: Value): Unit = s(this) = v

  /**
    * Update a value in-place. Takes an `Int` or a `String`, through the
    * implicitly-construct [[Value.Selector]] type.
    *
    * We cannot just overload `update` on `s: Int` and `s: String` because
    * of type inference problems in Scala 2.11.
    */
  def update[V](s: Value.Selector, f: Value => V)(implicit v: V => Value): Unit = s(this) = v(f(s(this)))

  //  override def toString = transform(ToJson.string)
}

/**
  * A very small, very simple AST that one can use in weepickle to faithfully
  * represent something that can be visited (i.e., a data type for each visit* method)
  */

// Visitor.visitArray(length: Int): ArrVisitor[T, J]
case class Arr(value: ArrayBuffer[Value]) extends Value

object Arr {
  implicit def from[T](items: TraversableOnce[T])(implicit viewBound: T => Value): Arr =
    Arr(items.map(x => x: Value).to(mutable.ArrayBuffer))

  def apply(items: Value*): Arr = new Arr(items.to(mutable.ArrayBuffer))
}


// Visitor.visitObject(length: Int): ObjVisitor[T, J]
case class Obj(value: mutable.Map[String, Value]) extends Value

object Obj {
  implicit def from(items: TraversableOnce[(String, Value)]): Obj = {
    Obj(mutable.LinkedHashMap(items.toSeq: _*))
  }

  // Weird telescoped version of `apply(items: (String, Value)*)`, to avoid
  // type inference issues due to overloading the existing `apply` method
  // generated by the case class itself
  // https://github.com/lihaoyi/upickle/issues/230
  def apply[V](item: (String, V), items: (String, Value)*)(implicit viewBound: V => Value): Obj = {
    val map = new mutable.LinkedHashMap[String, Value]()
    map.put(item._1, item._2)
    for (i <- items) map.put(i._1, i._2)
    Obj(map)
  }

  def apply(): Obj = Obj(new mutable.LinkedHashMap[String, Value]())
}

// Visitor.visitNull(): J
case object Null extends Value

// Visitor.visitTrue(): J and Visitor.visitFalse(): J
sealed abstract class Bool extends Value { def value: Boolean }

object Bool {
  def apply(value: Boolean): Bool = if (value) True else False
  def unapply(bool: Bool): Option[Boolean] = Some(bool.value)
}

case object True extends Bool { def value = true }

case object False extends Bool { def value = false }

// Visitor.visitString(cs: CharSequence): J
case class Str(value: CharSequence) extends Value

// Visitor.visitChar(c: Char): J
case class Chr(c: Char) extends Value


// Can be represented as numeric without loss
trait IsNumeric { def asBigDecimal: BigDecimal}

// Can be represented as an integer without loss
trait IsInteger extends IsNumeric { def asLong: Long }

// Visitor.visitInt32(i: Int): J
case class Int32(i: Int) extends Value with IsInteger {
  override def asBigDecimal: BigDecimal = BigDecimal(i)
  override def asLong: Long = i
}

// Visitor.visitInt64(l: Long): J
case class Int64(l: Long) extends Value with IsInteger {
  override def asBigDecimal: BigDecimal = BigDecimal(l)
  override def asLong: Long = l
}

// Visitor.visitUInt64(ul: Long): J
case class UInt64(ul: Long) extends Value with IsInteger {
  override def asBigDecimal: BigDecimal = BigDecimal(ul)
  override def asLong: Long = ul
}

// Visitor.visitFloat32(d: Float): J
case class Float32(f: Float) extends Value with IsNumeric {
  override def asBigDecimal: BigDecimal = BigDecimal(f)
}

// Visitor.visitFloat64(d: Double): J
case class Float64(d: Double) extends Value with IsNumeric {
  override def asBigDecimal: BigDecimal = BigDecimal(d)
}

// Visitor.visitFloat64String(s: String): J
case class Float64String(s: String) extends Value with IsNumeric {
  override def asBigDecimal: BigDecimal = BigDecimal(s)
}

// Visitor.visitFloat64StringParts(cs: CharSequence, decIndex: Int, expIndex: Int): J
case class Float64StringParts(cs: CharSequence, decIndex: Int, expIndex: Int) extends Value with IsNumeric {
  override def asBigDecimal: BigDecimal = BigDecimal(cs.toString)
}

// Visitor.visitBinary(bytes: Array[Byte], offset: Int, len: Int): J
case class Binary(bytes: Array[Byte], offset: Int, len: Int) extends Value

// Visitor.visitExt(tag: Byte, bytes: Array[Byte], offset: Int, len: Int): J
case class Ext(tag: Byte, bytes: Array[Byte], offset: Int, len: Int) extends Value

// Visitor.visitTimestamp(instant: Instant): J
case class Timestamp(instant: Instant) extends Value

