package querki.graphql

import sangria.ast.{BooleanValue, EnumValue, StringValue, Value}

trait GraphQLValue[T <: Value, R] {
  def name: String
  def fromValue(v: Value): Option[T]
  def value(t: T): R
}

object GraphQLValue {

  implicit val BooleanV = new GraphQLValue[BooleanValue, Boolean] {
    val name = "Boolean"

    def fromValue(v: Value): Option[BooleanValue] = {
      v match {
        case b: BooleanValue => Some(b)
        case _               => None
      }
    }

    def value(t: BooleanValue): Boolean = t.value
  }

  implicit val StringV = new GraphQLValue[StringValue, String] {
    val name = "String"

    def fromValue(v: Value): Option[StringValue] = {
      v match {
        case b: StringValue => Some(b)
        case _              => None
      }
    }

    def value(t: StringValue): String = t.value
  }

  implicit val EnumV = new GraphQLValue[EnumValue, String] {
    val name = "Enum"

    def fromValue(v: Value): Option[EnumValue] = {
      v match {
        case b: EnumValue => Some(b)
        case _            => None
      }
    }

    def value(t: EnumValue): String = t.value
  }
}
