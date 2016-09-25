object model {

  sealed trait PaintType {
    def value: String
  }

  case object Matte extends PaintType {
    val value = "M"
  }

  case object Gloss extends PaintType {
    val value = "G"
  }

  object PaintType {

    implicit class PaintTypeOps(value: Char) {
      def asPaintType: PaintType = value match {
        case 'M' => Matte
        case 'G' => Gloss
        case _ => throw new IllegalArgumentException("Invalid paint type!")
      }
    }

  }

  case class Paint(colourId: Int, paintType: PaintType)

  case class Customer(paints: List[Paint])

}