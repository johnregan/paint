
import java.io.InputStream

import scala.collection.mutable

sealed trait PaintType {
  def value: String
}

case object Matte extends PaintType {
  val value = "M"
}

case object Gloss extends PaintType {
  val value = "G"
}

case class Paint(colourId: Integer, paintType: PaintType)

case class Customer(paints: List[Paint])

object PaintShop {

  def solve(fileName: String): String = {
    val fileData: Iterator[String] = getLinesFromFile(fileName)

    val noColoursInBatch = Integer.parseInt(fileData.next)

    val customers: List[Customer] = getCustomersFromFile(fileData)

    val baseCases = extractCustomerBaseCases(customers)

    if (areBaseCasesValid(baseCases)) {
      val generatedBatch = generatePaintsInBatch(customers)

      val fullBatch = populateEmptyColours(noColoursInBatch, generatedBatch)

      val output = fullBatch.sortBy(_.colourId).map(_.paintType.value).mkString(" ")

      println(output)
      output
    } else {
      val output = "No solution exists"
      println(output)
      output
    }
  }

  def areBaseCasesValid(baseCases: List[Paint]): Boolean = {
    if (baseCases.groupBy(_.colourId).map(_._2.size).filter(_ > 1).isEmpty) {
      true
    } else {
      false
    }
  }

  def extractCustomerBaseCases(customers: List[Customer]): List[Paint] = {
    val baseCases =
      (for (customer <- customers;
            paint <- customer.paints
            if (customer.paints.size == 1)
      ) yield (paint)).distinct

    baseCases
  }

  def populateEmptyColours(noColoursInBatch: Int, generatedBatch: List[Paint]): List[Paint] = {
    val nonBatchedColours = (1 to noColoursInBatch).map(colourId => colourId).diff(generatedBatch.map(_.colourId)).toList

    if (nonBatchedColours.isEmpty) generatedBatch else generatedBatch.union(nonBatchedColours.map(new Paint(_, Gloss)))
  }

  def getLinesFromFile(fileName: String): Iterator[String] = {
    val stream: InputStream = getClass.getResourceAsStream(fileName)
    val lines = scala.io.Source.fromInputStream(stream).getLines
    lines
  }

  def generatePaintsInBatch(initialCustomers: List[Customer]): List[Paint] = {

    def build(paintsToBeChecked: List[Paint], remainingCustomers: List[Customer]): List[Paint] = {
      remainingCustomers match {
        case Nil => paintsToBeChecked
        case head :: tail if (head.paints.intersect(paintsToBeChecked).size > 0) => build(paintsToBeChecked, tail)
        case head :: tail => build(getUpdatedList(paintsToBeChecked, head.paints), initialCustomers)
      }
    }

    def getUpdatedList(paintsToBeChecked: List[Paint], customerPaint: List[Paint]): List[Paint] = {
      val differingPaintById = customerPaint.map(_.colourId).diff(paintsToBeChecked.map(_.colourId))

      val differingPaint = customerPaint.filter(paint => differingPaintById.contains(paint.colourId))
      val differingPaintGloss = differingPaint.filter(_.paintType == Gloss)
      if (differingPaintGloss.isEmpty) {
        paintsToBeChecked.union(differingPaint)
      } else {
        paintsToBeChecked.union(differingPaintGloss)
      }
    }
    build(extractCustomerBaseCases(initialCustomers), initialCustomers)
  }

  def getCustomersFromFile(fileData: Iterator[String]): List[Customer] = {
    val mutableCustomers: mutable.MutableList[Customer] = mutable.MutableList()
    for (line <- fileData) {
      mutableCustomers += getCustomerFromLine(line)
    }
    mutableCustomers.toList
  }

  def getCustomerFromLine(line: String): Customer = {
    val lineSpacesRemoved = line.replaceAll(" ", "")
    val colours = lineSpacesRemoved.toList.filter(_.isDigit)
    val paintTypes = lineSpacesRemoved.toList.filter(!_.isDigit)
    val colorsPaintTypes = colours.zip(paintTypes)

    val customer = new Customer(colorsPaintTypes map { case (colourId: Char, paintType: Char) =>
      new Paint(colourId.asDigit, paintType match {
        case 'M' => Matte
        case 'G' => Gloss
      })
    })
    customer
  }
}