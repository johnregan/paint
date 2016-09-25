
import java.io.InputStream

import model._


object PaintShop {

  import PaintType._

  def solve(fileName: String): String = {
    val fileData: Iterator[String] = readFileData(fileName)

    val noColoursInBatch = fileData.next.toInt

    val customers: List[Customer] = customersFromFile(fileData)

    val baseCases = extractCustomerBaseCases(customers)

    val output = areBaseCasesValid(baseCases) match {
      case true =>
        val generatedBatch  = generatePaintsInBatch(customers)
        val fullBatch       = populateEmptyColours(noColoursInBatch, generatedBatch)
        fullBatch.sortBy(_.colourId).map(_.paintType.value).mkString(" ")
      case false =>
        "No solution exists"
    }
    println(output)
    output
  }

  def areBaseCasesValid(baseCases: List[Paint]): Boolean = {
    !baseCases.groupBy(_.colourId).exists(_._2.size > 1)
  }

  def extractCustomerBaseCases(customers: List[Customer]): List[Paint] = {
    val baseCases = for {
      c <- customers
      p <- c.paints
      if c.paints.size == 1
    } yield p

    baseCases.distinct
  }

  def populateEmptyColours(noColoursInBatch: Int, generatedBatch: List[Paint]): List[Paint] = {
    val nonBatchedColours = (1 to noColoursInBatch).map(colourId => colourId).diff(generatedBatch.map(_.colourId)).toList

    if (nonBatchedColours.isEmpty) generatedBatch else generatedBatch.union(nonBatchedColours.map(new Paint(_, Gloss)))
  }

  def readFileData(fileName: String): Iterator[String] = {
    val stream: InputStream = getClass.getResourceAsStream(fileName)
    scala.io.Source.fromInputStream(stream).getLines
  }

  def generatePaintsInBatch(initialCustomers: List[Customer]): List[Paint] = {

    def generate(paintsToBeChecked: List[Paint], remainingCustomers: List[Customer]): List[Paint] = {
      remainingCustomers match {
        case Nil => paintsToBeChecked
        case head :: tail if head.paints.intersect(paintsToBeChecked).nonEmpty => generate(paintsToBeChecked, tail)
        case head :: tail => generate(updatedList(paintsToBeChecked, head.paints), initialCustomers)
      }
    }

    def updatedList(paintsToBeChecked: List[Paint], customerPaint: List[Paint]): List[Paint] = {
      val differingPaintById = customerPaint.map(_.colourId).diff(paintsToBeChecked.map(_.colourId))
      val differingPaint = customerPaint.filter(paint => differingPaintById.contains(paint.colourId))
      val differingPaintGloss = differingPaint.filter(_.paintType == Gloss)
      if (differingPaintGloss.isEmpty) {
        paintsToBeChecked.union(differingPaint)
      } else {
        paintsToBeChecked.union(differingPaintGloss)
      }
    }
    generate(extractCustomerBaseCases(initialCustomers), initialCustomers)
  }

  def customersFromFile(fileData: Iterator[String]): List[Customer] = {
    (fileData map customerFromLine).toList
  }

  def customerFromLine(line: String): Customer = {
    val lineSpacesRemoved = line.replaceAll(" ", "")
    val colours = lineSpacesRemoved.toList.filter(_.isDigit)
    val paintTypes = lineSpacesRemoved.toList.filter(!_.isDigit)
    val colorsPaintTypes = colours.zip(paintTypes)

    val paints = colorsPaintTypes map { case (colourId, paintType) =>
      Paint(colourId.asDigit, paintType.asPaintType)
    }
    Customer(paints)
  }
}