import scala.io.Source
//To use ISO codec instead of default utf8 for this specific csv
import scala.io.Codec
import HotelBooking.*

// Main Program Function
@main def runMainProgram(): Unit =

  //Read the csv file and record them into lists
  // Load and parse the CSV file
  val bookings: List[HotelBooking] =
    try
      //val source = Source.fromResource("Hotel_Dataset.csv")
      val source = Source.fromResource("Hotel_Dataset.csv")(Codec("ISO-8859-1"))

      // 1. Read lines into memory immediately
      val lines = source.getLines().drop(1).toList

      source.close()

      // 2. Process the lines and return the result
      lines.map { line =>
        val cols = line.split(",")
        // Note: Make sure your column indices match your CSV structure exactly
        val bookingId = cols(0)
        val destinationCountry = cols(9)
        val hotelName = cols(16)
        val bookingPrice = cols(20).toDouble
        val discount = cols(21).replace("%", "").toDouble / 100
        val profitMargin = cols(23).toDouble
        val noOfPeople = cols(11).toInt

        HotelBooking(bookingId, destinationCountry, hotelName, bookingPrice, discount, profitMargin, noOfPeople)
      }
    catch
      case _: java.io.IOException =>
        println("Problem reading the file.")
        List.empty[HotelBooking]
      case e: Exception =>
        println(s"Error reading file: ${e.getMessage}")
        List.empty[HotelBooking]
