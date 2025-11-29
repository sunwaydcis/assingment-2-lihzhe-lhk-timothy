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

      //Read lines and save them as vals
      val lines = source.getLines().drop(1).toList

      //Close the source reading process for better memory management
      source.close()

      //Process the lines to return the mapped list values using hotel booking class's constructor
      //Properly assign and use the constructor according to the column values in the csv file
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
      //Catch any file reading errors using IOException
      case _: java.io.IOException =>
        println("Problem reading the file.")
        List.empty[HotelBooking]
      //Blanket catch for most other file access errors
      case e: Exception =>
        println(s"Error reading file: ${e.getMessage}")
        List.empty[HotelBooking]
