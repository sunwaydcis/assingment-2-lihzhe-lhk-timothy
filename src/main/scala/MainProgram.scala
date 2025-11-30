import scala.io.Source
//To use ISO codec instead of default utf8 for this specific csv
import scala.io.Codec
//import ProgramClasses.HotelBooking

//Function to print lines in spaces between questions
def questionLineSpacing(): Unit =
  println("-" * 100)

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


  if bookings.nonEmpty then
    //spacing before questions
    questionLineSpacing()
    // Question 1: Which country has the highest number of bookings?
    val (topCountry, maxBookings) = bookings
      .groupBy(x => x.destinationCountry)
      .map { case (country, list) => (country, list.size) }
      .maxBy(x => x._2)
      //Extract the country name with the highest amount of bookings

    println(s"1. Country with the highest number of bookings: $topCountry (With $maxBookings total bookings)")

    //spacing between questions
    questionLineSpacing()

    // Question 2
    val bestPriceHotel = bookings.minBy(_.bookingPrice)
    val (bestPriceHotelName, bestPriceHotelPrice) = (bestPriceHotel.hotelName, bestPriceHotel.bookingPrice)
    val bestDiscountHotel = bookings.maxBy(_.discount)
    val (bestDiscountHotelName, bestDiscountHotelDiscount) = (bestDiscountHotel.hotelName, bestDiscountHotel.discount)
    val lowestMarginHotel = bookings.minBy(_.profitMargin)
    val (lowestMarginHotelName, lowestMarginHotelMargin) = (lowestMarginHotel.hotelName, lowestMarginHotel.profitMargin)

    println("2. Most Economical Hotel Options")
    println(f"   a. Best Booking Price: $bestPriceHotelName (With a booking price of SGD ${bestPriceHotelPrice}%.2f)")
    println(f"   b. Best Discount: $bestDiscountHotelName (With a discount of ${bestDiscountHotelDiscount * 100}%.0f percent)")
    println(s"   c. Best Profit Margin (Lowest): $lowestMarginHotelName (With a margin of $lowestMarginHotelMargin)")

    //spacing between questions
    questionLineSpacing()

    
  else
    println("No data found.")