import scala.io.{Source, Codec}
import scala.util.{Try, Success, Failure, Using}

object CsvParser:

  // Hold the file reading logic
  def loadFromResource(fileName: String): Seq[HotelBooking] =

    // Read CSV file
    val bookingsAttempt = Using(Source.fromInputStream(getClass.getResourceAsStream(fileName))(Codec("ISO-8859-1"))): source =>
      source.getLines()
        .drop(1) // Skip CSV header
        .map(parseLine) // Parse every line
        .collect: // Filter
          case Success(booking) => booking
        .toList

    // Handle the result
    bookingsAttempt match
      case Success(data) =>
        println(s"Successfully loaded ${data.size} valid records.")
        data
      case Failure(e) =>
        println(s"CRITICAL ERROR: Could not read file. ${e.getMessage}")
        List.empty

// Private Helpers
// Convert string into number safely
private def parseDoubleSafe(s: String): Option[Double] =
  val cleaned = s.replaceAll("[%$,]", "").trim
  Try(cleaned.toDouble).toOption

// Convert CSV line into HotelBooking object
private def parseLine(line: String): Try[HotelBooking] =
  val cols = line.split(",").map(_.trim)

  // Ensure row has enough columns
  if cols.length < 24 then
    Failure(InvalidDataException("Insufficient columns"))
  else
    // Helper to convert Option to Try
    def validate[T](opt: Option[T], err: String): Try[T] =
      opt.toRight(InvalidDataException(err)).toTry

    // Validate key fields and numbers
    for
      // Check 1: Ensure ID fields are not empty
      _ <- if cols(16).nonEmpty && cols(9).nonEmpty && cols(10).nonEmpty
           then Success(true)
           else Failure(InvalidDataException("Missing ID fields"))

      // Check 2: Parse and validate numbers
      price <- validate(parseDoubleSafe(cols(20)).filter(_ >= 0.0), "Invalid Price")

      // Discount defaults to 0.0 if missing / invalid
      discount = parseDoubleSafe(cols(21)).getOrElse(0.0) / 100.0

      profit <- validate(parseDoubleSafe(cols(23)), "Invalid Profit")

      people <- validate(Try(cols(11).toInt).toOption, "Invalid Person Count")

    yield
      HotelBooking(
        bookingId = cols(0),
        destinationCountry = cols(9),
        city = cols(10),
        hotelName = cols(16),
        bookingPrice = price,
        discount = discount,
        profitMargin = profit,
        noOfPeople = people
      )