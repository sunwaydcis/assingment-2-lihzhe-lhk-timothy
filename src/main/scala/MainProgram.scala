import scala.io.{Source, Codec}
import scala.util.{Try, Success, Failure, Using}

// Custom Exception for cleaner error reporting
case class InvalidDataException(message: String) extends Exception(message)

object MainProgram:

  // Print a separator line
  def questionLineSpacing(): Unit = println("-" * 100)

  // Convert string into number safely
  private def parseDoubleSafe(s: String): Option[Double] =
    val cleaned = s.replaceAll("[%$,]", "").trim
    Try(cleaned.toDouble).toOption

  // Convert CSV line into HotelBooking object
  def parseLine(line: String): Try[HotelBooking] =
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

  // Normalize a number between 0.0 and 1.0
  def minMaxNormalize(value: Double, min: Double, max: Double): Double =
    if (max - min).abs < 1e-9 then 0.5 else (value - min) / (max - min)

  // Main execution flow
  def main(args: Array[String]): Unit =
    println("Reading data...")

    // Read CSV file
    val bookingsAttempt = Using(Source.fromInputStream(getClass.getResourceAsStream("/Hotel_Dataset.csv"))(Codec("ISO-8859-1"))): source =>
      source.getLines()
        .drop(1) // Skip CSV header
        .map(parseLine) // Parse every line
        .collect: // Filter
           case Success(booking) => booking
        .toList

    // Handle the result of the file read attempt
    val bookings: Seq[HotelBooking] = bookingsAttempt match
      case Success(data) => data
      case Failure(e) =>
        println(s"CRITICAL ERROR: Could not read file. ${e.getMessage}")
        List.empty

    // Proceed only if we have valid data
    if bookings.nonEmpty then
      println(s"Successfully loaded ${bookings.size} valid records.")
      questionLineSpacing()

      // Question 1: Country with most bookings
      // Group by country > Count list size > Find max
      val (topCountry, maxBookings) = bookings
        .groupBy(_.destinationCountry)
        .map:
           case (country, list) => (country, list.size)
        .maxBy(_._2)

      println(s"1. Country with highest bookings: $topCountry ($maxBookings bookings)")

      // Line spacing between questions
      questionLineSpacing()

      // Question 2: Most economical hostel by Score (Price, Discount and Margin)
      // Step 1: Aggregate data (Calculate averages per hotel + location)
      val statsByHotel = bookings
        .groupBy(b => (b.hotelName, b.destinationCountry, b.city))
        .map:
           case ((name, country, city), list) =>
             val count = list.size.toDouble
             val avgPrice = list.map(_.bookingPrice).sum / count
             val avgDiscount = list.map(_.discount).sum / count
             val avgProfit = list.map(_.profitMargin).sum / count

             val hotelKey = f"$name ($country - $city)"
             hotelKey -> (avgPrice, avgDiscount, avgProfit)

      if statsByHotel.nonEmpty then
        // Step 2: Find global min / max for normalization
        val avgPrices = statsByHotel.values.map(_._1).toSeq
        val avgDiscounts = statsByHotel.values.map(_._2).toSeq
        val avgProfits = statsByHotel.values.map(_._3).toSeq

        val (minP, maxP) = (avgPrices.min, avgPrices.max)
        val (minD, maxD) = (avgDiscounts.min, avgDiscounts.max)
        val (minM, maxM) = (avgProfits.min, avgProfits.max)

        // Weights (Can adjust for business logic)
        val wPrice = 1.0
        val wDiscount = 1.0
        val wProfit = 1.0
        val weightSum = wPrice + wDiscount + wProfit

        // Step 3: Calculate scores
        val scored = statsByHotel.map:
           case (hotel, (price, disc, margin)) =>
             val normPrice = minMaxNormalize(price, minP, maxP)
             val normDiscount = minMaxNormalize(disc, minD, maxD)
             val normProfit = minMaxNormalize(margin, minM, maxM)

             // Low Price and Margin are better, so we reverse (1.0 - norm). But high discount is better so keep it
             val priceScore = 1.0 - normPrice
             val discountScore = normDiscount
             val profitScore = 1.0 - normProfit

             val combined = (priceScore * wPrice + discountScore * wDiscount + profitScore * wProfit) / weightSum
             hotel -> combined

        // Find winner
        val (winnerName, winnerScore) = scored.maxBy(_._2)

        println(f"2. Most Economical Hotel (by Score): $winnerName (Score: $winnerScore%.4f)")
        println()

        // Show top 3
        val top3 = scored.toSeq.sortBy(-_._2).take(3)
        println("   Top 3 by combined economy score:")
        top3.foreach:
           case (h, s) => println(f"     - $h (score: $s%.4f)")

      else
        println("2. Most Economical Hotel Options: no hotels to evaluate.")

      // Line spacing between questions
      questionLineSpacing()

      // Question 3: Most profitable hotel by score (Visitor and Margin)
      // Step 1: Aggregate Data
      val profitStats = bookings
        .groupBy(b => (b.hotelName, b.destinationCountry, b.city))
        .map:
          case ((name, country, city), list) =>
            // Metric 1: Total volume (Sum of people)
            val totalVisitors = list.map(_.noOfPeople).sum.toDouble
            // Metric 2: Efficiency (Average Margin)
            val avgMargin = list.map(_.profitMargin).sum / list.size.toDouble

            val hotelKey = f"$name ($country - $city)"
            hotelKey -> (totalVisitors, avgMargin)

      if profitStats.nonEmpty then
        // Step 2: Global min / max
        val allVisitors = profitStats.values.map(_._1).toSeq
        val allMargins = profitStats.values.map(_._2).toSeq

        val (minV, maxV) = (allVisitors.min, allVisitors.max)
        val (minM, maxM) = (allMargins.min, allMargins.max)

        // Step 3: Score calculation
        val scoredProfitability = profitStats.map:
          case (hotel, (visitors, margin)) =>
            val visitorScore = minMaxNormalize(visitors, minV, maxV)

            val marginScore = minMaxNormalize(margin, minM, maxM)

            // High visitors and high margin are both better (No reversal needed)
            val finalScore = (visitorScore + marginScore) / 2.0
            hotel -> finalScore

        val (bestHotel, bestScore) = scoredProfitability.maxBy(_._2)

        val (winVisitors, winMargin) = profitStats(bestHotel)

        println(f"3. Most Profitable Hotel (by Score): $bestHotel")
        println(f"   - Combined Score: $bestScore%.4f")
        println(f"   - Total Visitors: ${winVisitors.toInt}")
        println(f"   - Average Margin: $winMargin%.2f")

      else
        println("3. Most Profitable Hotel: No data.")

      // Line spacing between questions
      questionLineSpacing()

    else
      println("No data found.")
