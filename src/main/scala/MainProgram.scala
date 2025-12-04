//Import necessary libraries
import scala.io.{Source, Codec}
import scala.util.{Try, Success, Failure, Using}

// 1. Custom Exception
// Defined to handle specific data errors (like missing columns or bad numbers) neatly.
case class InvalidDataException(message: String) extends Exception(message)

object MainProgram:

  // Helper to print separator lines for readability
  def questionLineSpacing(): Unit = println("-" * 100)

  // 2. Safe Double Parser
  // This cleans up strings like "$1,200.50" or "15%" by removing special chars
  // and safely tries to convert them to a Double (returns Option).
  private def parseDoubleSafe(s: String): Option[Double] =
    val cleaned = s.replaceAll("[%$,]", "").trim
    Try(cleaned.toDouble).toOption

  // 3. Main Parsing Logic
  // Converts a raw CSV line into a HotelBooking object.
  def parseLine(line: String): Try[HotelBooking] =
    val cols = line.split(",").map(_.trim)

    // Basic check: Ensure we have enough columns to avoid "IndexOutOfBounds" errors
    if cols.length < 24 then
      Failure(InvalidDataException("Insufficient columns"))
    else
      // Small helper to convert an Option to a Try (Success or Failure)
      def validate[T](opt: Option[T], err: String): Try[T] =
        opt.toRight(InvalidDataException(err)).toTry

      // "For-Comprehension": This is a sequence of operations.
      // If ANY step fails (returns Failure), the whole thing stops and returns Failure.
      for
        // Check if essential text fields exist
        _ <- if cols(16).nonEmpty && cols(9).nonEmpty && cols(10).nonEmpty
        then Success(true)
        else Failure(InvalidDataException("Missing ID fields"))

        // Parse and validate numbers. filter(_ >= 0) ensures no negative prices.
        price <- validate(parseDoubleSafe(cols(20)).filter(_ >= 0.0), "Invalid Price")

        // Discounts are percentages (e.g., 20), so we divide by 100 to get decimals (0.2)
        discount = parseDoubleSafe(cols(21)).getOrElse(0.0) / 100.0

        profit <- validate(parseDoubleSafe(cols(23)), "Invalid Profit")

        people <- validate(Try(cols(11).toInt).toOption, "Invalid Person Count")

      // If all above succeed, create the object
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

  // 4. Normalization Formula
  // Converts a value to a 0.0 - 1.0 scale relative to the Min and Max of the dataset.
  // Includes a check `if (max - min).abs < 1e-9` to prevent dividing by zero if all values are the same.
  def minMaxNormalize(value: Double, min: Double, max: Double): Double =
    if (max - min).abs < 1e-9 then 0.5 else (value - min) / (max - min)

  def main(args: Array[String]): Unit =
    println("Reading data...")

    // 5. File Loading
    // "Using" safely opens and closes the file.
    // .drop(1) skips the CSV header row.
    // .collect keeps only the "Success" results (valid lines) and discards Failures.
    val bookingsAttempt = Using(Source.fromInputStream(getClass.getResourceAsStream("/Hotel_Dataset.csv"))(Codec("ISO-8859-1"))): source =>
      source.getLines().drop(1).map(parseLine)
        .collect:
          case Success(booking) => booking
        .toList

    // Handle whether the file load itself succeeded or failed
    val bookings: Seq[HotelBooking] = bookingsAttempt match
      case Success(data) => data
      case Failure(e) =>
        println(s"CRITICAL ERROR: Could not read file. ${e.getMessage}")
        List.empty

    if bookings.nonEmpty then
      println(s"Successfully loaded ${bookings.size} valid records.")
      questionLineSpacing()

      // --- QUESTION 1: Highest Bookings ---
      // Logic: Group by Country -> Count list size -> Find Max
      val (topCountry, maxBookings) = bookings
        .groupBy(_.destinationCountry)
        .map:
          case (country, list) => (country, list.size)
        .maxBy(_._2)

      println(s"1. Country with highest bookings: $topCountry ($maxBookings bookings)")
      questionLineSpacing()

      // --- QUESTION 2: Most Economical (Composite Score) ---
      // Step A: Calculate Averages per Hotel
      val statsByHotel = bookings
        .groupBy(b => (b.hotelName, b.destinationCountry, b.city)) // Group by unique hotel
        .map:
          case ((name, country, city), list) =>
            val count = list.size.toDouble
            // Average the metrics for this specific hotel
            val avgPrice = list.map(_.bookingPrice).sum / count
            val avgDiscount = list.map(_.discount).sum / count
            val avgProfit = list.map(_.profitMargin).sum / count

            val hotelKey = f"$name ($country - $city)"
            hotelKey -> (avgPrice, avgDiscount, avgProfit)

      if statsByHotel.nonEmpty then
        // Step B: Find Global Min/Max for Normalization
        val avgPrices = statsByHotel.values.map(_._1).toSeq
        val avgDiscounts = statsByHotel.values.map(_._2).toSeq
        val avgProfits = statsByHotel.values.map(_._3).toSeq

        val (minP, maxP) = (avgPrices.min, avgPrices.max)
        val (minD, maxD) = (avgDiscounts.min, avgDiscounts.max)
        val (minM, maxM) = (avgProfits.min, avgProfits.max)

        // Step C: Scoring
        val weightSum = 3.0 // Equal weighting (1+1+1)

        val scored = statsByHotel.map:
          case (hotel, (price, disc, margin)) =>
            // Normalize values to 0.0-1.0 range
            val normPrice = minMaxNormalize(price, minP, maxP)
            val normDiscount = minMaxNormalize(disc, minD, maxD)
            val normProfit = minMaxNormalize(margin, minM, maxM)

            // Invert Price: Low price (0.0) becomes High Score (1.0)
            val priceScore = 1.0 - normPrice
            // Discount: High discount (1.0) stays High Score (1.0)
            val discountScore = normDiscount
            // Invert Profit: Low margin becomes High Score (assuming "Economical" means low markup)
            val profitScore = 1.0 - normProfit

            // Weighted Average
            val combined = (priceScore + discountScore + profitScore) / weightSum
            hotel -> combined

        // Step D: Find Winner
        val (winnerName, winnerScore) = scored.maxBy(_._2)

        println(f"2. Most Economical Hotel (by Score): $winnerName (Score: $winnerScore%.4f)")
        println()

        // Display Top 3 for extra detail
        val top3 = scored.toSeq.sortBy(-_._2).take(3)
        println("   Top 3 by combined economy score:")
        top3.foreach:
          case (h, s) => println(f"     - $h (score: $s%.4f)")

      else
        println("2. Most Economical Hotel Options: no hotels to evaluate.")

      questionLineSpacing()

      // --- QUESTION 3: Most Profitable (Visitors & Margin) ---
      // Step A: Aggregate Data
      val profitStats = bookings
        .groupBy(b => (b.hotelName, b.destinationCountry, b.city))
        .map:
          case ((name, country, city), list) =>
            val totalVisitors = list.map(_.noOfPeople).sum.toDouble // Sum of all people
            val avgMargin = list.map(_.profitMargin).sum / list.size.toDouble // Average margin

            val hotelKey = f"$name ($country - $city)"
            hotelKey -> (totalVisitors, avgMargin)

      if profitStats.nonEmpty then
        // Step B: Min/Max for Visitors and Margin
        val allVisitors = profitStats.values.map(_._1).toSeq
        val allMargins = profitStats.values.map(_._2).toSeq

        val (minV, maxV) = (allVisitors.min, allVisitors.max)
        val (minM, maxM) = (allMargins.min, allMargins.max)

        // Step C: Calculate Score
        val scoredProfitability = profitStats.map:
          case (hotel, (visitors, margin)) =>
            // Normalize both (Higher is better for both visitors and margin)
            val visitorScore = minMaxNormalize(visitors, minV, maxV)
            val marginScore = minMaxNormalize(margin, minM, maxM)

            // Simple Average
            val finalScore = (visitorScore + marginScore) / 2.0
            hotel -> finalScore

        // Step D: Winner
        val (bestHotel, bestScore) = scoredProfitability.maxBy(_._2)
        val (winVisitors, winMargin) = profitStats(bestHotel)

        println(f"3. Most Profitable Hotel (by Score): $bestHotel")
        println(f"   - Combined Score: $bestScore%.4f")
        println(f"   - Total Visitors: ${winVisitors.toInt}")
        println(f"   - Average Margin: $winMargin%.2f")

      else
        println("3. Most Profitable Hotel: No data.")

      questionLineSpacing()

    else
      println("No data found.")