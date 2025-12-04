import scala.io.{Source, Codec}
import scala.util.{Try, Success, Failure, Using}


case class InvalidDataException(message: String) extends Exception(message)

object MainProgram:

  def questionLineSpacing(): Unit = println("-" * 100)

  private def parseDoubleSafe(s: String): Option[Double] =
    val cleaned = s.replaceAll("[%$,]", "").trim
    Try(cleaned.toDouble).toOption

  def parseLine(line: String): Try[HotelBooking] =
    val cols = line.split(",").map(_.trim)

    if cols.length < 24 then
      Failure(InvalidDataException("Insufficient columns in CSV row"))
    else
      val rawName = cols(16)
      val rawCountry = cols(9)
      val rawCity = cols(10)

      if rawName.isEmpty || rawCountry.isEmpty || rawCity.isEmpty then
        Failure(InvalidDataException("Missing essential ID fields (Name/Country/City)"))
      else
        Try:
          val rawPrice = cols(20)
          val rawDiscount = cols(21)
          val rawProfit = cols(23)
          val rawPeople = cols(11)

          val price = parseDoubleSafe(rawPrice).filter(_ >= 0.0)
            .getOrElse(throw InvalidDataException("Invalid Price"))

          val discount = parseDoubleSafe(rawDiscount).getOrElse(0.0) / 100.0

          val profit = parseDoubleSafe(rawProfit)
            .getOrElse(throw InvalidDataException("Invalid Profit"))

          val people = Try(rawPeople.toInt).toOption
            .getOrElse(throw InvalidDataException("Invalid Person Count"))

          HotelBooking(
            bookingId = cols(0),
            destinationCountry = rawCountry,
            city = rawCity,
            hotelName = rawName,
            bookingPrice = price,
            discount = discount,
            profitMargin = profit,
            noOfPeople = people
          )

  def minMaxNormalize(value: Double, min: Double, max: Double): Double =
    if (max - min).abs < 1e-9 then 0.5 else (value - min) / (max - min)

  def main(args: Array[String]): Unit =
    println("Reading data...")

    val bookingsAttempt = Using(Source.fromInputStream(getClass.getResourceAsStream("/Hotel_Dataset.csv"))(Codec("ISO-8859-1"))): source =>
      source.getLines().drop(1).map(parseLine)
        .collect:
           case Success(booking) => booking
        .toList

    val bookings: Seq[HotelBooking] = bookingsAttempt match
      case Success(data) => data
      case Failure(e) =>
        println(s"CRITICAL ERROR: Could not read file. ${e.getMessage}")
        List.empty

    if bookings.nonEmpty then
      println(s"Successfully loaded ${bookings.size} valid records.")
      questionLineSpacing()

      // Question 1
      val (topCountry, maxBookings) = bookings
        .groupBy(_.destinationCountry)
        .map:
           case (country, list) => (country, list.size)
        .maxBy(_._2)

      println(s"1. Country with highest bookings: $topCountry ($maxBookings bookings)")
      questionLineSpacing()

      // Question 2: Economy Score
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
        val avgPrices = statsByHotel.values.map(_._1).toSeq
        val avgDiscounts = statsByHotel.values.map(_._2).toSeq
        val avgProfits = statsByHotel.values.map(_._3).toSeq

        val (minP, maxP) = (avgPrices.min, avgPrices.max)
        val (minD, maxD) = (avgDiscounts.min, avgDiscounts.max)
        val (minM, maxM) = (avgProfits.min, avgProfits.max)

        val wPrice = 1.0
        val wDiscount = 1.0
        val wProfit = 1.0
        val weightSum = wPrice + wDiscount + wProfit

        val scored = statsByHotel.map:
           case (hotel, (price, disc, margin)) =>
             val normPrice = minMaxNormalize(price, minP, maxP)
             val normDiscount = minMaxNormalize(disc, minD, maxD)
             val normProfit = minMaxNormalize(margin, minM, maxM)

             val priceScore = 1.0 - normPrice
             val discountScore = normDiscount
             val profitScore = 1.0 - normProfit

             val combined = (priceScore * wPrice + discountScore * wDiscount + profitScore * wProfit) / weightSum
             hotel -> combined

        val (winnerName, winnerScore) = scored.maxBy(_._2)

        val bestPrice = bookings.minBy(_.bookingPrice)
        val bestDisc = bookings.maxBy(_.discount)
        val bestMargin = bookings.minBy(_.profitMargin)

        println("2. Most Economical Hotel")

        println(f"   (Single View) Lowest Price:  ${bestPrice.hotelName} (${bestPrice.destinationCountry} - ${bestPrice.city}) (SGD ${bestPrice.bookingPrice}%.2f)")
        println(f"   (Single View) Best Discount: ${bestDisc.hotelName} (${bestDisc.destinationCountry} - ${bestDisc.city}) (${bestDisc.discount * 100}%.0f%%)")
        println(f"   (Single View) Low Margin:    ${bestMargin.hotelName} (${bestMargin.destinationCountry} - ${bestMargin.city}) (${bestMargin.profitMargin}%.2f)")
        println()
        println(f"   (Combined Score Winner): $winnerName (Score: $winnerScore%.4f)")
        println()

        val top3 = scored.toSeq.sortBy(-_._2).take(3)
        println("   Top 3 by combined economy score:")
        top3.foreach:
           case (h, s) => println(f"     - $h (score: $s%.4f)")

      else
        println("2. Most Economical Hotel Options: no hotels to evaluate.")

      questionLineSpacing()

      // Question 3
      val profitStats = bookings
        .groupBy(b => (b.hotelName, b.destinationCountry, b.city))
        .map:
          case ((name, country, city), list) =>
            val totalVisitors = list.map(_.noOfPeople).sum.toDouble
            val avgMargin = list.map(_.profitMargin).sum / list.size.toDouble

            val hotelKey = f"$name ($country - $city)"
            hotelKey -> (totalVisitors, avgMargin)

      if profitStats.nonEmpty then
        val allVisitors = profitStats.values.map(_._1).toSeq
        val allMargins = profitStats.values.map(_._2).toSeq

        val (minV, maxV) = (allVisitors.min, allVisitors.max)
        val (minM, maxM) = (allMargins.min, allMargins.max)

        val scoredProfitability = profitStats.map:
          case (hotel, (visitors, margin)) =>
            val visitorScore = minMaxNormalize(visitors, minV, maxV)

            val marginScore = minMaxNormalize(margin, minM, maxM)

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

      questionLineSpacing()

    else
      println("No data found.")