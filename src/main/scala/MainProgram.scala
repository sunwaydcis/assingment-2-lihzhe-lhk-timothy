object MainProgram:

  // Print a separator line
  def questionLineSpacing(): Unit = println("-" * 100)

  // Main execution flow
  def main(args: Array[String]): Unit =
    println("Reading data...")

    // Delegate loading to CsvParser
    val bookings = CsvParser.loadFromResource("/Hotel_Dataset.csv")

    if bookings.nonEmpty then
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

      // Question 2: Most economical hostel by score (Price, Discount and Margin)
      // groupMapReduce calculates sums without creating lists
      val rawStats = bookings.groupMapReduce(b => (b.hotelName, b.destinationCountry, b.city))
        (b => StatAccumulator(b.bookingPrice, b.discount, b.profitMargin, 1)) // Map
        (_ + _) // Reduce

      // Step 1: Convert sums to averages
      val statsByHotel = rawStats.map:
         case (key, stats) =>
           val count = stats.count.toDouble
           val avgPrice = stats.sum1 / count
           val avgDiscount = stats.sum2 / count
           val avgProfit = stats.sum3 / count

           val (name, country, city) = key
           val hotelKey = f"$name ($country - $city)"
           hotelKey -> (avgPrice, avgDiscount, avgProfit)

      if statsByHotel.nonEmpty then
        // Step 2: Find global min / max for normalization
        val avgPrices = statsByHotel.values.map(_._1)
        val avgDiscounts = statsByHotel.values.map(_._2)
        val avgProfits = statsByHotel.values.map(_._3)

        // Delegate math to AnalyticsUtils
        val (minP, maxP) = AnalyticsUtils.findMinMax(avgPrices)
        val (minD, maxD) = AnalyticsUtils.findMinMax(avgDiscounts)
        val (minM, maxM) = AnalyticsUtils.findMinMax(avgProfits)

        // Weights (Can adjust for business logic)
        val wPrice = 1.0
        val wDiscount = 1.0
        val wProfit = 1.0
        val weightSum = wPrice + wDiscount + wProfit

        // Step 3: Calculate scores
        val scored = statsByHotel.map:
          case (hotel, (price, disc, margin)) =>
            val normPrice = AnalyticsUtils.minMaxNormalize(price, minP, maxP)
            val normDiscount = AnalyticsUtils.minMaxNormalize(disc, minD, maxD)
            val normProfit = AnalyticsUtils.minMaxNormalize(margin, minM, maxM)

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
      // Step 1: Aggregate data
      val rawProfitStats = bookings.groupMapReduce(b => (b.hotelName, b.destinationCountry, b.city))
        (b => StatAccumulator(b.noOfPeople.toDouble, b.profitMargin, 0.0, 1))
        (_ + _)

      val profitStats = rawProfitStats.map:
         case (key, stats) =>
            val totalVisitors = stats.sum1 // Sum
            val avgMargin = stats.sum2 / stats.count.toDouble // Average

            val (name, country, city) = key
            val hotelKey = f"$name ($country - $city)"
            hotelKey -> (totalVisitors, avgMargin)

      if profitStats.nonEmpty then
        // Step 2: Global min / max
        val allVisitors = profitStats.values.map(_._1)
        val allMargins = profitStats.values.map(_._2)

        val (minV, maxV) = AnalyticsUtils.findMinMax(allVisitors)
        val (minM, maxM) = AnalyticsUtils.findMinMax(allMargins)

        // Step 3: Score calculation
        val scoredProfitability = profitStats.map:
          case (hotel, (visitors, margin)) =>
            val visitorScore = AnalyticsUtils.minMaxNormalize(visitors, minV, maxV)
            val marginScore = AnalyticsUtils.minMaxNormalize(margin, minM, maxM)

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