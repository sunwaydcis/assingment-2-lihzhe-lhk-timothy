// Custom Exception for cleaner error reporting
case class InvalidDataException(message: String) extends Exception(message)

// Helper class to aggregate data in O(1) memory
case class StatAccumulator(sum1: Double, sum2: Double, sum3: Double, count: Int):
  def +(other: StatAccumulator): StatAccumulator =
    StatAccumulator(sum1 + other.sum1, sum2 + other.sum2, sum3 + other.sum3, count + other.count)