object AnalyticsUtils:
  
  // Normalize a number between 0.0 and 1.0
  def minMaxNormalize(value: Double, min: Double, max: Double): Double =
    if (max - min).abs < 1e-9 then 0.5 else (value - min) / (max - min)

  // Finds min and max in a single pass instead of two
  def findMinMax(values: Iterable[Double]): (Double, Double) =
    values.foldLeft((Double.MaxValue, Double.MinValue)):
      case ((min, max), v) => (math.min(min, v), math.max(max, v))