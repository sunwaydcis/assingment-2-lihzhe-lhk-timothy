import scala.io.Source

// Define a case class to represent the relevant fields from the dataset
case class HotelBooking(
                         bookingId : String,
                         destinationCountry: String,
                         hotelName: String,
                         bookingPrice: Double,
                         discount: Double,
                         profitMargin: Double,
                         noOfPeople: Int
                       )