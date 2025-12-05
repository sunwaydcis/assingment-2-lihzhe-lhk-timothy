// Case class to hold the parsed data

case class HotelBooking(
                         bookingId: String,
                         destinationCountry: String,
                         city: String,
                         hotelName: String,
                         bookingPrice: Double,
                         discount: Double,
                         profitMargin: Double,
                         noOfPeople: Int
                       )