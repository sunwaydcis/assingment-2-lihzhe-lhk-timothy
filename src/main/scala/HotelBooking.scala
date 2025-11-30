//package ProgramClasses

case class HotelBooking(
                         bookingId : String,
                         destinationCountry: String,
                         hotelName: String,
                         bookingPrice: Double,
                         discount: Double,
                         profitMargin: Double,
                         noOfPeople: Int
                       )