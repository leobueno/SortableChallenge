import java.util.Date

import play.api.libs.functional.syntax._
import play.api.libs.json._

import play.api.libs.json.Reads.dateReads
import play.api.libs.json.Writes.dateWrites

/**
  * Created by Leonardo on 21/01/2016.
  */
object JsonParsing {
  /**
    * Readers and Writers to serialize and deserialize json strings
    */
  implicit val isoDateReads = dateReads("yyyy-MM-dd'T'HH:mm:ss.SSSXXX")

  implicit val isoDateWrites = dateWrites("yyyy-MM-dd'T'HH:mm:ss.SSSXXX")

  implicit val productReads: Reads[Product] = (
    (JsPath \ "model").read[String] and
      (JsPath \ "announced-date").read[Date] and
      (JsPath \ "product_name").read[String] and
      (JsPath \ "family").readNullable[String] and
      (JsPath \ "manufacturer").read[String]
    ) (Product.apply _)

  implicit val productWrites: Writes[Product] = (
    (JsPath \ "model").write[String] and
      (JsPath \ "announced-date").write[Date] and
      (JsPath \ "product_name").write[String] and
      (JsPath \ "family").writeNullable[String] and
      (JsPath \ "manufacturer").write[String]
    ) (unlift(Product.unapply))

  implicit val listingReads: Reads[Listing] = (
    (JsPath \ "currency").read[String] and
      (JsPath \ "price").read[BigDecimal] and
      (JsPath \ "manufacturer").read[String] and
      (JsPath \ "title").read[String]
    ) (Listing.apply _)

  implicit val listingWrites: Writes[Listing] = (
    (JsPath \ "currency").write[String] and
      (JsPath \ "price").write[BigDecimal] and
      (JsPath \ "manufacturer").write[String] and
      (JsPath \ "title").write[String]
    ) (unlift(Listing.unapply))

  implicit val productWithListingsWrites = new Writes[(String, List[Listing])] {
    override def writes(modelWithListings: (String, List[Listing])): JsValue = {
      val (model, listings) = modelWithListings
      Json.obj(
        "product_name" -> model,
        "listings" -> JsArray(listings.map(Json.toJson(_)))
      )
    }
  }
}
