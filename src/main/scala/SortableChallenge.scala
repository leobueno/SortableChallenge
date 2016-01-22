import java.time.{Duration, LocalDateTime}
import java.util.Date

import play.api.libs.json._

import FileIO._
import JsonParsing._

import java.io.File

//Import methods used for normalizing model/manufacturer names
import Normalization._

/**
  * A product matcher implemented using heuristics
  *
  * @param products the list of products against which we will be matching listings
  */
class HeuristicsProductMatcher(products: List[Product]) {

  // <editor-fold desc="Regular expressions used for indexing and generating keys for matching">
  /** dsc -> digital still camera
    * dmc -> digital media camera
    * dslr -> digital single-lens reflex
    * See: http://photo.net/digital-camera-forum/00Y0SA
    */
  val DSC_DMC_DSLR_NON_MATCHING_GROUP = "(?:dsc|dmc|dslr)"

  /** is -> Image Stabilization
    * hd -> High Definition
    * hs -> high sensitivity
    * c -> Compact
    * fd -> face detection
    * See: https://en.wikipedia.org/wiki/Canon_Digital_IXUS
    */

  val IGNORABLE_SUFFIX_NON_MATCHING_GROUP = "(?:hd|is|hs|c|fd|exr)"

  val MODEL_WITH_IGNORABLE_SUFFIX = s"""^(.+) $IGNORABLE_SUFFIX_NON_MATCHING_GROUP$$""".r

  /**
    * Sometimes models will be prefixed with acronyms for digital still camera or digital media camera
    */
  val MODEL_WITH_IGNORABLE_PREFIX =
    s"""^$DSC_DMC_DSLR_NON_MATCHING_GROUP-(.+)$$""".r

  /**
    * Sometimes models will be suffixed with acronyms for high definition, image stabilization and
    */
  val MODEL_WITH_IGNORABLE_PREFIX_AND_SUFFIX =
    s"""^$DSC_DMC_DSLR_NON_MATCHING_GROUP-(.+) $IGNORABLE_SUFFIX_NON_MATCHING_GROUP$$""".r

  val DISCARD_AFTER_FOR_REGEX = "(.+) (?:for|For|für|pour|para) .+".r

  val DISCARD_AFTER_WITH_REGEX = "(.+) with .+".r

  val DISCARD_AFTER_W_SLASH_REGEX = "(.+) w/ .+".r

  val KEY_IN_LISTING_WITH_MODEL_PREFIX = s"""^$DSC_DMC_DSLR_NON_MATCHING_GROUP(.+)$$""".r

  /**
    * k, b -> Black
    * r -> Red
    * s -> Silver
    * l,a -> Blue
    * p -> Pink
    * v -> Violet
    * d -> orange
    * t -> Chocolat
    * w -> white
    * n -> champagNe gold
    * g -> green
    * h -> grey
    *
    */
  val COLOR_NON_MATCHING_GROUP = "(?:k|b|r|s|l|a|p|v|d|t|w|n|g)"

  val KEY_IN_LISTING_WITH_MODEL_COLOR = s"""^$DSC_DMC_DSLR_NON_MATCHING_GROUP?(.+)$COLOR_NON_MATCHING_GROUP$$""".r

  /** At least for panasonic
    * s -> exclusively Japan domestic model)
    * P/PC/PL -> North America
    * EB -> UK
    * EF -> France
    * EG / EGM /GN /keg-> Germany
    * EE -> Russian
    * GD -> Korea
    * GT/GK -> (China)
    */
  val KEY_IN_LISTING_WITH_MODEL_COUNTRY_AND_COLOR =
    s"""^$DSC_DMC_DSLR_NON_MATCHING_GROUP?(.+)(?:s|p|pc|pl|eb|ef|keg|eg|ee|gd|gt|gk)$COLOR_NON_MATCHING_GROUP$$""".r

  val KEY_IN_LISTING_WITH_MODEL_SUFFIX = s"""^$DSC_DMC_DSLR_NON_MATCHING_GROUP?(.+\\d)$IGNORABLE_SUFFIX_NON_MATCHING_GROUP$$""".r
  // </editor-fold>

  /** Create our database of products
    * One or more index keys will be generated for each product and the Map will contain one or more entry for each product
    * For details on how the indexing keys are generated, check [[generateIndexingKeysAndProductTuples(Product)]]
    */
  val database: Map[String, Product] = products.flatMap(generateIndexingKeysAndProductTuples).toMap

  /**
    * Finds the best product match for a listing
    *
    * @param listing the listing to match against our product database
    * @return the best match
    */
  def findMatch(listing: Listing) : Option[Product] = {

    val relevantTitleSection = extractRelevantTitleSection(listing)

    val keysToMatch: Array[String] = generateKeysToMatch(relevantTitleSection)

    val possibleMatches = findAllPossibleMatches(listing, keysToMatch)

    selectBestMatch(possibleMatches)

  }

  /** Look up products in te database by keys, if a possible match is found, check for false positives to confirm
    * the match
    *
    * @param listing     The listing to be matched agains the product database
    * @param keysToMatch All the keys we will search for in the product database
    * @return All possible matches found
    */
  def findAllPossibleMatches(listing: Listing, keysToMatch: Array[String]): Seq[(String, Product)] = {
    keysToMatch.flatMap {
      keyToBeChecked =>
        val maybeAMatch = database.get(keyToBeChecked)
        maybeAMatch match {
          case Some(product) =>
            // Perform additional validations to check if the match is not a false positive
            if (isFalsePositiveMatch(listing, product)) {
              // Match was not confirmed
              None
            } else {
              //match confirmed
              Some((keyToBeChecked, product))
            }
          case None => None
        }
    }
  }

  /** Selects the best match among all possible matches found
    * We will elect the match with the longest match key as the best match
    *
    * @param possibleMatches ALl the possible matches found, may be an empty Seq
    * @return The best match found or None if no match was found
    */
  def selectBestMatch(possibleMatches: Seq[(String, Product)]): Option[Product] = {
    if (possibleMatches.length > 1) {
      val (_, bestProduct) = possibleMatches.maxBy { case (key, product) => key.length }
      Some(bestProduct)
    } else {
      //The trivial case, when we have zero or only one match
      possibleMatches.headOption.map{case (_, bestProduct) => Some(bestProduct)}.getOrElse(None)
    }
  }

  /** Check if the match is a false positive
    *
    * @param listing the listing we are trying to find matching products for
    * @param product a possible product match
    * @return True if the match is a false positive, false if it's a valid match
    */
  def isFalsePositiveMatch(listing: Listing, product: Product): Boolean = {
    product.manufacturer != listing.manufacturer && product.manufacturer != listing.alternativeManufacturer
  }

  /** Returns only the relevant title section of the listing
    * e.g:
    * - Panasonic Lumix DMW-BCG10 '''for''' Lumix ZS7, ZS5, TZ10, TZ8 Series => Panasonic Lumix DMW-BCG10
    * - Sony T Series DSC-T99 14.1 Megapixel DSC Camera with Super HAD CCD Image Sensor (Silver) => - Sony T Series DSC-T99 14.1 Megapixel DSC Camera
    * - Nikon D3100 Body Only Black w/ HD Video
    *
    * @param listing the listing from which we want to extract the relevant title section
    * @return the relevant title section of the listing
    */
  def extractRelevantTitleSection(listing: Listing): String = {
    val relevantSection = listing.title match {
      case DISCARD_AFTER_FOR_REGEX(firstPart) => firstPart
      case DISCARD_AFTER_WITH_REGEX(firstPart) => firstPart
      case DISCARD_AFTER_W_SLASH_REGEX(firstPart) => firstPart
      case str: String => str
    }
    relevantSection
  }


  /////////////////////////////////////////////////////////////////////////////////////////////////
  // Methods used for indexing the products
  /////////////////////////////////////////////////////////////////////////////////////////////////

  /** Generates many tubles of (index, produc) for a given product
    * Starts with the simples match possible (normalizedModel as key) and add variations,
    * generated by adding the family name, dropping prefixes/suffixes that sometimes are
    * not present in the listings, etc.
    */
  def generateIndexingKeysAndProductTuples(product: Product): Seq[(String, Product)] = {
    val allIndexingKeys = Seq(product.normalizedModel) ++
      alternateIndexingKeysWithFamilyName(product.family, product.normalizedModel) ++
      alternateIndexingKeysBaseOnProductName(product) ++
      alternateIndexingKeysDroppingPrefixesOrSuffixes(product)

    //We may have some duplications
    allIndexingKeys.distinct.map(indexingKey => (indexingKey, product))
  }

  /** Generate indexing keys for looking up using family + model
    *
    * There are 2 distinct cases:
    *
    * a) Family is a simple name, like FinePix: In this case we will index using only one prefix, finepix
    * b) Family is a compound name, like Digital IXUS: In this case we will index using prefixes digitalixus and ixus
    *
    * @param familyOpt     The product family or none if the family is not available
    * @param baseModelName The base model name to use when generating alternative index keys
    * @return The alternative indexing keys created using the family name
    */
  def alternateIndexingKeysWithFamilyName(familyOpt: Option[String], baseModelName: String): Seq[String] = {
    familyOpt match {
      case Some(family) =>

        val familyTokens = family.toLowerCase.split(" ")

        // Start with the simples index key possible using family name + baseModelName,
        val result = Seq(normalized(familyTokens) + baseModelName)

        if (familyTokens.size > 1) {
          //If the family name kas multiple components, like Digital IXUS, drop the first component
          //and generate anoter alternative index key. Ex:
          //  Model base name: 130is
          //  Family name: digital ixus
          //  Index keys: Seq(digitalixus130is, ixus130is)
          result :+ (normalized(familyTokens.drop(1)) + baseModelName)
        } else {
          result
        }

      case None => Seq()
    }
  }

  /** Generate alternate indexing keys from the product name
    * Most of the time the product name is manufatcturer + family + model or manufatcturer + model, but some times it
    * contains information that is not present in any other field. For example:
    * {{{
    *   {
    *     "product_name":"Casio-EX-ZR100",
    *     "manufacturer":"Casio",
    *     "model":"ZR100",
    *     "family":"Exilim",
    *     "announced-date":"2011-02-14T19:00:00.000-05:00"}
    * }}}
    *
    * In the example above, by using the product name to generate index keys we can create a new index key like exzr100
    *
    * @param product The product to generate index keys for
    * @return A Seq with and index key generated from the product name
    */
  def alternateIndexingKeysBaseOnProductName(product: Product): Seq[String] = {
    val productNameTokens = product.name.toLowerCase.split("[_-]")
    val familyNameTokens = product.family.getOrElse("").toLowerCase.split("[ _-]")
    val tokensToUseInLookupKey = productNameTokens.filter(t => t != product.manufacturer && !familyNameTokens.contains(t))
    Seq(normalized(tokensToUseInLookupKey))
  }

  /** Generate alternate indexing keys from the product model name by dropping some prefixes and suffixes that are
    * sometimes dropped from the product model in the listings
    *
    * @param product The product to generate index keys for
    * @return Seq with index keys generated
    */
  def alternateIndexingKeysDroppingPrefixesOrSuffixes(product: Product): Seq[String] = {
    product.modelRaw.toLowerCase match {
      case MODEL_WITH_IGNORABLE_PREFIX_AND_SUFFIX(relevantModelPart) => alternateIndexingKeysWithFamilyName(product.family, relevantModelPart) :+ normalized(relevantModelPart)
      case MODEL_WITH_IGNORABLE_SUFFIX(relevantModelPart) => alternateIndexingKeysWithFamilyName(product.family, relevantModelPart) :+ normalized(relevantModelPart)
      case MODEL_WITH_IGNORABLE_PREFIX(relevantModelPart) => alternateIndexingKeysWithFamilyName(product.family, relevantModelPart) :+ normalized(relevantModelPart)
      case _ => Seq()
    }
  }

  /////////////////////////////////////////////////////////////////////////////////////////////////
  // Methods used to generate keys to match from listing titles
  /////////////////////////////////////////////////////////////////////////////////////////////////

  /** Given a listing title, generate keys to use for product lookup in the product database
    *
    * @param relevantTitleSection The part of the title that is relevant for searching
    * @return Keys to use for product lookup
    */
  def generateKeysToMatch(relevantTitleSection: String): Array[String] = {

    //The base input we use to generate the search keys are tokens from the relevant section of the title
    val tokens = relevantTitleSection.split("[ ,./\\+]")

    //We will consider individual tokens and also sequences of 2 and 3 contiguous tokens
    //3 tokens adds 25 more matches and 1s of runtime
    //4 tokens adds only 1 more match and another sec of runtime
    //5 tokens didn't add any new matches, but increased the runtime in another sec, so I stopped at 3
    val baseKeysToMatch = (tokens.map(normalized) ++ consecutiveTokens(tokens, 2) ++ consecutiveTokens(tokens, 3)).distinct

    //Based on the base keys we will use for matching, we generate some variations. eg: if a token ends with a color
    // code like K (black) or T (chocolat) generate and alternative search key dropping this letter
    val keysToMatchVariations = generateKeyVariations(baseKeysToMatch)

    //the keys to match are the base keys plus the variants
    baseKeysToMatch ++ keysToMatchVariations
  }

  /** Returns a list of strings generated by concatenating n tokens and normalizing them
    *
    * @param tokens All title tokens
    * @param numberOfConsecutiveTokens number of consecutive tokens to concatenate
    * @return List of strings composed of n consecutive tokens
    */
  def consecutiveTokens(tokens: Array[String], numberOfConsecutiveTokens: Int): List[String] = {
    for {
      i <- List.range(0, tokens.length - numberOfConsecutiveTokens)
      selected = tokens.slice(i, i + numberOfConsecutiveTokens)
    } yield normalized(selected)
  }

  /** Given a lookup key, generate some variations, by dropping some prefixes and suffixes used to indicate
    * model variations for different colors, product origins, etc. Check the comments in the regex used for more info
    *
    * @param baseKeysToMatch base key to use for generating variations
    * @return key variations
    */
  def generateKeyVariations(baseKeysToMatch: Array[String]) = {
    baseKeysToMatch.flatMap {
      case KEY_IN_LISTING_WITH_MODEL_COUNTRY_AND_COLOR(baseModel) => Some(baseModel)
      case KEY_IN_LISTING_WITH_MODEL_SUFFIX(baseModel) => Some(baseModel)
      case KEY_IN_LISTING_WITH_MODEL_COLOR(baseModel) => Some(baseModel)
      case KEY_IN_LISTING_WITH_MODEL_PREFIX(baseModel) => Some(baseModel)
      case k: String => None
    }
  }

}

/**
  * Trait for classes with raw manufacturer and normalized manufacturer properties (lower case, single word).
  */
trait HasManufacturer {

  def manufacturerRaw: String

  val manufacturer = {
    val manParts = manufacturerRaw.split("\\W+")
    if (manParts.isEmpty) {
      ""
    } else {
      manParts(0).toLowerCase
    }
  }
}

/**
  * Reusable normalization functions
  */
object Normalization {

  def normalized(rawString: String): String = rawString.toLowerCase.split("\\W+").mkString

  def normalized(rawStrings: Array[String], sep: String = ""): String = rawStrings.map(s => normalized(s)).mkString(sep)

}

/**
  * A product to which listings will be matched agains
  *
  * @param modelRaw        model name, before normalization
  * @param announcedDate   date when the product was announced
  * @param name            product name is usually formed by: Manufacturer Short Name + "_" + Optional Family Name + Model Name,
  *                        but sometime each of these components may be slightly different
  * @param family          The product family name, eg. Cyber-Short
  * @param manufacturerRaw The raw manufacturer name, before normalization
  */
case class Product(modelRaw: String, announcedDate: Date, name: String, family: Option[String], manufacturerRaw: String) extends HasManufacturer {
  def isAllDigits(str: String) = str forall Character.isDigit

  val normalizedModel = {
    //A model with only digits may is prone to missclassification
    if (isAllDigits(modelRaw)) {
      //if we have a family, the we'll use family+model as model, else we'll use manufacturer + model
      normalized(Array(family.getOrElse(manufacturer).toLowerCase, modelRaw))
    } else {
      normalized(modelRaw)
    }
  }
}

/**
  * A listing to be matched against products
  *
  * @param currency        The currency used in the price (e.g.: USD, EUR, CAD)
  * @param price           The price of the product
  * @param manufacturerRaw The raw manufacturer name, before normalization (E.g. Canon Canada)
  * @param title           The listing title. E.g: Canon PowerShot A495 10.1 MP Digital Camera with 3.3x Optical Zoom and 2.5-Inch LCD (Blue)
  */
case class Listing(currency: String, price: BigDecimal, manufacturerRaw: String, title: String) extends HasManufacturer {
  lazy val alternativeManufacturer = normalized(title.split("\\W+").headOption.getOrElse(""))
}

/** The application entry point
  */
object SortableChallenge {

  def main(args: Array[String]): Unit = {

    val products = loadProducts

    val listings = loadListings

    val matcher = new HeuristicsProductMatcher(products)

    val startTime = LocalDateTime.now()

    val results = listings.map{
      listingToMap =>
          (listingToMap, matcher.findMatch(listingToMap))
    }

    val matched = results.flatMap{
      case (listing, Some(product)) => Some((product, listing))
      case _ => None
    }

    val unmatched = results.flatMap{
      case (listing, None) => Some(listing)
      case _ => None
    }

    val endTime = LocalDateTime.now()

    println(
      s"""
         |Matching took ${Duration.between(startTime, endTime)}
         |Matched: ${matched.size}
         |Not Matched: ${unmatched.size}
         |Total: ${listings.size}
    """.stripMargin
    )

    println("saving matches to results.json\n")
    saveResultsToFile(matched)

    println("saving unmatched listings to unmatched.json\n")
    saveUnmatchedProductToFile(unmatched)
  }

  def loadProducts: List[Product] = {
    scala.io.Source.fromFile("products.txt", "utf-8").getLines().map(Json.parse(_).as[Product]).toList
  }

  def loadListings: List[Listing] = {
    scala.io.Source.fromFile("listings.txt", "utf-8").getLines().map(Json.parse(_).as[Listing]).toList
  }

  /** Saves unmatched listings to a file named unmatched.json. Useful for looking for patterns in unmatched products
    * to improve the heuristics
    *
    * @param unmatched unmatched listings
    */
  def saveUnmatchedProductToFile(unmatched: List[Listing]) = {
    val unmatchedFile = new File("unmatched.txt")
    unmatchedFile.write(
      unmatched.map(Json.toJson(_).toString)
    )
  }

  /** Save the match results to the file results.txt
    *
    * results.txt will be a file full of Result objects, that simply associates a Product with a list of matching
    * Listing objects.
    * {"product_name": String, "listings": Array[Listing]}
    *
    * @param matched list of product to listing matches
    */
  def saveResultsToFile(matched: List[(Product, Listing)]) = {

    val resultsFile = new File("results.txt")

    // Group matches by model name
    val results: Map[String, List[Listing]] = matched.groupBy(_._1.modelRaw).mapValues(_.map(_._2))

    resultsFile.write(
      results.map{
        productAndListings => Json.toJson(productAndListings).toString()
      }
    )
  }

}
