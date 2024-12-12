package hackerchallenges.bundlepricing.services

import hackerchallenges.bundlepricing.domain.BundlePricingDomain._
import hackerchallenges.bundlepricing.usecases.CalculatePriceWithBundles.calculatePriceWithBundles
import hackerchallenges.bundlepricing.usecases.FindApplicableBundles.findApplicableBundles
import hackerchallenges.bundlepricing.usecases.IsCartValid.isCartValid

import scala.util.Try


/**
 * Specs for the BundlePricingService, which take as parameter a catalog and the current promotions
 * and then can bundle a cart to optimize the price
 */
class BundlePricingService(catalog: Seq[CatalogItem], bundlePromotions: Seq[BundlePromotion]) {


  /**
   * Group cart item to bundles to get the lowest possible cart price
   *
   * @return
   * Success: cart price in cents, example Price(2250) => $22.50
   * Failure: InvalidCartException if the cart isn't valid (contains an item which doesn't exist in catalog)
   */
  def bundleCartToLowestPrice(cart: Cart): Try[Price] = Try {
    if (!isCartValid(cart,catalog)) throw InvalidCartException

    val applicableBundles = findApplicableBundles(cart,bundlePromotions)

    calculatePriceWithBundles(cart, applicableBundles)
  }

}