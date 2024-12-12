package yoppworks.hackerchallenges.bundlepricing.usecases

import yoppworks.hackerchallenges.bundlepricing.domain.BundlePricingDomain.{BundlePromotion, Cart, Price}
import yoppworks.hackerchallenges.bundlepricing.usecases.ApplyBundle.applyBundle
import yoppworks.hackerchallenges.bundlepricing.usecases.CalculatePriceWithoutBundles.calculatePriceWithoutBundles

object CalculatePriceWithBundles {

  /**
   * Calculate the price of a cart with all applicable bundles
   *
   * @param cart    the cart to calculate the price for
   * @param bundles the list of applicable bundles
   * @return the price of the cart with all applicable bundles
   */
  def calculatePriceWithBundles(cart: Cart, bundles: Seq[BundlePromotion]): Price = {
    if (cart.cartItems.isEmpty) {
      Price(0)
    } else {
      bundles.flatMap { bundle =>
        if (bundle.cartItems.forall(item =>
          cart.cartItems.exists(cartItem =>
            cartItem.catalogItem == item.catalogItem &&
              cartItem.quantity.value >= item.quantity.value
          )
        )) {
          val newCart = applyBundle(cart, bundle)
          Some(Price(bundle.totalDiscountedPrice.value + calculatePriceWithBundles(newCart, bundles).value))
        } else {
          None
        }
      }.minByOption(_.value).getOrElse(calculatePriceWithoutBundles(cart))
    }
  }

}
