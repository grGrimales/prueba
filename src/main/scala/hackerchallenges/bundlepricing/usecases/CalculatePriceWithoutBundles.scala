package hackerchallenges.bundlepricing.usecases

import hackerchallenges.bundlepricing.domain.BundlePricingDomain.{Cart, Price}

object CalculatePriceWithoutBundles {

  /**
   * Calculate the price of a cart without any bundles
   *
   * @param cart the cart to calculate the price for
   * @return the price of the cart without any bundles
   */
  def calculatePriceWithoutBundles(cart: Cart): Price = {
    Price(cart.cartItems.map { cartItem =>
      cartItem.catalogItem.unitPrice.value * cartItem.quantity.value
    }.sum)
  }

}
