package hackerchallenges.bundlepricing.usecases

import hackerchallenges.bundlepricing.domain.BundlePricingDomain.{BundlePromotion, Cart, CartItem, Quantity}

object ApplyBundle {

  /**
   * Apply a bundle to a cart
   *
   * @param cart   the cart to apply the bundle to
   * @param bundle the bundle to apply
   * @return the cart with the bundle applied
   */
  def applyBundle(cart: Cart, bundle: BundlePromotion): Cart = {
    val updatedCartItems = cart.cartItems.flatMap { cartItem =>
      bundle.cartItems.find(_.catalogItem == cartItem.catalogItem) match {
        case Some(bundleItem) =>
          val remainingQuantity = cartItem.quantity.value - bundleItem.quantity.value
          if (remainingQuantity > 0) {
            Some(CartItem(cartItem.catalogItem, Quantity(remainingQuantity)))
          } else None
        case None =>
          Some(cartItem)
      }
    }
    Cart(updatedCartItems)
  }

}
