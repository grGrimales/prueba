package hackerchallenges.bundlepricing.usecases

import hackerchallenges.bundlepricing.domain.BundlePricingDomain.{BundlePromotion, Cart}

object FindApplicableBundles {


  /**
   * Find applicable bundles for the cart
   *
   * @param cart the cart to check
   * @param bundlePromotions the list of bundle promotions to check
   * @return
   * the list of applicable bundles
   */
  def findApplicableBundles(cart: Cart,bundlePromotions: Seq[BundlePromotion]): Seq[BundlePromotion] = {
    bundlePromotions.filter { bundle =>
      bundle.cartItems.forall { bundleItem =>
        cart.cartItems.exists { cartItem =>
          cartItem.catalogItem == bundleItem.catalogItem &&
            cartItem.quantity.value >= bundleItem.quantity.value
        }
      }
    }
  }

}
