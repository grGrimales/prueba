package yoppworks.hackerchallenges.bundlepricing.usecases

import yoppworks.hackerchallenges.bundlepricing.domain.BundlePricingDomain.{Cart, CatalogItem}

object IsCartValid {


  /** Check if the cart is valid, i.e. all items in the cart are in the catalog
   *
   * @param cart the cart to check
   * @param catalog the catalog to check against
   * @return
   * true if all items in the cart are in the catalog
   * false otherwise
   */
  def isCartValid(cart: Cart, catalog: Seq[CatalogItem] ): Boolean = {
    cart.cartItems.forall(cartItem =>
      catalog.contains(cartItem.catalogItem)
    )
  }

}
