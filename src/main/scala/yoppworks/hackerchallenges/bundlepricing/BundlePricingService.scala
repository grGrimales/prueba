package yoppworks.hackerchallenges.bundlepricing

import yoppworks.hackerchallenges.bundlepricing.BundlePricingDomain.{Cart, CartItem, CatalogItem, InvalidCartException, Price, Quantity, BundlePromotion}
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
    if (!isCartValid(cart)) throw InvalidCartException

    val applicableBundles = findApplicableBundles(cart)

    calculatePriceWithBundles(cart, applicableBundles)
  }


  /** Check if the cart is valid, i.e. all items in the cart are in the catalog
   *
   * @param cart the cart to check
   * @return
   * true if all items in the cart are in the catalog
   * false otherwise
   */
  def isCartValid(cart: Cart): Boolean = {
    cart.cartItems.forall(cartItem =>
      catalog.contains(cartItem.catalogItem)
    )
  }


  /**
   * Find all applicable bundles for a given cart
   *
   * @param cart the cart to check
   * @return
   * a list of all applicable bundles
   */
  def findApplicableBundles(cart: Cart): Seq[BundlePromotion] = {
    bundlePromotions.filter { bundle =>
      bundle.cartItems.forall { bundleItem =>
        cart.cartItems.exists { cartItem =>
          cartItem.catalogItem == bundleItem.catalogItem &&
            cartItem.quantity.value >= bundleItem.quantity.value
        }
      }
    }
  }

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
