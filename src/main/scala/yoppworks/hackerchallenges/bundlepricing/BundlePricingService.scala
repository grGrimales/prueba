package yoppworks.hackerchallenges.bundlepricing

import yoppworks.hackerchallenges.bundlepricing.BundlePricingDomain._
import scala.util.Try

/**
  * Specs for the BundlePricingService, which take as parameter a catalog and the current promotions
  * and then can bundle a cart to optimize the price
  */
class BundlePricingService(catalog: Seq[CatalogItem], bundlePromotions: Seq[BundlePromotion]) {

  /**
    * Group cart item to bundles to get the lowest possible cart price
    * @return
    *   Success: cart price in cents, example Price(2250) => $22.50
    *   Failure: InvalidCartException if the cart isn't valid (contains an item which doesn't exist in catalog)
    */
  def bundleCartToLowestPrice(cart: Cart): Try[Price] = Try {
    if (!isCartValid(cart)) throw InvalidCartException

    val applicableBundles = findApplicableBundles(cart)

    // Calcula el precio más bajo utilizando las combinaciones posibles de bundles
    calculatePriceWithBundles(cart, applicableBundles)
  }



  def isCartValid(cart: Cart): Boolean = {
    cart.cartItems.forall( cartItem =>
      catalog.contains(cartItem.catalogItem)
    )
  }

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

  def calculatePriceWithBundles(cart: Cart, bundles: Seq[BundlePromotion]): Price = {
    if (cart.cartItems.isEmpty) {
      Price(0) // Si el carrito está vacío, el precio es 0
    } else {
      // Generar todas las combinaciones posibles de bundles
      bundles.flatMap { bundle =>
        if (bundle.cartItems.forall(item =>
          cart.cartItems.exists(cartItem =>
            cartItem.catalogItem == item.catalogItem &&
              cartItem.quantity.value >= item.quantity.value
          )
        )) {
          // Aplica el bundle y calcula el precio restante
          val newCart = applyBundle(cart, bundle)
          Some(Price(bundle.totalDiscountedPrice.value + calculatePriceWithBundles(newCart, bundles).value))
        } else {
          None // El bundle no aplica
        }
      }.minByOption(_.value).getOrElse(calculatePriceWithoutBundles(cart))
    }
  }
  def calculatePriceWithoutBundles(cart: Cart): Price = {
    Price(cart.cartItems.map { cartItem =>
      cartItem.catalogItem.unitPrice.value * cartItem.quantity.value
    }.sum)
  }


  def applyBundle(cart: Cart, bundle: BundlePromotion): Cart = {
    // Ajusta las cantidades de los productos en el carrito según el bundle aplicado
    val updatedCartItems = cart.cartItems.flatMap { cartItem =>
      bundle.cartItems.find(_.catalogItem == cartItem.catalogItem) match {
        case Some(bundleItem) =>
          // Resta la cantidad del bundle del ítem en el carrito
          val remainingQuantity = cartItem.quantity.value - bundleItem.quantity.value
          if (remainingQuantity > 0) {
            Some(CartItem(cartItem.catalogItem, Quantity(remainingQuantity)))
          } else None // Si no queda cantidad, elimina el producto del carrito
        case None =>
          // Producto no usado en el bundle, mantenerlo
          Some(cartItem)
      }
    }
    Cart(updatedCartItems)
  }




}
