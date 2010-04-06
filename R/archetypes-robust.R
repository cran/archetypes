#' @include archetypes-kit-blocks.R
{}



#' Robust archetypes.
#' @param data A numeric \eqn{n \times m} data matrix.
#' @param k The number of archetypes.
#' @param familyBlocks Exchange predefined family blocks.
#' @param ... Arguments available for \code{\link{archetypes}}.
#' @return An object of class \code{robustArchetypes} and
#'   \code{\link{archetypes-class}}.
#' @export
#' @rdname archetypes
robustArchetypes <- function(data, k, familyBlocks = list(), ...) {

  family <- do.call(archetypesFamily, c(list('robust'), familyBlocks))

  archetypes(data, k, family = family, ...)
}



#' Robust family constructor helper.
#' @return A list of blocks.
#' @nord
.robust.archetypesFamily <- function() {
  f <- .original.archetypesFamily()
  f$class <- 'robustArchetypes'
  f$weightfn <- center.weightfn
  f$reweightsfn <- bisquare0.reweightsfn
  f
}



setOldClass('robustArchetypes')



#' Return fitted archetypes.
#' @param object An \code{robustArchetypes} object.
#' @param ... Ignored.
#' @return Matrix with \eqn{k} archetypes.
#' @importFrom modeltools parameters
#' @nord
setMethod('parameters',
          signature = signature(object = 'robustArchetypes'),
          .parameters.archetypes)

