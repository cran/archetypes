#' @include archetypes-kit-blocks.R
{}



#' Weighted archetypes.
#' @param data A numeric \eqn{n \times m} data matrix.
#' @param k The number of archetypes.
#' @param weights Data weights matrix.
#' @param familyBlocks Exchange predefined family blocks.
#' @param ... Arguments available for \code{\link{archetypes}}.
#' @return An object of class \code{weightedArchetypes} and
#'   \code{\link{archetypes-class}}.
#' @export
#' @rdname archetypes
weightedArchetypes <- function(data, k, weights = NULL,
                               familyBlocks = list(), ...) {

  family <- do.call(archetypesFamily, c(list('weighted'), familyBlocks))

  archetypes(data, k, weights = weights, family = family, ...)
}



#' Weighted family constructor helper.
#' @return A list of blocks.
#' @nord
.weighted.archetypesFamily <- function() {
  f <- .original.archetypesFamily()
  f$class <- 'weightedArchetypes'
  f$globweightfn <- center.globweightfn
  f
}



setOldClass('weightedArchetypes')



#' Return fitted archetypes.
#' @param object An \code{weightedArchetypes} object.
#' @param ... Ignored.
#' @return Matrix with \eqn{k} archetypes.
#' @importFrom modeltools parameters
#' @nord
setMethod('parameters',
          signature = signature(object = 'weightedArchetypes'),
          .parameters.archetypes)
