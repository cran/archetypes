
#' Archetypal maps
#'
#' Two-dimensional projection of the observations based on the alpha
#' coefficients into a space spanned by the (projected) archetypes.
#'
#' @param object An \code{\link{archetypes}} object
#' @param projection Projection function; see
#'   \code{\link{archmap_projections}}
#' @param rotate Rotation angle to rotate the projection
#' @param cex Character expansion of archetypes
#' @param col Color of observations
#' @param pch Point character of observations
#' @param xlab A label for the x-axis
#' @param ylab A label for the y-axis
#' @param axes Logical value to draw axes or not
#' @param asp The y/x aspect ratio
#' @param ... Arguments passed to the underlying plot function
#'
#' @return
#'   Invisible matrix with the projected archetypes
#'
#' @examples
#'   data("skel", package = "archetypes")
#'   skel2 <- subset(skel, select = -Gender)
#'
#'   set.seed(1981)
#'   a <- archetypes(skel2, k = 5)
#'
#'   ## Simplex projection:
#'   archmap(a, col = skel$Gender)
#'
#'   ## MDS projection:
#'   archmap(a, projection = atypes_projection, col = skel$Gender)
#'
#' @family archmap
#'
#' @export
archmap <- function(object, projection = simplex_projection, rotate = 0,
                    cex = 1.5, col = 1, pch = 1, xlab = "", ylab = "",
                    axes = FALSE, asp = TRUE, ...) {

    stopifnot("archetypes" %in% class(object))
    stopifnot(is.function(projection))

    k <- object$k
    if( k < 3) {
      stop("Need at least 3 archetypes.\n")
    }

    ## Projection:
    cmds <- projection(parameters(object))


    ## Rotation:
    if ( rotate != 0 ){
      a <- pi*rotate/180
      A <- matrix(c(cos(a), -sin(a), sin(a),
                    cos(a)), ncol=2)
      cmds <- cmds %*% A
    }

    ## Active archetypes:
    hmds <- chull(cmds)
    active <- 1:k %in% hmds

    ## Plot region spanned by the projected archetypes:
    plot(cmds, type = "n", xlab = xlab, ylab = ylab, axes = axes, asp = asp, ...)
    points(coef(object) %*% cmds, col = col, pch = pch)

    rad <- ceiling(log10(k)) + 1.5
    polygon(cmds[hmds,])
    points(cmds[active,], pch=21, cex=rad*cex, bg="grey")
    text(cmds[active,], labels=(1:k)[active], cex=cex)
    if(any(!active)){
      points(cmds[!active,,drop=FALSE], pch=21, cex=rad*cex,
             bg="white", fg="grey")
      text(cmds[!active,,drop=FALSE], labels=(1:k)[!active],
           cex=cex, col="grey20")
    }

    invisible(cmds)
}



#' Archetypal map projections
#'
#' @param x Archetypes matrix
#' @param r Radius of the simplex projection
#'
#' @return
#'   Matrix with the projected archetypes
#'
#' @family archmap
#'
#' @aliases archmap_projections
#' @rdname archmap_projections
#' @export
simplex_projection <- function(x, r = 10) {
  n <- nrow(x)

  phi <- seq(-pi, pi, length.out = n + 1)

  x <- r * cos(phi)
  y <- r * sin(phi)

  m <- cbind(x, y)

  m[-1, ]
}



#' @rdname archmap_projections
#' @export
atypes_projection <- function(x) {
  cmdscale(dist(x))
}
