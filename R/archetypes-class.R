

#' Archetypes object constructor and methods.
#' @param archetypes The archetypes; a \eqn{p \times m} matrix, see
#'   \code{\link{atypes}}.
#' @param k The number of archetypes;
#' @param alphas The coefficients; a \eqn{n \times p} matrix, see
#'   \code{\link{alphas}}.
#' @param rss The residual sum of squares; see \link{rss}.
#' @param iters The number of iterations to the convergence.
#' @param call The call of the \code{\link{archetypes}} function.
#' @param history If \code{saveHistory} set then an environment with the
#'   archetypes object for each execution step;
#' @param kappas The kappas for each system of linear equations.
#' @param betas The data coefficients; a \eqn{p \times n} matrix.
#' @param zas The temporary archetypes.
#' @param family The archetypes family.
#' @param familyArgs Additional arguments for family blocks.
#' @param residuals The residuals.
#' @param weights The data weights.
#' @param reweights The data reweights.
#' @return A list with an element for each parameter and class attribute
#'   \code{archetypes}.
#' @seealso \code{\link{archetypes}}
#' @rdname archetypes-class
#' @aliases archetypes-class
as.archetypes <- function(archetypes, k, alphas, rss, iters = NULL, call = NULL,
                          history = NULL, kappas = NULL, betas = NULL, zas = NULL,
                          family = NULL, familyArgs = NULL, residuals = NULL,
                          weights = NULL, reweights = NULL) {

  return(structure(list(archetypes = archetypes,
                        k = k,
                        alphas = alphas,
                        rss = rss,
                        iters = iters,
                        kappas = kappas,
                        betas = betas,
                        zas = zas,
                        call = call,
                        history = history,
                        family = family,
                        familyArgs = familyArgs,
                        residuals = residuals,
                        weights = weights,
                        reweights = reweights),
                   class = c(family$class, 'archetypes')))
}



setOldClass('archetypes')



#' Print method for archetypes object.
#' @param x An \code{archetypes} object.
#' @param full Full information or just convergence and rss information.
#' @param ... Ignored.
#' @return Undefined.
#' @method print archetypes
#' @S3method print archetypes
#' @nord
print.archetypes <- function(x, full = TRUE, ...) {
  if ( full ) {
    cat('Archetypes object\n\n')
    cat(paste(deparse(x$call), collapse = '\n'), '\n\n')
  }

  cat('Convergence after', x$iters, 'iterations\n')
  cat('with RSS = ', rss(x), '.\n', sep = '')
}



#' Return fitted data, i.e. archetypes data approximation.
#' @param object An \code{archetypes}-related object.
#' @param ... Ignored.
#' @return Matrix with approximated data.
#' @method fitted archetypes
#' @importFrom stats fitted
#' @S3method fitted archetypes
#' @rdname archetypes-class
fitted.archetypes <- function(object, ...) {
  t(t(object$archetypes) %*% t(object$alphas))
}



#' Return fitted archetypes.
#' @param object An \code{archetypes} object.
#' @param ... Ignored.
#' @return Matrix with \eqn{k} archetypes.
#' @nord
.parameters.archetypes <- function(object, ...) {
  object$archetypes
}

#' Return fitted archetypes.
#' @param object An \code{archetypes} object.
#' @param ... Ignored.
#' @return Matrix with \eqn{k} archetypes.
#' @importFrom modeltools parameters
#' @rdname archetypes-class
setMethod('parameters',
          signature = signature(object = 'archetypes'),
          .parameters.archetypes)



#' Return coefficients.
#' @param object An \code{archetypes} object.
#' @param type Return alphas or betas.
#' @param ... Ignored.
#' @return Coefficient matrix.
#' @method coef archetypes
#' @importFrom stats coef
#' @S3method coef archetypes
#' @rdname archetypes-class
coef.archetypes <- function(object, type = c('alphas', 'betas'), ...) {
  type <- match.arg(type)
  object[[type]]
}


#' Return residuals.
#' @param object An \code{archetypes} object.
#' @param ... Ignored.
#' @return Matrix with residuals.
#' @method residuals archetypes
#' @importFrom stats residuals
#' @S3method residuals archetypes
#' @rdname archetypes-class
residuals.archetypes <- function(object, ...) {
  object$residuals
}



#' Residual sum of squares.
#' @param object An object.
#' @param ... Ignored.
#' @return Residual sum of squares.
#' @export
#' @rdname archetypes-generics
rss <- function(object, ...) {
  UseMethod('rss')
}

#' Residual sum of squares getter.
#' @param object An \code{archetypes} object.
#' @param type Return scaled, single or global RSS.
#' @param ... Ignored.
#' @return Residual sum of squares.
#' @method rss archetypes
#' @S3method rss archetypes
#' @rdname archetypes-class
rss.archetypes <- function(object, type = c('scaled', 'single', 'global'), ...) {
  type <- match.arg(type)
  resid <- residuals(object)

  switch(type,
         scaled = object$rss,
         single = apply(resid, 1, object$family$normfn),
         global = object$family$normfn(resid) / nrow(resid))
}



#' Return weights.
#' @param object An \code{archetypes} object.
#' @param type Return global weights (weighted archetypes) or
#'   weights calculated during the iterations (robust archetypes).
#' @param ... Ignored.
#' @return Vector of weights.
#' @method weights archetypes
#' @importFrom stats weights
#' @S3method weights archetypes
#' @rdname archetypes-class
weights.archetypes <- function(object, type = c('weights', 'reweights'), ...) {
  type <- match.arg(type)
  object[[type]]
}



#' Kappa getter.
#' @param z An \code{archetypes} object.
#' @param ... Ignored.
#' @return A vector of kappas.
#' @method kappa archetypes
#' @S3method kappa archetypes
#' @rdname archetypes-class
kappa.archetypes <- function(z, ...) {
  return(z$kappas)
}



#' Predict coefficients or data based on archetypes.
#' @param object An \code{archetypes} object.
#' @param type Predict alphas or data.
#' @param ... Ignored.
#' @return Prediction.
#' @method predict archetypes
#' @S3method predict archetypes
#' @nord
predict.archetypes <- function(object, newdata = NULL,
                               type = c('alphas', 'data'), ...) {
  type <- match.arg(type)

  if ( is.null(newdata) )
    return(switch(type,
                  alphas = coef(object, type = 'alphas'),
                  data = fitted(object)))

  stop('Not implemented yet.')

  ### Something like the following ...
  #if ( type == 'alphas' )
  #  object$family$alphasfn(NULL, t(object$archetypes), t(newdata))
}



#' Number of parameters.
#' @param object An object.
#' @param ... Further arguments.
#' @return Number of parameters.
#' @export
#' @rdname archetypes-generics
nparameters <- function(object, ...) {
  UseMethod('nparameters')
}



#' Number of archetypes
#' @param object An \code{archetypes}-related object.
#' @param ... Further arguments.
#' @return Number of archetypes.
#' @method nparameters archetypes
#' @S3method nparameters archetypes
#' @rdname archetypes-class
nparameters.archetypes <- function(object, ...) {
  return(object$k)
}
