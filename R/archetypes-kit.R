#' @include archetypes-kit-blocks.R
#' @include archetypes-class.R
{}



#' Perform archetypal analysis on a data matrix.
#' @param data A numeric \eqn{n \times m} data matrix.
#' @param k The number of archetypes.
#' @param maxIterations The maximum number of iterations.
#' @param minImprovement The minimal value of improvement between two
#'   iterations.
#' @param maxKappa The limit of kappa to report an ill-ness warning.
#' @param verbose Print some details during execution.
#' @param saveHistory Save each execution step in an environment for
#'   further analyses.
#' @param family Blocks defining the underlying problem solving mechanisms;
#'   see \code{\link{archetypesFamily}}.
#' @return An object of class \code{\link{archetypes}}, see
#'   \code{\link{as.archetypes}}.
#' @seealso \code{\link{stepArchetypes}}
#' @references Cutler and Breiman. Archetypal Analysis. Technometrics,
#'   36(4), 1994. 338-348.
#' @examples
#'   data(toy)
#'   a <- archetypes(toy, 3)
#' @export
#' @note Please see the vignette for a detailed explanation!
archetypes <- function(data, k, maxIterations=100,
                       minImprovement=sqrt(.Machine$double.eps),
                       maxKappa=1000, verbose=TRUE, saveHistory=TRUE,
                       family=archetypesFamily('default')) {
  
  ### Helpers:
  mycall <- match.call()
  
  history <- NULL
  snapshot <- function(name) {
    history[[paste('s', name, sep='')]] <-
      list(archetypes=as.archetypes(t(family$rescalefn(x,
             family$undummyfn(x, zs))), k, alphas=t(alphas),
             betas=t(betas), zas=t(family$rescalefn(x,
             family$undummyfn(x, zas))), rss=rss,
             kappas=kappas))
  }


  ### Data preparation:
  x <- t(data)
  x <- family$scalefn(x)
  x <- family$dummyfn(x)

  n <- ncol(x)
  m <- nrow(x)


  ### Initialization:
  init <- family$initfn(x, k)
  
  betas <- init$betas
  alphas <- init$alphas

  zs <- x %*% betas
  rss <- family$normfn(zs %*% alphas - x) / n

  zas <- NULL

  kappas <- c(alphas=kappa(alphas), betas=kappa(betas),
              zas=-Inf, zs=kappa(zs))
  isIll <- c(kappas) > maxKappa
  errormsg <- NULL
  
  if ( saveHistory ) {
    history <- new.env(parent=emptyenv())
    snapshot(0)
  }

  
  ### Main loop:
  i <- 1
  imp <- +Inf

  tryCatch(while ( (i <= maxIterations) & (imp >= minImprovement) ) {
    
    ## Alpha's:
    alphas <- family$alphasfn(alphas, zs, x)
    zas <- family$zalphasfn(alphas, x)
    rss1 <- family$normfn(zas %*% alphas - x) / n

    kappas[c('alphas', 'zas')] <- c(kappa(alphas), kappa(zas))

    
    ## Beta's:
    betas <- family$betasfn(betas, x, zas)
    zs <- x %*% betas

    kappas[c('betas', 'zs')] <- c(kappa(betas), kappa(zs))

    
    ## RSS and improvement:
    rss2 <- family$normfn(zs %*% alphas - x) / n
    
    imp <- rss - rss2
    rss <- rss2

    kappas <- c(alphas=kappa(alphas), betas=kappa(betas),
                zas=kappa(zas), zs=kappa(zs))
    isIll <- isIll & (kappas > maxKappa)
    

    ## Loop Zeugs:
    if ( verbose )
      cat(i, ': rss = ', formatC(rss, 8, format='f'),
          ', improvement = ', formatC(imp, 8, format='f'),
          '\n', sep = '')
    
    if ( saveHistory )
      snapshot(i)
    
    i <- i + 1
  },
  error=function(e) errormsg <<- e)
  

  ### Check illness:
  if ( !is.null(errormsg) ) {
    warning('k=', k, ': ', errormsg)
    return(as.archetypes(NULL, k, NULL, NA, iters=i,
                         call=mycall, history=history, kappas=kappas))
  }

  if ( any(isIll) )
    warning('k=', k, ': ', paste(names(isIll)[isIll], collapse=', '),
            ' > maxKappa', sep='')

  
  ### Rescale archetypes:
  zs <- family$undummyfn(x, zs)
  zs <- family$rescalefn(x, zs)
  zs <- t(zs)

  
  return(as.archetypes(zs, k, t(alphas), rss, iters=(i-1),
                       call=mycall, history=history, kappas=kappas,
                       betas=t(betas)))
}
