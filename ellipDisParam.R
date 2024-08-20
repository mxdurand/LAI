# Ellipsiodal distribution (Campbell 1990)
ellipDisParam <- function(ang = NULL, xi = NULL)
{
  if(is.null(ang) & is.null(xi)){stop("Need to define either 'ang' or 'xi'")}
  if(!is.null(ang) & !is.null(xi)){stop("Cannot define both 'ang' and 'xi'. Choose one.")}
  
  if(is.null(ang)){
    # Calculate ang
    ang <- 9.65 * ( 3 + xi) ^ -1.65
    ang <- ang * 180 / pi
    return(ang)
  } else if(is.null(xi)){
    if(ang < 1){message("Warning: 'ang' needs to be defined in degrees.")}
    # Calculate xi
    ang <- ang * pi / 180
    xi <- -3 + (ang / 9.65) ^ -0.6061
    return(xi)
  }
}
