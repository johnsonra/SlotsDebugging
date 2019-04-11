#' @title Print method for Slots
print.slots <- function(x, ...)
{
}


#' @title Plot method for Slots
#' 
#' @param x An object used to select a method.
#' @param ... Further arguments passed to or from other methods.
#' 
#' @rdname plot_methods
#' @method plot slots
#' 
#' @export
plot.slots <- function(x, ...)
{
    args <- list(...)
    
    # defaults for this plot (allow these to be overruled by the user)
    
    if(is.null(args$xlim))
        args$xlim <- c(0, 20)
    
    if(is.null(args$ylab))
        args$ylab <- 'Frequency'
    
    if(is.null(args$xlab))
        args$xlab <- 'Winnings from a single spin'
    
    if(is.null(args$main))
        args$main <- 'Summary of slot object'
    
    if(is.null(args$type))
       args$type <- 'h'
    
    if(is.null(args$bty))
        args$bty <- 'l'
    
    args$x <- table(tmp$score)
    
    do.call(plot, args)
}