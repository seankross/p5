# Creating & Reading


# Setting

#' @export
background <- function(sketch, r, g, b, a = NULL){
  sketch$x$setup <- sketch$x$setup %>%
    js_append(sprintf("p.background(%d,%d,%d,%d);", r, g, b, ifelse(is.null(a), 100, a)))
  sketch
}
