#' @export
arc <- function(a, b, c, d, start, stop){
  sketch$x$draw <- sketch$x$draw %>%
    js_append(sprintf("p.arc(%d,%d,%d,%d);", r, g, b, ifelse(is.null(a), 100, a)))
  sketch
}
