#' @importFrom rlang is_formula
#' @export
createCanvas <- function(sketch, w, h){
  if(is_formula(w)){
    w <- unform(w)
  }

  if(is_formula(h)){
    h <- unform(h)
  }

  sketch$x$setup <- sketch$x$setup %>%
    js_append(sprintf("p.createCanvas(%s,%s);", w, h))

  if(is.null(sketch$width) && validCssUnit(w)){
    sketch$width <- w
  }

  if(is.null(sketch$height) && validCssUnit(h)){
    sketch$height <- h
  }

  sketch
}
