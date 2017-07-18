#' @export
createCanvas <- function(sketch, w, h){
  sketch$x$setup <- sketch$x$setup %>%
    js_append(sprintf("p.createCanvas(%d,%d);", w, h))

  if(is.null(sketch$width)){
    sketch$width <- w
  }

  if(is.null(sketch$height)){
    sketch$height <- h
  }

  sketch
}
