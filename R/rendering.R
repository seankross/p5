#' Set the dimensions of the sketch canvas
#'
#' @param sketch A p5 sketch.
#' @param w The width of the canvas in pixels.
#' @param h The height of the canvas in pixels.
#' @param renderer Either the default renderer (`NULL`) or `~WEBGL`.
#' @importFrom rlang is_formula
#' @export
#' @examples
#' \dontrun{
#'
#' p5() %>%
#'   createCanvas(300, 200) %>%
#'   background("#FAE")
#'
#' }
createCanvas <- function(sketch, w, h, renderer = NULL){
  if(is_formula(w)){
    w <- unform(w)
  }

  if(is_formula(h)){
    h <- unform(h)
  }

  if(is_formula(renderer)){
    renderer <- unform(renderer)
  } else if(is.null(renderer)) {
    renderer <- "null"
  }

  sketch$x$setup <- sketch$x$setup %>%
    js_append(sprintf("p.createCanvas(%s,%s,%s);", w, h, renderer))

  if(is.null(sketch$width) && validCssUnit(w)){
    sketch$width <- w
  }

  if(is.null(sketch$height) && validCssUnit(h)){
    sketch$height <- h
  }

  sketch
}
