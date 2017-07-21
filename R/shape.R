#' @export
rect <- function(sketch, x = NULL, y = NULL, w = NULL, h = NULL){
  base_prototype(sketch, "rect", shape_factory, x = x, y = y, w = w, h = h)
}

#' @export
line <- function(sketch, x1 = NULL, y1 = NULL, x2 = NULL, y2 = NULL){
  base_prototype(sketch, "line", shape_factory, x1 = x1, y1 = y1, x2 = x2, y2 = y2)
}

#' @export
arc <- function(sketch, a = NULL, b = NULL, c = NULL, d = NULL, start = NULL, stop = NULL, mode = NULL){
  base_prototype(sketch, "arc", shape_factory,
                 a = a, b = b, c = c, d = d, start = start, stop = stop, mode = mode)
}

#' @export
ellipse <- function(sketch, x = NULL, y = NULL, w = NULL, h = NULL){
  base_prototype(sketch, "ellipse", shape_factory, x = x, y = y, w = w, h = h)
}

# Utilities

shape_factory <- function(func, ...){
  args_ <- list(...)
  x <- function(...){
    sprintf(paste0("p.", func, "(",
                   paste(rep("%s", length(args_)), collapse = ","),
                   ");"), ...)
  }
  function(l){
    do.call(x, l)
  }
}

# @param sketch A p5 object.
# @param func A string of a function name.
# @param factory A function which returns a function that creates the p5 string.
# @param ... Named arguments for the function.
#' @importFrom purrr reduce
base_prototype <- function(sketch, func, factory, ...){
  section <- get_section(sketch, func)

  args_ <- prepare_args(sketch, ...) %>%
    map(factory(func, ...)) %>%
    reduce(JS_)

  sketch$x[[section]] <- sketch$x[[section]] %>%
    js_append(args_)
  sketch
}
