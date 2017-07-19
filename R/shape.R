#' @export
rect <- function(sketch, x = NULL, y = NULL, w = NULL, h = NULL){
  section <- get_section(sketch, "rect")

  args_ <- prepare_args(sketch, x = x, y = y, w = w, h = h) %>%
    map( ~ sprintf("p.rect(%s,%s,%s,%s);", .x$x, .x$y, .x$w, .x$h)) %>%
    reduce(JS_)

  sketch$x[[section]] <- sketch$x[[section]] %>%
    js_append(args_)
  sketch
}

#' @export
line <- function(sketch, x1, y1, x2, y2){
  shape_prototype(sketch, "line", x1 = x1, y1 = y1, x2 = x2, y2 = y2)
  # section <- get_section(sketch, "line")
  #
  # args_ <- prepare_args(sketch, x1 = x1, y1 = y1, x2 = x2, y2 = y2) %>%
  #   map( ~ sprintf("p.line(%s,%s,%s,%s);", .x$x1, .x$y1, .x$x2, .x$y2)) %>%
  #   reduce(JS_)
  #
  # sketch$x[[section]] <- sketch$x[[section]] %>%
  #   js_append(args_)
  # sketch
}

paste_ <- function(...){
  capture.output(cat(..., sep = ","))
}

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

prepare_args(sketch, x1 = x1, x2 = x2, y1 = y1, y2 = y2) %>%
  map(shape_factory(func, x1 = x1, x2 = x2, y1 = y1, y2 = y2))

shape_prototype <- function(sketch, func, ...){
  section <- get_section(sketch, func)

  args_ <- prepare_args(sketch, ...) %>%
    map(shape_factory(func, ...)) %>%
    reduce(JS_)

  sketch$x[[section]] <- sketch$x[[section]] %>%
    js_append(args_)
  sketch
}


#' @export
arc <- function(sketch, a, b, c, d, start, stop, mode = ~OPEN){
  sketch$x$draw %>%
    js_append(sprintf("p.arc(%s,%s,%s,%s,%s,%s,%s);", a, b, c, d, start, stop, mode))
}
