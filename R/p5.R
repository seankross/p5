#' <Add Title>
#'
#' <Add Description>
#'
#' @import htmlwidgets
#'
#' @export
p5 <- function(data = NULL, width = NULL, height = NULL, padding = 0) {

  id <- paste(c("p5-", sample(0:9, 10, replace = TRUE)), collapse = "")
  fn <- paste(sample(letters, 10, replace = TRUE), collapse = "")

  # forward options using x
  x = list(
    type = "sketch",
    pre = JS_(";"),
    setup = JS_("p.setup = function() {", "};"),
    between = JS_(";"),
    draw = JS_("p.draw = function() {", "};"),
    post = JS_(";"),
    data = data,
    fn = fn
  )

  # create widget
  htmlwidgets::createWidget(
    name = "p5",
    x,
    width = width,
    height = height,
    sizingPolicy = sizingPolicy(padding = padding),
    elementId = id
  )
}

#' @export
setup <- function(data){

}

#' @export
draw <- function(data){

}


#' Shiny bindings for p5
#'
#' Output and render functions for using p5 within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a p5
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name p5-shiny
#'
#' @export
p5Output <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'p5', width, height, package = 'p5')
}

#' @rdname p5-shiny
#' @export
renderP5 <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, p5Output, env, quoted = TRUE)
}

js_append <- function(js, to_append){
  tokens <- unlist(strsplit(js, "\n"))
  end <- tokens[length(tokens)]
  tokens[length(tokens)] <- to_append
  JS_(c(tokens, end))
}

#' @export
ellipse <- function(sketch, x, y, w, h = NULL){

}

#' @export
fill <- function(sketch, r, g, b, a = NULL){

}

#' @export
p5_js <- function(...){

}

JS_ <- function(...){
  x <- c(...)
  paste(x, collapse = "\n")
}

#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL
