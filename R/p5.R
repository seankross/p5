#' <Add Title>
#'
#' <Add Description>
#'
#' @import htmlwidgets
#'
#' @export
p5 <- function(data = NULL, padding = 0) {

  # forward options using x
  x = list(
    type = "sketch",
    setup = JS("function setup() {", "}"),
    draw = JS("function draw() {", "}"),
    data = data
  )

  # create widget
  htmlwidgets::createWidget(
    name = "p5",
    x,
    width = 0,
    height = 0,
    sizingPolicy = sizingPolicy(padding = padding)
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
  JS(c(tokens, end))
}

#' @export
ellipse <- function(sketch, x, y, w, h = NULL){

}

#' @export
fill <- function(sketch, r, g, b, a = NULL){

}

#' @export
background <- function(sketch, r, g, b, a = NULL){
  sketch$x$setup <- sketch$x$setup %>%
   js_append(sprintf("background(%d,%d,%d,%d);", r, g, b, ifelse(is.null(a), 100, a)))
  sketch
}

#' @export
createCanvas <- function(sketch, w, h){
  sketch$x$setup <- sketch$x$setup %>%
    js_append(sprintf("createCanvas(%d,%d);", w, h))
  sketch
}

#' @export
p5_js <- function(...){

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
