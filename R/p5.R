#' <Add Title>
#'
#' <Add Description>
#'
#' @import htmlwidgets
#'
#' @export
p5 <- function(data = NULL, width = NULL, height = NULL, padding = 0) {
  if(!is.null(data) && !is.data.frame(data)){
    stop("The data argument must be a data frame.")
  }

  id <- paste(c("p5-", sample(0:9, 10, replace = TRUE)), collapse = "")
  fn <- paste(sample(letters, 10, replace = TRUE), collapse = "")

  # forward options using x
  x = list(
    section = "sketch",
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

unform <- function(form){
  if(length(form) != 2){
    stop("Formula argument should be one sided.")
  }
  as.character(form)[2]
}

#' @importFrom htmltools validateCssUnit
validCssUnit <- function(x){
  result <- tryCatch(validateCssUnit(x),
           error = function(e) TRUE)
  !isTRUE(result)
}

get_section <- function(sketch, verb){
  if(sketch$x$section != "sketch"){
    return(sketch$x$section)
  }

  if(verb %in% c("createCanvas")){
    "setup"
  } else {
    "draw"
  }
}

#' @importFrom purrr map pmap
#' @importFrom rlang is_formula
prepare_args <- function(sketch, ...){
  args_ <- list(...) %>%
    map2(names(list(...)), function(x_, n_){
      if(is.null(x_) && (n_ %in% colnames(sketch$x$data))){
        n_
      } else if(is_formula(x_)){
        unform(x_)
      } else {
        as.character(x_)
      }
    }) %>%
    map(function(x_){
      x_is_a_colname <- !is.null(sketch$x$data) && (x_ %in% colnames(sketch$x$data))
      if(x_is_a_colname){
        sketch$x$data[[x_]]
      } else {
        x_
      }
    })

  args_ %>%
    map(function(x_){
      if(length(x_) != max(lengths(args_))){
        rep_len(x_, max(lengths(args_)))
      } else {
        x_
      }
    }) %>%
    pmap(function(...){
      list(...)
    })
}
