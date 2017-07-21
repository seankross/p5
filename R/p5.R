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

js_append <- function(js, to_append){
  tokens <- unlist(strsplit(js, "\n"))
  end <- tokens[length(tokens)]
  tokens[length(tokens)] <- to_append
  JS_(c(tokens, end))
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

parse_rhs <- function(rhs, acc = NULL){
  if(is.name(rhs)){
    return(c(paste0("p.", as.character(rhs)), acc))
  } else if(is.call(rhs)){
    sym <- as.character(rhs[[1]])
    rhs_ <- rhs[[2]]
    nm <- paste0("p.", as.character(rhs[[3]]))
    parse_rhs(rhs_, c(sym, nm, acc))
  }
}

parse_formula <- function(form){
  rhs <- form[[2]]
  parse_rhs(rhs)
}

is_constant <- function(y, sketch = NULL){
  constants <- c("HALF_PI", "PI", "QUARTER_PI", "TAU", "TWO_PI", "frameCount",
           "focused", "displayWidth", "displayHeight", "windowWidth",
           "windowHeight", "width", "height",  "deviceOrientation",
           "accelerationX", "accelerationY", "accelerationZ", "pAccelerationX",
           "pAccelerationY", "pAccelerationZ", "rotationX", "rotationY",
           "rotationZ", "pRotationX", "pRotationY", "pRotationZ",
           "keyIsPressed", "key", "keyCode", "mouseX", "mouseY", "pmouseX",
           "pmouseY", "winMouseX", "winMouseY", "pwinMouseX", "pwinMouseY",
           "mouseButton", "mouseIsPressed", "touches", "pixels", "OPEN",
           "CHORD", "PIE")
  if(!is.null(sketch$x$data)){
    !(y %in% colnames(sketch$x$data)) && (y %in% constants)
  } else {
    y %in% constants
  }
}

#' @importFrom stringr str_extract_all
#' @importFrom purrr map_chr
unform <- function(form, sketch = NULL){
  if(length(form) != 2){
    stop("Formula argument should be one sided.")
  }

  formula_string <- as.character(form)[2]
  words_ <- formula_string %>% str_extract_all("\\w+", simplify = TRUE) %>% as.character()
  symbols_ <- formula_string %>% str_extract_all("[^\\w\\s]+", simplify = TRUE) %>% as.character()

  # Check is any of words_ are constants and if they are add a "p."
  words_ <- words_ %>% map_chr(~ ifelse(is_constant(.x, sketch), paste0("p.", .x), .x))

  if(length(words_) == 1){
    words_
  } else {
    paste(paste0(words_, c(symbols_, "")), collapse = "")
  }
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

  if(verb %in% c("createCanvas", "background", "noLoop")){
    "setup"
  } if(verb %in% c("js")){
    "post"
  } else {
    "draw"
  }
}

#' @importFrom purrr map pmap map2
#' @importFrom rlang is_formula
prepare_args <- function(sketch, ...){
  args_ <- list(...) %>%
    map2(names(list(...)), function(x_, n_){
      if(is.null(x_) && (n_ %in% colnames(sketch$x$data))){
        n_
      } else if(is_formula(x_)){
        unform(x_, sketch)
      } else if(is.null(x_)){
        "null"
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
