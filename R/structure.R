#' @export
noLoop <- function(sketch){
  empty_prototype(sketch, "noLoop")
}

#' @export
js <- function(sketch, ...){
  script <- JS_(...)
  section <- get_section(sketch, "js")

  sketch$x[[section]] <- sketch$x[[section]] %>%
    js_append(script)
  sketch
}

#' @importFrom purrr map2
#' @export
sketch <- function(pre = NULL, setup = NULL, between = NULL, draw = NULL,
                   post = NULL, width = NULL, height = NULL, padding = 0){
  args_ <- list(pre = pre, setup = setup, between = between, draw = draw, post = post) %>%
    map2(names(.), ~
          if(is.null(.x) && (.y %in% c("setup", "draw"))){
            JS_(paste0("p.", .y, " = function() {"), "};")
          } else if(is.null(.x)) {
            ";"
          } else {
            .x$x[[.y]]
          }
         )

  id <- paste(c("p5-", sample(0:9, 10, replace = TRUE)), collapse = "")
  fn <- paste(sample(letters, 10, replace = TRUE), collapse = "")

  # forward options using x
  x = list(
    section = "sketch",
    pre = args_$pre,
    setup = args_$setup,
    between = args_$between,
    draw = args_$draw,
    post = args_$post,
    data = NULL,
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
setup <- function(data = NULL){
  make_part(data, "setup")
}

#' @export
draw <- function(data = NULL){
  make_part(data, "draw")
}

make_part <- function(data, part){
  sketch_part <- p5(data)
  sketch_part$x$section <- part
  sketch_part
}
