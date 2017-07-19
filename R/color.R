# Creating & Reading


# Setting

#' @export
background <- function(sketch, colorstring){
  section <- get_section(sketch, "background")

  sketch$x[[section]] <- sketch$x[[section]] %>%
    js_append(sprintf("p.background('%s');", colorstring))
  sketch
}

#' @export
stroke <- function(sketch, colorstring, alpha = 100){
  section <- get_section(sketch, "stroke")

  sketch$x[[section]] <- sketch$x[[section]] %>%
    js_append(sprintf("p.stroke('%s');", colorstring, alpha))
  sketch
}
