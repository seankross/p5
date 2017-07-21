# Creating & Reading


# Setting

#' @export
background <- function(sketch, colorstring){
  color_prototype(sketch, "background", colorstring)
}

#' @export
stroke <- function(sketch, colorstring){
  color_prototype(sketch, "stroke", colorstring)
}

#' @export
fill <- function(sketch, colorstring){
  color_prototype(sketch, "fill", colorstring)
}

#' @export
noFill <- function(sketch){
  empty_prototype(sketch, "noFill")
}

# Utilities

color_prototype <- function(sketch, func, ...){
  section <- get_section(sketch, func)

  sketch$x[[section]] <- sketch$x[[section]] %>%
    js_append(sprintf(
      paste0("p.", func, "('%s');"), ...))
  sketch
}

empty_prototype <- function(sketch, func){
  section <- get_section(sketch, func)

  sketch$x[[section]] <- sketch$x[[section]] %>%
    js_append(paste0("p.", func, "();"))
  sketch
}
