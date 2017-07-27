# Creating & Reading


# Setting

#' Set the canvas bachground color
#'
#' @param sketch A p5 sketch.
#' @param colorstring A hex color string.
#' @export
#' @examples
#' \dontrun{
#'
#' p5() %>%
#'   background("#FAE")
#'
#' setup() %>%
#'   background(rgb(255, 204, 0, maxColorValue = 255))
#'
#' draw() %>%
#'   background("#808080")
#'
#' }
background <- function(sketch, colorstring){
  color_prototype(sketch, "background", colorstring)
}

#' Set the color used to draw lines around shapes
#'
#' @param sketch A p5 sketch.
#' @param colorstring A hex color string.
#' @export
#' @examples
#' \dontrun{
#'
#' p5() %>%
#'   stroke("#FF0000") %>%
#'   rect(10, 10, 20, 30) %>%
#'   stroke("#0000FF") %>%
#'   rect(20, 30, 30, 20)
#'
#' }
stroke <- function(sketch, colorstring){
  color_prototype(sketch, "stroke", colorstring)
}

#' Turn off shape outlines
#'
#' @param sketch A p5 sketch.
#' @export
#' @examples
#' \dontrun{
#'
#' p5() %>%
#'   stroke("#FF0000") %>%
#'   fill("#DCDCDC") %>%
#'   rect(10, 10, 20, 30) %>%
#'   noStroke() %>%
#'   rect(40, 10, 20, 30)
#'
#' }
noStroke <- function(sketch){
  empty_prototype(sketch, "noStroke")
}


#' Set shape color
#'
#' @param sketch A p5 sketch.
#' @param colorstring A hex color string.
#' @export
#' @examples
#' \dontrun{
#'
#' p5() %>%
#'   fill("#FAE") %>%
#'   rect(10, 10, 10, 10) %>%
#'   fill("#808080") %>%
#'   rect(30, 30, 10, 10)
#'
#' }
fill <- function(sketch, colorstring){
  color_prototype(sketch, "fill", colorstring)
}

#' Disable shape color
#'
#' @param sketch A p5 sketch.
#' @export
#' @examples
#' \dontrun{
#'
#' p5() %>%
#'   fill("#FAE") %>%
#'   rect(10, 10, 10, 10) %>%
#'   noFill() %>%
#'   rect(30, 30, 10, 10)
#'
#' }
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
