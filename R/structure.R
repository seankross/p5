#' Stop draw() from looping.
#'
#' @param sketch A p5 sketch.
#'
#' @export
#' @examples
#' \dontrun{
#' # Click the resulting sketch
#'
#' pre_ <- pre() %>%
#'   js("var x = 0;")
#' setup_ <- setup() %>%
#'   createCanvas(100, 100)
#' draw_ <- draw() %>%
#'   background("#F0F8FF") %>%
#'   js(" x = x + 0.1;
#'        if (x > width) {
#'          x = 0;
#'        } ") %>%
#'    line(~x, 0, ~x, ~height)
#' post_ <- post() %>%
#'   js(" mousePressed = function() {
#'          noLoop();
#'        }
#'
#'        mouseReleased = function() {
#'          loop();
#'        } ")
#' sketch(pre = pre_, setup = setup_, draw = draw_, post = post_)
#' }
noLoop <- function(sketch){
  empty_prototype(sketch, "noLoop")
}

#' Add Javascript to any part of a sketch
#'
#' @param sketch A p5 sketch.
#' @param ... Strings of Javascript code to add to the sketch.
#' @param objn The name of the sketch object followed by a period. Most of the
#' time you should keep the default `p.`, however it can be useful to change this
#' to and empty string (`""`).
#' @importFrom stringr str_replace_all
#' @export
js <- function(sketch, ..., objn = "p."){
  p5_api <- "(alpha|blue|brightness|color|green|hue|lerpColor|lightness|red|saturation|background|clear|colorMode|fill|noFill|noStroke|stroke|arc|ellipse|line|point|quad|rect|triangle|ellipseMode|noSmooth|rectMode|smooth|strokeCap|strokeJoin|strokeWeight|bezier|bezierPoint|bezierTangent|curve|curveTightness|curvePoint|curveTangent|beginContour|beginShape|bezierVertex|curveVertex|endContour|endShape|quadraticVertex|vertex|loadModel|model|plane|box|sphere|cylinder|cone|ellipsoid|torus|HALF_PI|PI|QUARTER_PI|TAU|TWO_PI|preload|setup|draw|remove|noLoop|loop|push|pop|redraw|print|frameCount|focused|cursor|frameRate|noCursor|displayWidth|displayHeight|windowWidth|windowHeight|windowResized|width|height|fullscreen|pixelDensity|displayDensity|getURL|getURLPath|getURLParams|createCanvas|resizeCanvas|noCanvas|createGraphics|blendMode|applyMatrix|resetMatrix|rotate|rotateX|rotateY|rotateZ|scale|shearX|shearY|translate|append|arrayCopy|concat|reverse|shorten|shuffle|sort|splice|subset|float|int|str|boolean|byte|char|unchar|hex|unhex|join|match|matchAll|nf|nfc|nfp|nfs|split|splitTokens|trim|deviceOrientation|accelerationX|accelerationY|accelerationZ|pAccelerationX|pAccelerationY|pAccelerationZ|rotationX|rotationY|rotationZ|pRotationX|pRotationY|pRotationZ|setMoveThreshold|setShakeThreshold|deviceMoved|deviceTurned|deviceShaken|keyIsPressed|key|keyCode|keyPressed|keyReleased|keyTyped|keyIsDown|mouseX|mouseY|pmouseX|pmouseY|winMouseX|winMouseY|pwinMouseX|pwinMouseY|mouseButton|mouseIsPressed|mouseMoved|mouseDragged|mousePressed|mouseReleased|mouseClicked|mouseWheel|touches|touchStarted|touchMoved|touchEnded|createImage|saveCanvas|saveFrames|loadImage|image|tint|noTint|imageMode|pixels|blend|copy|filter|get|loadPixels|set|updatePixels|loadFont|loadJSON|loadStrings|loadTable|loadXML|httpGet|httpPost|httpDo|save|saveJSON|saveStrings|saveTable|day|hour|minute|millis|month|second|year|abs|ceil|constrain|dist|exp|floor|lerp|log|mag|map|max|min|norm|pow|round|sq|sqrt|noise|noiseDetail|noiseSeed|acos|asin|atan|atan2|cos|sin|tan|degrees|radians|angleMode|randomSeed|random|randomGaussian|textAlign|textLeading|textSize|textStyle|textWidth|text|textFont|camera|perspective|ortho|ambientLight|directionalLight|pointLight|OPEN|CHORD|PIE|WEBGL)"

  script <- JS_(...) %>%
    str_replace_all(p5_api, paste0(objn, "\\1"))
  section <- get_section(sketch, "js")

  sketch$x[[section]] <- sketch$x[[section]] %>%
    js_append(script)
  sketch
}

#' Create a sketch from multiple parts
#'
#' @description
#' p5 sketches are split into five sections:
#'
#' 1. Code before the [setup].
#' 2. The [setup] code.
#' 3. Code between [setup] and [draw].
#' 4. The [draw] code.
#' 5. Code after [draw].
#'
#' The [p5] function automatically determines where an illustrating function
#' should be added. You can use `sketch` along with the [pre], [setup],
#' [between], [draw], and [post] functions in order to explicitly specify where
#' illustrating functions should be added.
#'
#' @param pre A [pre] sketch.
#' @param setup A [setup] sketch.
#' @param between A [between] sketch.
#' @param draw A [draw] sketch.
#' @param post A [post] sketch.
#' @param width Width of the sketch.
#' @param height Height of the sketch.
#' @param padding Padding of the sketch.
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

#' Make a setup sketch
#'
#' @param data An optional [tibble::data_frame()].
#' @export
setup <- function(data = NULL){
  make_part(data, "setup")
}

#' Make a draw sketch
#'
#' @param data An optional [tibble::data_frame()].
#' @export
draw <- function(data = NULL){
  make_part(data, "draw")
}

#' Make a pre sketch
#'
#' @param data An optional [tibble::data_frame()].
#' @export
pre <- function(data = NULL){
  make_part(data, "pre")
}

#' Make a post sketch
#'
#' @param data An optional [tibble::data_frame()].
#' @export
post <- function(data = NULL){
  make_part(data, "post")
}

#' Make a between sketch
#'
#' @param data An optional [tibble::data_frame()].
#' @export
between <- function(data = NULL){
  make_part(data, "between")
}

make_part <- function(data, part){
  sketch_part <- p5(data)
  sketch_part$x$section <- part
  sketch_part
}

#' Combine multiple sketches together
#'
#' @importFrom htmlwidgets createWidget
#' @importFrom stringr str_split
#' @export
bind_sketches <- function(..., width = NULL, height = NULL, padding = 0){
  id <- paste(c("p5-", sample(0:9, 10, replace = TRUE)), collapse = "")
  fn <- paste(sample(letters, 10, replace = TRUE), collapse = "")

  # forward options using x
  # x = list(
  #   section = "sketch",
  #   pre = JS_(";"),
  #   setup = JS_("p.setup = function() {", "};"),
  #   between = JS_(";"),
  #   draw = JS_("p.draw = function() {", "};"),
  #   post = JS_(";"),
  #   data = data,
  #   fn = fn
  # )

  sect2vec <- function(sect){
    unlist(str_split(sect, "\n"))
  }

  combine_sections1 <- function(sect1, sect2){
    JS_(sect2vec(sect1), sect2vec(sect2))
  }

  combine_sections2 <- function(sect1, sect2){
    vec1 <- sect2vec(sect1)
    vec2 <- sect2vec(sect2)

    vec1 <- vec1[-length(vec1)]
    vec2 <- vec2[-1]

    JS_(vec1, vec2)
  }

  result <- reduce(list(...), function(sketch1, sketch2){
    sketch1$x$pre <- combine_sections1(sketch1$x$pre, sketch2$x$pre)
    sketch1$x$setup <- combine_sections2(sketch1$x$setup, sketch2$x$setup)
    sketch1$x$between <- combine_sections1(sketch1$x$between, sketch2$x$between)
    sketch1$x$draw <- combine_sections2(sketch1$x$draw, sketch2$x$draw)
    sketch1$x$post <- combine_sections1(sketch1$x$post, sketch2$x$post)
    sketch1
  })

  # create widget
  htmlwidgets::createWidget(
    name = "p5",
    result$x,
    width = width,
    height = height,
    sizingPolicy = sizingPolicy(padding = padding),
    elementId = id
  )
}
