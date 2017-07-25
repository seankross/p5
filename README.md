# p5

https://p5js.org/

## Installation

```r
devtools::install_github("seankross/p5")
```

## Demo

```r
library(p5)
library(tibble)

squares <- data_frame(x = c(100, 100, 200, 200),
                      y = c(50, 150, 50, 150),
                      w = rep(40, 4),
                      h = rep(40, 4))

squares %>%
  p5() %>%
  createCanvas(300, 200) %>%
  background("#002d72") %>%
  rect()
  
p5() %>%
  createCanvas(100, 100) %>%
  background("#808080") %>%
  arc(50, 55, 50, 50, 0, ~HALF_PI) %>%
  noFill() %>%
  arc(50, 55, 60, 60, ~HALF_PI, ~PI) %>%
  arc(50, 55, 70, 70, ~PI, ~PI+QUARTER_PI) %>%
  arc(50, 55, 80, 80, ~PI+QUARTER_PI, ~TWO_PI)
  
squares %>%
  draw() %>%
  fill("#808080") %>%
  rect() %>%
  sketch(draw = ., 
    setup = setup() %>% createCanvas(300, 200))
  
draw() %>%
  background("#F4F8FC") %>%
  line(~mouseX, 0, ~mouseX, 200) %>%
  sketch(draw = .)

p5() %>%
  createCanvas(400, 300) %>%
  background("#F4F8FC") %>%
  ellipse(~mouseX, ~mouseY, 30, 30)
  
setup_ <- setup() %>%
  createCanvas(640, 480)
draw_ <- draw() %>%
  js("
      if (mouseIsPressed) {
        fill(0);
      } else {
        fill(255);
      }
  ") %>%
  ellipse(~mouseX, ~mouseY, 80, 80)
sketch(setup = setup_, draw = draw_)
```


