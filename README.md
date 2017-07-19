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
```


