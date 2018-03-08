
library(dplyr)
library(ggplot2)
file <- function(theme){
  path % file.path() %>% png(,width=960,height=480)
}
file(theme = "theme_linedraw")
ggplot(data=diamonds, aes(carat,price ))+
  geom_point(aes(colour= color))+
  theme_linedraw()
