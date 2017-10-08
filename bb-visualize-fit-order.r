source("")

library(dplyr)
library(ggplot2)
source("f-help-functions.r")

df.order <- read.table("b-set_fit_order.csv")
tb.order <- as_tibble(df.order)

ggp.order.rmse <-
  ggplot(data = tb.order,
       mapping = aes(x = order, y = rmse)) +
  geom_point()
print(ggp.order.rmse)

ggp.order.dist <-
  ggplot(data = tb.order,
         mapping = aes(x = order, y = dist)) +
  geom_point()
print(ggp.order.dist)

ggp.rmse.dist <-
  ggplot(data = tb.order,
         mapping = aes(x = rmse, y = dist, color = order)) +
  geom_point() + scale_color_distiller(palette = "RdYlBu") +
  geom_line()
print(ggp.rmse.dist)


