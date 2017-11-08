source("")

library(dplyr)
library(ggplot2)
source("f-help-functions.r")

df.order <- read.table("b-set_fit_order.csv")
tb.order <- as_tibble(df.order)

## Pfad zum Speichern von Abbildungen festlegen:
save.dir <- "/home/skiefer/01-Master-Thesis/02-code-git/05-visu-pdf-tikz/"
# "05-visu-pdf-tikz/"



ggp.order.rmse <-
  ggplot(data = tb.order,
         mapping = aes(x = order, y = rmse)) +
  geom_point() +
  scale_x_continuous(name = "Ordnung des Fits") +
  scale_y_continuous(name = "Betrag der mittleren Distanz \n der Maximal-Jetstreams in $^{\\circ}$") +
  geom_vline(xintercept = 24, linetype = "dotdash") +
  theme_bw()
print(ggp.order.rmse)

ggp.order.dist <-
  ggplot(data = tb.order,
         mapping = aes(x = order, y = dist)) +
  geom_point() + 
  scale_x_continuous(name = "Ordnung des Fits") +
  scale_y_continuous(name = "Betrag der mittleren Distanz \n zwischen Polarfront- und \n Subtropen-Jetstream in $^{\\circ}$") +
  geom_vline(xintercept = 24, linetype = "dotdash") +
  theme_bw()
print(ggp.order.dist)

# 
# plt.save(plt = ggp.order.rmse, 
#          width = 140, height = 70, pointsize = 11,
#          filepath = "/home/skiefer/01-Master-Thesis/02-code-git/05-visu-pdf-tikz/00-fit-order/", 
#          filename = "order_rmse")
# plt.save(plt = ggp.order.dist, 
#          width = 140, height = 70, pointsize = 11,
#          filepath = "/home/skiefer/01-Master-Thesis/02-code-git/05-visu-pdf-tikz/00-fit-order/", 
#          filename = "order_dist")



plt.save(plt = ggp.order.rmse, 
         width = 135, height = 70, pointsize = 11,
         filepath = paste0(save.dir, "00-fit-order/"), 
         filename = "order_rmse")
plt.save(plt = ggp.order.dist, 
         width = 135, height = 70, pointsize = 11,
         filepath = paste0(save.dir, "00-fit-order/"), 
         filename = "order_dist")
