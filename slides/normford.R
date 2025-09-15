library(tidyverse)


markers <- tibble(x = seq(from = -3, to = 3, by = 0.5)) |> 
   mutate(y=dnorm(x))

labels <- tribble(~x, ~y, ~label,
-0.25 , dnorm(-0.25)/2, "19.1%",
0.25 , dnorm(0.25)/2, "19.1%",
-0.75, dnorm(0.75)/2, "15.0%",
0.75, dnorm(0.75)/2, "15.0%",
-1.25, dnorm(1.25)/3, "9.2%",
1.25, dnorm(1.25)/3, "9.2%",
-1.75, dnorm(2.25), "4.4%",
1.75, dnorm(2.25), "4.4%",
-2.25, dnorm(2.25)/2, "1.7%",
2.25, dnorm(2.25)/2, "1.7%",
2.75, dnorm(2.75)*2, "~0.5%",
3.25, dnorm(3.25)*4, "~0.1%",
-2.75, dnorm(2.75)*2, "~0.5%",
-3.25, dnorm(3.25)*4, "~0.1%")



breaks_1 <- seq(-3, 3, by = 1)
sigma_labels <- c("-3σ","-2σ","-1σ","0","+1σ","+2σ","+3σ")
z_labels     <- as.character(breaks_1)
cum_labels   <- c("0.1%","2.3%","15.9%","50%","84.1%","97.7%","99.9%")
tick_labels  <- paste(sigma_labels, z_labels, cum_labels, sep = "\n")

# Samme lineheight begge steder
lh <- 1.00

p <- ggplot(data = data.frame(x = c(-4, 4)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1)) + ylab("") +
  scale_y_continuous(breaks = NULL) +
  geom_segment(data = markers, aes(x=x, y =0, xend = x, yend = y)) +
  geom_text(data = labels, mapping= aes(x,y, label = label)) +
  scale_x_continuous(breaks = breaks_1, labels = tick_labels, name = NULL) +
  annotate("segment", x = -1, y = dnorm(1), xend = 1, yend = dnorm(1), color = "red") +
  annotate("text", x = 0, y = dnorm(0.97), label = "68%", color = "RED") +

  annotate("segment", x = -1.96, y = dnorm(1.96), xend = 1.96, yend = dnorm(1.96), color = "blue") +
  annotate("text", x = 0, y = dnorm(1.9), label = "95%", color = "BLUE") +
  theme(line = element_blank(), rect = element_blank())


# --- Tilføj venstre kolonne med 3-linjers label, med vertikal nudge ---
gt <- ggplotGrob(p)
axis_b_id <- which(gt$layout$name == "axis-b")
axis_b    <- gt$layout[axis_b_id, ]

# Justér denne for at løfte/sænke venstreteksten i forhold til tick-linjerne
y_nudge_lines <- 0.9  # positiv = lidt op, negativ = lidt ned

left_lab <- textGrob(
  "Standard Deviation\nZ-Score\nCumulative Percent",
  x = unit(1, "npc"),
  just = c("right","top"),
  gp = gpar(fontsize = 10, lineheight = lh)
)
# Flyt grob'en en smule op i samme celle som x-aksen
left_lab$vp <- viewport(y = unit(1, "npc") + unit(y_nudge_lines, "lines"),
                        just = c("center","top"))

# Kolonnebredde baseret på tekstens faktiske bredde + lidt luft
left_w <- grobWidth(left_lab) + unit(0.6, "lines")

gt <- gtable_add_cols(gt, widths = left_w, pos = axis_b$l - 1)
left_col <- axis_b$l

gt <- gtable_add_grob(
  gt, left_lab,
  t = axis_b$t, b = axis_b$b,
  l = left_col, r = left_col,
  clip = "off"
)

grid.newpage(); grid.draw(gt)

