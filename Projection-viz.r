library(ggplot2)
library(dplyr)
library(ggrepel)
library(grid)

# ---- Theme for poster ----
plot_aes <- theme_minimal(base_size = 16) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 14),
    text = element_text(size = 16, family = "Futura Medium"),
    axis.text = element_text(color = "black", size = 14),
    axis.title = element_text(size = 16, face = "bold"),
    axis.ticks = element_line(size = 1),
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold")
  )

# ---- Example embedding space ----
set.seed(42)
embedding_2d <- data.frame(
  word = c("I", "me", "my"),
  x = rnorm(3, 0, 2),
  y = rnorm(3, 0, 2)
)

# Positive / Negative poles
H <- c(2, 1)
L <- c(-2, -1)

# Axis vector + extension factor
axis_vec <- H - L
axis_vec <- axis_vec / sqrt(sum(axis_vec^2))
axis_length <- sqrt(sum((H - L)^2))
extension <- 1.5
axis_start <- L - extension * axis_vec
axis_end   <- H + extension * axis_vec

pos_statement <- "I am good / I feel good"
neg_statement <- "I am bad / I feel bad"

# ---- Projection function ----
project_point <- function(pt, anchor = L, axis = axis_vec) {
  v <- pt - anchor
  proj_length <- sum(v * axis)
  proj <- anchor + proj_length * axis
  perp_dist <- sqrt(sum((pt - proj)^2))
  proj_score_norm <- proj_length / axis_length
  list(proj = proj, proj_length = proj_length,
       perp_dist = perp_dist, proj_score_norm = proj_score_norm)
}

projections <- lapply(1:nrow(embedding_2d), function(i) {
  res <- project_point(as.numeric(embedding_2d[i, c("x", "y")]))
  data.frame(
    word = embedding_2d$word[i],
    proj_x = res$proj[1],
    proj_y = res$proj[2],
    proj_length = res$proj_length,
    perp_dist = res$perp_dist,
    proj_score_norm = res$proj_score_norm
  )
})

projections <- bind_rows(projections)
embedding_2d <- embedding_2d %>% left_join(projections, by = "word")

# ---- Offset vectors for arrows ----
offset_factor <- 0.1
embedding_2d <- embedding_2d %>%
  rowwise() %>%
  mutate(
    arrow_end_x = proj_x - offset_factor*(proj_x - L[1]),
    arrow_end_y = proj_y - offset_factor*(proj_y - L[2])
  )

# ---- Perpendicular vector for labels ----
perp_vec <- c(-axis_vec[2], axis_vec[1])
label_nudge <- 1.2  # increase for poster clarity

# ---- Plot ----
p <- ggplot() +
  # Extended axis line
  geom_segment(aes(x = axis_start[1], y = axis_start[2],
                   xend = axis_end[1], yend = axis_end[2]),
               arrow = arrow(length = unit(0.5, "cm"), ends = "both"),
               color = "#1f77b4", size = 1.5) +
  
  # Axis labels
  geom_text_repel(aes(x = axis_end[1], y = axis_end[2],
                      label = paste("Positive pole\n", pos_statement)),
                  color = "#1f77b4", fontface = "bold",
                  box.padding = 1, point.padding = 1.5, nudge_y = 1.0, size = 6) +
  geom_text_repel(aes(x = axis_start[1], y = axis_start[2],
                      label = paste("Negative pole\n", neg_statement)),
                  color = "#1f77b4", fontface = "bold",
                  box.padding = 1, point.padding = 1.5, nudge_y = -1.0, size = 6) +
  
  # Original embeddings
  geom_point(data = embedding_2d, aes(x = x, y = y), color = "blue", size = 6) +
  geom_text_repel(data = embedding_2d, aes(x = x, y = y, label = word),
                  color = "blue", fontface = "bold", size = 6, box.padding = 2, point.padding = 2) +
  
  # Projections
  geom_point(data = embedding_2d, aes(x = proj_x, y = proj_y), color = "orange", size = 6) +
  geom_text_repel(data = embedding_2d, 
                  aes(x = proj_x, y = proj_y,
                      label = paste0(word, "\nscore=", round(proj_score_norm, 2))),
                  color = "orange", size = 5.5,
                  box.padding = 2, point.padding = 2,
                  nudge_x = label_nudge * perp_vec[1],
                  nudge_y = label_nudge * perp_vec[2]) +
  
  # Projection vectors
  geom_segment(data = embedding_2d,
               aes(x = L[1], y = L[2], xend = arrow_end_x, yend = arrow_end_y),
               arrow = arrow(length = unit(0.25, "cm")), color = "darkgreen", size = 1.2) +
  geom_text_repel(data = embedding_2d,
                  aes(x = (L[1]+arrow_end_x)/2, y = (L[2]+arrow_end_y)/2,
                      label = round(proj_length, 2)),
                  color = "darkgreen", size = 5.5, box.padding = 2, point.padding = 2) +
  
  # Perpendiculars
  geom_segment(data = embedding_2d,
               aes(x = x, y = y, xend = proj_x, yend = proj_y),
               linetype = "dashed", color = "gray50") +
  
  # Axes + formatting
  geom_hline(yintercept = 0, color = "black", size = 0.8, alpha = 0.6) +
  geom_vline(xintercept = 0, color = "black", size = 0.8, alpha = 0.6) +
  coord_cartesian(xlim = c(-6, 6), ylim = c(-6, 6)) +
  labs(title = "Word Projections onto Semantic Axis",
       x = "Embedding X", y = "Embedding Y") +
  plot_aes +
  annotate("text", x = -5.5, y = -5.5,
           label = paste(
             "Blue = original embeddings",
             "Orange = projection onto axis (with normalized score)",
             "Green = projection vector from L (with raw length)",
             sep = "\n"),
           hjust = 0, vjust = 0, size = 5, color = "black")

# ---- Save for poster: 8x8 inches ----
ggsave("/Users/sm9518/Downloads/embedding_axis_plot_poster.png", p, width = 8, height = 8, dpi = 300)
