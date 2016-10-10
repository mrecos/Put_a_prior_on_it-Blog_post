library("purrr")
library("ggplot2")
alpha1 <- c(0.5, 1, 2, 3)
beta1 <- c(0.5, 1, 2, 3)
b1 <- data.frame(expand.grid(alpha1, beta1))
purrr::map2(.x = b1[,1],
            .y = b1[,2],
            .f = ~ rbeta(n = 1e3, # warning, 1e7 takes a while!
                         shape1 = .x, 
                         shape2 = .y
            )
) %>%
  as.data.frame() %>%
  setNames(., paste0("alpha~", b1[,1], "~~beta~", b1[,2])) %>%
  gather() %>%
  ggplot(aes(x = value, group = key)) +
  geom_density(color = "skyblue3", fill = "skyblue") +
  facet_wrap(~key, ncol = 4, scales = "free",  labeller = label_parsed) +
  theme_minimal() +
  labs(title = "The Versatile Beta Distribution\n") +
  xlab(NULL) +
  ylab(NULL) +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 0), # hack
    strip.text.x = element_text(size = 11, face = "bold",
                                family="Trebuchet MS"),
    plot.title=element_text(hjust=0.5, size = 15,
                            family="Trebuchet MS", face = "bold")
  )
ggsave("~/Dropbox/R/lyrics_bayes/beta_dist_plot.png", 
       height = 5, width = 6)

