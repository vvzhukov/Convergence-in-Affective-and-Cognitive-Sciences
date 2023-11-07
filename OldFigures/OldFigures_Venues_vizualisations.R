library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(broom)

n_data_plot <- function(SA1a) {
  colnames(SA1a)[2] <- "n_data"
  #SA1a$Total_Citations <- as.numeric(SA1a$Total_Citations)
  SA1a$JIF_2021 <- as.numeric(SA1a$JIF_2021)
  SA1a$JCI_2021 <- as.numeric(SA1a$JCI_2021)
  SA1a$JIF_5_yrs <- as.numeric(SA1a$JIF_5_yrs)
  SA1a$Article_Influence_score <- as.numeric(SA1a$Article_Influence_score)
  
  SA1a %>% 
    select(c(n_data, JIF_2021, JCI_2021, JIF_5_yrs, Eigenfactor_Norm, Article_Influence_score)) %>% 
    pivot_longer(-n_data) -> tbl_SA1a_long
  
  tbl_SA1a_long <- tbl_SA1a_long[!is.na(tbl_SA1a_long$value),]
  
  
  plot <- tbl_SA1a_long %>%
    mutate(text = fct_reorder(name, value)) %>%
    ggplot( aes(x=value, color=text, fill=text)) +
    geom_histogram(alpha=0.6, bins = 300) +
    scale_x_continuous(limits=c(0,15)) +
    scale_fill_viridis(discrete=TRUE) +
    scale_color_viridis(discrete=TRUE) +
    theme_ipsum() +
    theme(
      legend.position="none",
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 8)
    ) +
    xlab("") +
    ylab("") +
    facet_wrap(~text)
  
  return(plot)
}

n_data_plot(SA1a)
n_data_plot(SA2a)
n_data_plot(SA3a)
n_data_plot(SA4a)
n_data_plot(SA5a)

n_data_plot(SA1c)
n_data_plot(SA2c)
n_data_plot(SA3c)
n_data_plot(SA4c)
n_data_plot(SA5c)

# ------------------------------------------------------------------------------
# -----------------------------YEARLY STATS-------------------------------------
# ------------------------------------------------------------------------------

n_data_plot_y <- function(SA1ay) {
  colnames(SA1ay)[3] <- "n_data"
  SA1ay$JIF_2021 <- as.numeric(SA1ay$JIF_2021)
  SA1ay$JCI_2021 <- as.numeric(SA1ay$JCI_2021)
  SA1ay$JIF_5_yrs <- as.numeric(SA1ay$JIF_5_yrs)
  SA1ay$Article_Influence_score <- as.numeric(SA1ay$Article_Influence_score)
  SA1ay$Total_Citations <- NULL
  
  SA1ay %>% 
    select(c(year, JIF_2021, JCI_2021, JIF_5_yrs, Eigenfactor_Norm, Article_Influence_score)) %>% 
    pivot_longer(-year) -> tbl_SA1ay_long
  
  tbl_SA1ay_long <- tbl_SA1ay_long[!is.na(tbl_SA1ay_long$value),]

  plot <- ggplot(tbl_SA1ay_long) + 
    # observed points, model, and se
    geom_point(alpha = 0.1,
               aes(value, year), tbl_SA1ay_long) +  
    facet_wrap(~ name, scales = "free_x") + 
    ylim(1935, 2025) +
    xlab("") +
    ylab("") + 
    coord_flip()
  
  return(plot)
}


n_data_plot_y(SA1ay)
n_data_plot_y(SA2ay)
n_data_plot_y(SA3ay)
n_data_plot_y(SA4ay)
n_data_plot_y(SA5ay)

n_data_plot_y(SA1cy)
n_data_plot_y(SA2cy)
n_data_plot_y(SA3cy)
n_data_plot_y(SA4cy)
n_data_plot_y(SA5cy)





SA1a$SA <- 'SA1a'
SA2a$SA <- 'SA2a'
SA3a$SA <- 'SA3a'
SA4a$SA <- 'SA4a'
SA5a$SA <- 'SA5a'
SA1c$SA <- 'SA1c'
SA2c$SA <- 'SA2c'
SA3c$SA <- 'SA3c'
SA4c$SA <- 'SA4c'
SA5c$SA <- 'SA5c'

all_SAs <- rbind(SA1a, SA2a, SA3a, SA4a, SA5a, SA1c, SA2c, SA3c, SA4c, SA5c)
all_SAs <- all_SAs[,c('JIF_2021', 'SA')]
colnames(all_SAs) <- c("value", "text")


typeof(data$value)
all_SAs$value <- as.double(all_SAs$value)

all_SAs %>%
  mutate(text = fct_reorder(text, value)) %>%
  ggplot( aes(x=value, color=text, fill=text)) +
  geom_histogram(alpha=0.6, bins = 10) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("") +
  ylab("Journal Impact Factor (JIF) 2021") +
  scale_x_continuous(limits=c(0,20)) +
  facet_wrap(~text)
