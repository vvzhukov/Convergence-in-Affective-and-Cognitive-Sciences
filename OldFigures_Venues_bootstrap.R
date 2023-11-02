library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(broom)

set.seed(42)
n_boot <- 1000

n_data_plot <- function(SA1a) {
  colnames(SA1a)[2] <- "n_data"
  SA1a$Total_Citations <- as.numeric(SA1a$Total_Citations)
  SA1a$JIF_2021 <- as.numeric(SA1a$JIF_2021)
  SA1a$JCI_2021 <- as.numeric(SA1a$JCI_2021)
  SA1a$JIF_5_yrs <- as.numeric(SA1a$JIF_5_yrs)
  SA1a$Article_Influence_score <- as.numeric(SA1a$Article_Influence_score)
  
  SA1a %>% 
    select(c(n_data, Total_Citations, JIF_2021, JCI_2021, JIF_5_yrs, Eigenfactor_Norm, Article_Influence_score)) %>% 
    pivot_longer(-n_data) -> tbl_SA1a_long
  
  tbl_SA1a_long <- tbl_SA1a_long[!is.na(tbl_SA1a_long$value),]
  
  tbl_SA1a_long %>% 
    nest(model_data = c(n_data, value)) %>% 
    # for n_data and value observations within each level of name
    mutate(plot_data = map(model_data, ~ {
      # calculate information about the observed mpg and value observations
      # within each level of name to be used in each bootstrap sample
      submodel_data <- .x
      n <- nrow(submodel_data)
      min_x <- min(submodel_data$value)
      max_x <- max(submodel_data$value)
      pred_x <- seq(min_x, max_x, length.out = 100)
      
      # do the bootstrapping by
      # 1) repeatedly sampling samples of size n with replacement n_boot times,
      # 2) for each bootstrap sample, fit a model, 
      # 3) and make a tibble of predictions
      # the _dfr means to stack each tibble of predictions on top of one another
      map_dfr(1:n_boot, ~ {
        submodel_data %>% 
          sample_n(n, TRUE) %>% 
          lm(n_data ~ value, .) %>% 
          # suppress augment() warnings about dropping columns
          { suppressWarnings(augment(., newdata = tibble(value = pred_x))) }
      }) %>% 
        # the bootstrapping is finished at this point
        # now work across bootstrap samples at each value
        group_by(value) %>% 
        # to estimate the lower and upper 95% quantiles of predicted mpgs
        summarize(l = quantile(.fitted, .025),
                  u = quantile(.fitted, .975),
                  .groups = "drop"
        ) %>% 
        arrange(value)
    })) %>% 
    select(-model_data) %>% 
    unnest(plot_data) -> tbl_plot_data
  
  plot <- ggplot() + 
    # observed points, model, and se
    geom_point(aes(value, n_data), tbl_SA1a_long) + 
    geom_smooth(aes(value, n_data), tbl_SA1a_long, 
                method = "lm", formula = "y ~ x", alpha = 0.25, fill = "red") + 
    # overlay bootstrapped se for direct comparison
    geom_ribbon(aes(value, ymin = l, ymax = u), tbl_plot_data, 
                alpha = 0.25, fill = "blue") + 
    facet_wrap(~ name, scales = "free_x")
  
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

n_boot <- 1000

n_data_plot_y <- function(SA1ay) {
  colnames(SA1ay)[3] <- "n_data"
  SA1ay$Total_Citations <- as.numeric(SA1ay$Total_Citations)
  SA1ay$JIF_2021 <- as.numeric(SA1ay$JIF_2021)
  SA1ay$JCI_2021 <- as.numeric(SA1ay$JCI_2021)
  SA1ay$JIF_5_yrs <- as.numeric(SA1ay$JIF_5_yrs)
  SA1ay$Article_Influence_score <- as.numeric(SA1ay$Article_Influence_score)
  
  SA1ay %>% 
    select(c(year,n_data, Total_Citations, JIF_2021, JCI_2021, JIF_5_yrs, Eigenfactor_Norm, Article_Influence_score)) %>% 
    pivot_longer(-year) -> tbl_SA1ay_long
  
  tbl_SA1ay_long <- tbl_SA1ay_long[!is.na(tbl_SA1ay_long$value),]
  
  tbl_SA1ay_long %>% 
    nest(model_data = c(year, value)) %>% 
    # for year and value observations within each level of name
    mutate(plot_data = map(model_data, ~ {
      # calculate information about the observed year and value observations
      # within each level of name to be used in each bootstrap sample
      submodel_data <- .x
      n <- nrow(submodel_data)
      min_x <- min(submodel_data$value)
      max_x <- max(submodel_data$value)
      pred_x <- seq(min_x, max_x, length.out = 100)
      
      # do the bootstrapping by
      # 1) repeatedly sampling samples of size n with replacement n_boot times,
      # 2) for each bootstrap sample, fit a model, 
      # 3) and make a tibble of predictions
      # the _dfr means to stack each tibble of predictions on top of one another
      map_dfr(1:n_boot, ~ {
        submodel_data %>% 
          sample_n(n, TRUE) %>% 
          lm(year ~ value, .) %>% 
          # suppress augment() warnings about dropping columns
          { suppressWarnings(augment(., newdata = tibble(value = pred_x))) }
      }) %>% 
        # the bootstrapping is finished at this point
        # now work across bootstrap samples at each value
        group_by(value) %>% 
        # to estimate the lower and upper 95% quantiles of predicted mpgs
        summarize(l = quantile(.fitted, .025),
                  u = quantile(.fitted, .975),
                  .groups = "drop"
        ) %>% 
        arrange(value)
    })) %>% 
    select(-model_data) %>% 
    unnest(plot_data) -> tbl_plot_data
  
  plot <- ggplot() + 
    # observed points, model, and se
    geom_point(aes(value, year), tbl_SA1ay_long) + 
    geom_smooth(aes(value, year), tbl_SA1ay_long, 
                method = "lm", formula = "y ~ x", alpha = 0.25, fill = "red") + 
    # overlay bootstrapped se for direct comparison
    geom_ribbon(aes(value, ymin = l, ymax = u), tbl_plot_data, 
                alpha = 0.25, fill = "blue") + 
    facet_wrap(~ name, scales = "free_x") + 
    ylim(1930, 2030)
  
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