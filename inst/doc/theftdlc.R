## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.height = 7,
  fig.width = 7,
  warning = FALSE,
  fig.align = "center"
)

## ----setup, message = FALSE, warning = FALSE----------------------------------
library(dplyr)
library(ggplot2)
library(theft)
library(theftdlc)

## ---- message = FALSE, warning = FALSE, eval = FALSE--------------------------
#  theft::simData

## ---- message = FALSE, warning = FALSE----------------------------------------
head(simData)

## ---- message = FALSE, warning = FALSE----------------------------------------
feature_matrix <- calculate_features(data = simData, 
                                     id_var = "id", 
                                     time_var = "timepoint", 
                                     values_var = "values", 
                                     group_var = "process", 
                                     feature_set = "catch22",
                                     seed = 123)

## ---- message = FALSE, warning = FALSE----------------------------------------
plot(feature_matrix, type = "quality")

## ---- message = FALSE, warning = FALSE----------------------------------------
plot(feature_matrix, type = "matrix", norm_method = "RobustSigmoid")

## ---- message = FALSE, warning = FALSE----------------------------------------
plot(feature_matrix, type = "violin",
     feature_names = c("CO_f1ecac", "PD_PeriodicityWang_th0_01"))

## ---- eval = FALSE------------------------------------------------------------
#  plot(feature_matrix, type = "violin",
#       feature_names = c("CO_f1ecac", "PD_PeriodicityWang_th0_01"),
#       size = 0.7, alpha = 0.9)

## ---- message = FALSE, warning = FALSE----------------------------------------
low_dim <- project(feature_matrix,
                   norm_method = "RobustSigmoid",
                   unit_int = TRUE,
                   low_dim_method = "PCA",
                   seed = 123)

## ---- message = FALSE, warning = FALSE----------------------------------------
plot(low_dim)

## ---- message = FALSE, warning = FALSE----------------------------------------
low_dim2 <- project(feature_matrix,
                    norm_method = "RobustSigmoid",
                    unit_int = TRUE,
                    low_dim_method = "tSNE",
                    perplexity = 10,
                    seed = 123)

plot(low_dim2, show_covariance = FALSE)

## ---- message = FALSE, warning = FALSE----------------------------------------
plot(feature_matrix, type = "cor")

## ---- message = FALSE, warning = FALSE----------------------------------------
feature_classifiers <- classify(feature_matrix,
                                by_set = FALSE,
                                n_resamples = 5,
                                use_null = TRUE)

## ---- message = FALSE, warning = FALSE----------------------------------------
myclassifier <- function(formula, data){
  mod <- e1071::svm(formula, data = data, kernel = "radial", scale = FALSE,
                    probability = TRUE)
}

feature_classifiers_radial <- classify(feature_matrix,
                                       classifier = myclassifier,
                                       by_set = FALSE,
                                       n_resamples = 5,
                                       use_null = TRUE)

## ---- message = FALSE, warning = FALSE----------------------------------------
feature_vs_null <- compare_features(feature_classifiers,
                                    by_set = FALSE,
                                    hypothesis = "null")

head(feature_vs_null)

## ---- message = FALSE, warning = FALSE----------------------------------------
pairwise_features <- compare_features(feature_classifiers,
                                      by_set = FALSE,
                                      hypothesis = "pairwise",
                                      p_adj = "holm")

head(pairwise_features)

## ---- message = FALSE, warning = FALSE----------------------------------------
top_10 <- feature_vs_null %>%
  dplyr::slice_min(p.value, n = 10) %>%
  dplyr::select(c(feature_set, original_names, p.value))

feature_matrix_filt <- feature_matrix %>%
  dplyr::filter(feature_set %in% top_10$feature_set & names %in% top_10$original_names)

feature_matrix_filt <- structure(feature_matrix_filt, class = c("feature_calculations", "data.frame"))
plot(feature_matrix_filt, type = "cor")

## ---- message = FALSE, warning = FALSE----------------------------------------
plot(feature_matrix_filt,
     type = "violin",
     feature_names = top_10$original_names)

## ---- message = FALSE, warning = FALSE----------------------------------------
interval(feature_classifiers, by_set = FALSE)

## ---- message = FALSE, warning = FALSE----------------------------------------
feature_matrix2 <- calculate_features(data = simData, 
                                      group_var = "process", 
                                      feature_set = "catch22",
                                      features = list("mean" = mean, "sd" = sd),
                                      seed = 123)

set_classifiers <- classify(feature_matrix2,
                                        by_set = TRUE,
                                        n_resamples = 5,
                                        use_null = TRUE)

head(set_classifiers)

## ---- message = FALSE, warning = FALSE----------------------------------------
interval_calcs <- interval(set_classifiers)

interval_calcs %>%
  ggplot2::ggplot(ggplot2::aes(x = reorder(feature_set, -.mean), y = .mean,
                               colour = feature_set)) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = .lower, ymax = .upper)) +
  ggplot2::geom_point(size = 5) +
  ggplot2::labs(x = "Feature set",
                y = "Classification accuracy") +
  ggplot2::scale_colour_brewer(palette = "Dark2") +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "none",
                 panel.grid.minor = ggplot2::element_blank())

## ---- message = FALSE, warning = FALSE----------------------------------------
feature_clusters <- cluster(feature_matrix, k = 6)

## ---- message = FALSE, warning = FALSE----------------------------------------
feature_clusters$Data %>%
    dplyr::filter(names %in% c("CO_HistogramAMI_even_2_5", 
                               "DN_OutlierInclude_p_001_mdrmd")) %>%
    tidyr::pivot_wider(id_cols = c("id", "group", "cluster"), 
                       names_from = "names", values_from = "values") %>%
    ggplot2::ggplot(ggplot2::aes(x = CO_HistogramAMI_even_2_5, 
                                 DN_OutlierInclude_p_001_mdrmd, 
                                 colour = as.factor(cluster))) +
    ggplot2::stat_ellipse(ggplot2::aes(fill = as.factor(cluster)), geom = "polygon", alpha = 0.2) +
    ggplot2::geom_point() +
    ggplot2::labs(colour = "Cluster") +
    ggplot2::guides(fill = "none") +
    ggplot2::scale_fill_brewer(palette = "Dark2") +
    ggplot2::scale_colour_brewer(palette = "Dark2") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom",
                   panel.grid.minor = ggplot2::element_blank())

