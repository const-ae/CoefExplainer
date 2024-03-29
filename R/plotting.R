


#' Plot each Group of Datapoints and the Coefficients
#'
#' The groups consists of the different combinations of values for the
#' covariates on the right hand side of the formula.
#'
#' @param coefExplFit the result from the \code{\link{CoefExplainer}()}.
#' @param showCoefficients a boolean that decides if the coefficients are
#'   plotted above the data.
#'
#' @export
plotModel <- function(coefExplFit, showCoefficients = TRUE){

  label_df <- tibble(group = seq_len(coefExplFit$n_groups),
                     label = coefExplFit$labels)

  y_axis_range <- range(coefExplFit$data[[coefExplFit$dependent_var]])
  extend_range <- (y_axis_range[2] - y_axis_range[1]) * 0.1
  y_axis_range <- y_axis_range + c(-1, 1) * extend_range


  mean_df <- coefExplFit$data %>%
    mutate(group = coefExplFit$groups,
           ..prediction = predict(coefExplFit$linear_model)) %>%
    group_by(.data$group) %>%
    summarize(group_mean = mean(!!sym(coefExplFit$dependent_var)),
              prediction = mean(.data$..prediction))

  res <- coefExplFit$data %>%
    mutate(group = coefExplFit$groups) %>%
    inner_join(label_df, by = "group") %>%
    ggplot(aes(x = .data$group, y = !!sym(coefExplFit$dependent_var) )) +
    ggbeeswarm::geom_quasirandom() +
    geom_segment(data = mean_df,
                 aes(xend = as.numeric(.data$group) - 0.2, y = .data$group_mean, yend = .data$group_mean),
                 color = "red", size = 2) +
    geom_segment(data = mean_df,
                 aes(xend = as.numeric(.data$group) + 0.2, y = .data$prediction, yend = .data$prediction),
                 color = "blue", size = 2) +
    coord_trans(ylim =y_axis_range) +
    scale_x_continuous(breaks = seq_len(coefExplFit$n_groups),
                       labels = coefExplFit$labels)

  if(showCoefficients){
    coef_df <- purrr::map_df(seq_len(coefExplFit$n_groups), function(g){
      vec_length <- coefExplFit$model_matrix[coefExplFit$example_for_groups_idx[g], ] * coefExplFit$beta
      vec_length <- vec_length[abs(vec_length) > 1e-10]
      vec_pos <- cumsum(vec_length)

      tibble(group = g,
             start = unname(dplyr::lag(vec_pos, default = 0)),
             end = vec_pos,
             covariate = factor(names(vec_pos), levels = names(coefExplFit$beta)),
             nparam = seq_along(vec_pos))
    })

    res <- res +
      geom_segment(data = coef_df,
                   aes(x = .data$group + .data$nparam * 0.03, xend = .data$group + .data$nparam * 0.03 ,
                       y = .data$start, yend = .data$end, color = .data$covariate),
                   size = 2) +
        scale_color_discrete(drop = FALSE)

  }

  res
}

#' Visualize each combination of covariates of the model matrix
#'
#' @inheritParams plotModel
#' @param model_matrix a model matrix. By default taken from the `coefExplFit` object.
#' @param sel_rows which rows of the model matrix to look at. By default taken from the `coefExplFit` object.
#'
#'
#' @export
plotModelMatrix <- function(coefExplFit,
                            model_matrix = coefExplFit$model_matrix,
                            sel_rows = coefExplFit$example_for_groups_idx){

  if(! missing(coefExplFit) && all(coefExplFit$example_for_groups_idx == sel_rows)){
    labels <- coefExplFit$labels
  }else{
    labels <- if(! is.null(names(sel_rows))){
      names(sel_rows)
    }else{
      paste0("Example ", seq_along(sel_rows))
    }
  }

  as_tibble(model_matrix[sel_rows, ,drop=FALSE]) %>%
    mutate(group = seq_along(sel_rows)) %>%
    pivot_longer(cols = -.data$group, names_to = "covariate", values_to = "value") %>%
    mutate(covariate = factor(.data$covariate, levels = colnames(model_matrix))) %>%
    ggplot(aes(x = .data$covariate, y = .data$group)) +
    geom_point(data= function(x) filter(x, abs(.data$value) > 1e-10), size = 5, aes(color = .data$covariate)) +
    geom_point(data= function(x) filter(x, abs(.data$value) <= 1e-10), size = 5, color = "lightgrey") +
    geom_text(aes(label = round(.data$value, digits = 1))) +
    scale_y_continuous(breaks = seq_along(sel_rows), labels = labels,
                       name = "group", trans = "reverse") +
    scale_x_discrete(position = "top", guide = guide_axis(n.dodge = 3))
}

#' Make a bar plot of the coefficients
#'
#' @inheritParams plotModel
#' @param interceptSeparate should the intercept be placed separately
#'
#' @export
plotCoef <- function(coefExplFit, interceptSeparate = TRUE){

  if(interceptSeparate && "(Intercept)" %in% names(coefExplFit$beta)){
    tmp <- tibble(covariate = factor(names(coefExplFit$beta), levels = names(coefExplFit$beta)),
                  magnitude = coefExplFit$beta) %>%
      mutate(Intercept = .data$covariate == "(Intercept)")
    p1 <- tmp %>%
      filter(.data$Intercept) %>%
      ggplot(aes(x = .data$covariate, y = .data$magnitude)) +
      geom_col(aes(fill = .data$covariate), show.legend = FALSE) +
      scale_x_discrete(position = "top") +
      scale_fill_discrete(drop=FALSE)
    p2 <- tmp %>%
      filter(! .data$Intercept) %>%
      ggplot(aes(x = .data$covariate, y = .data$magnitude)) +
      geom_col(aes(fill = .data$covariate), show.legend = FALSE) +
      scale_x_discrete(position = "top", guide = guide_axis(n.dodge = 3)) +
      scale_fill_discrete(drop=FALSE)

    cowplot::plot_grid(p1, p2, nrow = 1, rel_widths = c(2, sum(! tmp$Intercept) + 1),
                       align = "h")
  }else{
    tibble(covariate = factor(names(coefExplFit$beta), levels = names(coefExplFit$beta)),
           magnitude = coefExplFit$beta) %>%
      ggplot(aes(x = .data$covariate, y = .data$magnitude)) +
      geom_col(aes(fill = .data$covariate)) +
      scale_x_discrete(position = "top", guide = guide_axis(n.dodge = 3))
  }


}


#' Make a combined plot of Model, ModelMatrix, and Coef
#'
#' @inheritParams plotModel
#' @param title the title of the plot. By default the formula
#'
#' @export
plotAll <- function(coefExplFit, title = as.character(coefExplFit$formula)){
  ptitle <- cowplot::ggdraw() + cowplot::draw_label(label = title)
  p1 <- plotModel(coefExplFit) + guides(color = "none")
  p2 <- plotModelMatrix(coefExplFit) + guides(color = "none")
  p3 <- plotCoef(coefExplFit)
  cowplot::plot_grid(ptitle, p1, cowplot::plot_grid(p2,p3, nrow = 1), nrow = 3, rel_heights = c(0.2, 1, 0.8))
}


