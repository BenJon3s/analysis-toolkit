#### Simple Spaghetti Plot ####
#' Creates a faceted time by protein plot colored by group for Olink HT Protein 
#'
#' @param data data frame containing variables protein_var, grouping_var, time_var, 
#' y_var, and subject_var.
#' @param protein_list Character vector containing desired proteins to plot present in protein_var
#' @param protein_var Character. Variable containing the name of protein assigned to y_var
#' @param grouping_var Character. Variable containing the group for cumulative trend lines
#' @param time_var Numeric/date. Variable containing the date of each y_var
#' @param y_var Numeric. Variable containing Protein Expression value or other
#' @param subject_var Character. Variable containing the per-individual unique identifier
#' @param custom_color Character vector of hexcodes or r colors to use in plot.
#' When left as default NULL, color palette Set1 is used 
#' @param plot_method Character string specifying smoothing line to use as specified in method within ggplot2::geom_smooth. Default is loess curve
#' @return ggplot object displaying the spaghetti plot
#' 
#' @examples
#' simple_spaghetti(data = spaghetti_dat,
#' protein_list = c("Protein1", "Protein2"),
#' protein_var = Assay,
#' grouping_var = treatment_arm,
#' time_var = baseline_week, 
#' y_var = LogProtExp_Raw,
#' subject_var = SubjectRef,
#' custom_color = c("#377EB8", "#E41A1C"),
#' plot_method = "loess")
#' 
#' 
#' 
#' 
library(dplyr)
library(ggplot2)
library(ggprism)

simple_spaghetti <- function(data, 
                             protein_list, 
                             protein_var,
                             grouping_var,
                             time_var, 
                             y_var, 
                             subject_var, 
                             custom_color = NULL, 
                             plot_method = "loess"){
  
  plot_obj <- data %>%
    dplyr::filter({{protein_var}} %in% protein_list) %>%
    ggplot2::ggplot(ggplot2::aes(x = {{time_var}},
                                 y = {{y_var}},
                                 color = {{grouping_var}})) +
    ggplot2::geom_line(
      ggplot2::aes(group = {{subject_var}},
                   color = {{grouping_var}}),
      linetype = "dashed",
      size = 0.2) +
    ggplot2::geom_smooth(se = F,
                         size = 1.5,
                         ggplot2::aes(color = {{grouping_var}}), 
                         method = plot_method) +
    ggplot2::facet_wrap(vars({{protein_var}}),
                        scales = 'free') +
    ggprism::theme_prism()
  
  
  if(is.null(custom_color)){
    plot_result <- plot_obj +
      ggplot2::scale_color_brewer(palette = "Set1")
  } else{
    plot_result <- plot_obj +
      ggplot2::scale_color_manual(values = custom_color)
  }
  
  return(plot_result)
  
}

