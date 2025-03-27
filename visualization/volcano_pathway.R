#' Plot Volcano plot with labeled pathways and points and Save Volcano plot to current working directory
#' 
#' Creates a volcano plot of usually log2fc by -log p-value primarily used for proteomics data.
#' 
#' @param volcano_dat Data frame with rows as individual proteins and containing columns fc, p.value, pathway_var, and protein_var
#' @param fc Numeric. Variable found in volcano_dat that contains the fold-change or difference between 2 groups per protein
#' @param p.value Numeric. Variable that contains p-value of difference between 2 groups per protein
#' @param pathway_var Character. Variable containing name of associated pathway with a given protein specified in protein_var
#' @param protein_var Character. Variable containing GeneID of the protein associated with each FC and p.value difference
#' @param xlab Expression or Character string. X label for the plot - describes fc variable. Default value is expression("Log"[2]*" fold change") 
#' 
#' @param ylab Expression or Character string. Y label for the plot - describes p.value variable. Default value is expression("-Log"[10]*" (P-value)")
#' @param title Expression or Character string. Title for the volcano plot describing the comparison being made and the directionality of the LogFC, also used in the file naming
#' @param palette Character string corresponding to one of the palettes recognized by scale_color_brewer. Only evaluated when manual_color = F
#' @param manual_color Logical. If F then color_palette will be used for the pathways, if T then custom color values can be used
#' @param color_val Character String or Character vector consisting of colors used for the pathways. Length must be the same as length of unique pathway_var
#' @param legend Logical. If F then plot is generated without a legend, if T then plot is generated with a gigantic legend, which I really just use when I'm generating multiple of these plots for a panel
#' 
#' @param save_plot Logical. If F, plot is only shown in viewing window, if T then plot is saved to current working directory as a 10x10 .jpg
#' @return ggplot object showing the volcano plot
#' 
#' @examples
#' volcano_pathway(volcano_dat = volcano_dat,
#' fc = a_vs_b_Log2FC,
#' p.value = a_vs_b_Pvalue,
#' pathway_var = StringDB_pathways,
#' protein_var = GeneID,
#' palette = 'Set1',
#' manual_color = F,
#' legend = F,
#' save_plot = T,
#' title = "A vs B")
#' 
#' @importFrom dplyr %>%
library(ggplot2)
library(dplyr)

volcano_pathway <- function(volcano_dat, 
                            fc,
                            p.value,
                            pathway_var,
                            protein_var,
                            xlab = expression("Log"[2]*" fold change"),
                            ylab = expression("-Log"[10]*" (P-value)"),
                            title,
                            palette = "Set1",
                            manual_color = F, 
                            color_val = "#E41A1C",
                            legend = F,
                            save_plot = F){
  
  volcano_path <- volcano_dat %>% 
    ggplot2::ggplot(
      ggplot2::aes(x = {{fc}},
                   y = -log10({{p.value}})
                   ))+
    ggplot2::geom_point(color = 'grey50',
               size = 3,
               alpha = 0.7)+
    ggplot2::geom_point(data = volcano_dat %>%
                 dplyr::filter(is.na({{pathway_var}})==F),
               ggplot2::aes(color = {{pathway_var}}),
               size = 3)+
    ggplot2::geom_vline(xintercept=0,
               lty=2,
               col="black",
               lwd=0.5) +
    ggplot2::geom_hline(yintercept = 1.30103,
               lty = 2,
               col = "black",
               lwd = 0.5) +
    ggrepel::geom_label_repel(data = volcano_dat %>% 
                       dplyr::filter(is.na({{pathway_var}})==F ),
                     ggplot2::aes(label = {{protein_var}},
                         color = {{pathway_var}}),
                     size = 5,
                     max.overlaps = 15,
                     segment.color = 'transparent',
                     box.padding = 0.1,
                     show.legend = F)+
    ggplot2::labs(x = xlab,
         y = ylab,
         tag = paste0(nrow(volcano_dat %>% 
                             dplyr::filter(!is.na({{pathway_var}}))),
                      " Significant Proteins in Pathways"))+
    ggplot2::ggtitle(title)+
    ggplot2::theme_classic()
  
  if(legend == F){
    volcano_path <- volcano_path + 
      ggplot2::theme(legend.position = 'none',
            plot.tag.position = c(0.85, -0.02),
            plot.margin = margin(t = 40,
                                 r = 20,
                                 b = 40,
                                 l = 20,
                                 unit = 'pt'),
            axis.text = element_text(size = 15, 
                                     face = 'bold'),
            plot.tag = element_text(size = 15),
            axis.line = element_line(linewidth = 1),
            plot.title = element_text(size = 20, face = 'bold',
                                      hjust = 0.5),
            legend.title = element_blank(),
            legend.text = element_text(size = 15),
            axis.title.x = element_text(size = 18,
                                       face = 'bold'),
            axis.title.y = element_text(size = 18, 
                                        face = 'bold'))
  } else if(legend == T){
    legend_path <- volcano_path + 
      ggplot2::theme(
        plot.tag.position = c(0.85, -0.02),
        plot.margin = margin(t = 40,
                             r = 20,
                             b = 40,
                             l = 20,
                             unit = 'pt'),
        axis.text = element_text(size = 15, 
                                 face = 'bold'),
        plot.tag = element_text(size = 15),
        axis.line = element_line(linewidth = 1),
        plot.title = element_text(size = 20, face = 'bold',
                                  hjust = 0.5),
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        axis.title.x =element_text(size = 18),
        axis.title.y = element_text(size = 18))
  }
  
  
  if(manual_color == T){
    volcano_path <- volcano_path +
      ggplot2::scale_color_manual(values = color_val)
  } else {
    volcano_path <- volcano_path +
      ggplot2::scale_color_brewer(palette = palette)
  }
  
  if(save_plot == T){
  ggplot2::ggsave(plot = volcano_path,
         filename = paste0(title, "_", "stringdb", '_volcano.jpg'),
         device = "jpg", 
         width = 10, 
         height = 10)
  }
  
  
  return(volcano_path)
}
