#### Spaghetti Plot for general longitudinal plotting ####\
#' @importFrom dplyr %>%

library(ggplot2)
library(dplyr)
library(zoo)

#### 1 Protein - faceted by individual ####
# default start is window size = 3, can be adjusted as necessary
# Could use either 
moving_spaghetti <- function(data,
                             subject_ref, 
                             time, 
                             window_size = 3,
                             Protein, 
                             palette = 'Dark2',
                             Group){
  
  
  mov_avg_data <- data %>% 
    dplyr::arrange({{subject_ref}}, {{time}}) %>% 
    dplyr::group_by({{subject_ref}}) %>% 
    dplyr::mutate(MovingAverage = 
                    zoo::rollapply({{Protein}}, 
                                   width = window_size,
                                   FUN = mean, 
                                   align = "right", ## Revisit
                                   partial = T))
  
  plot <- mov_avg_data %>% 
    ggplot2::ggplot(ggplot2::aes(x = {{time}}, 
               y = MovingAverage, 
               color = {{Group}}))+
    ggplot2::geom_point()+
    ggplot2:geom_line(lwd = 1)+
    ggplot2::theme_classic()+
    ggplot2::xlab(dplyr::enquo(time))+
    ggplot2::ylab(dplyr::enquo(Protein))+
    ggplot2::scale_color_brewer(palette = palette)+
    ggplot2::theme(legend.position = "top")+
    ggplot2::facet_wrap(.~{{subject_ref}})
  
  return(plot)
  
  
}

moving_spaghetti(data = test_plot_dat, 
                 Protein = IFNA1_IFNA13, 
                 Group = treament_arm,
                 window_size = 3)




comprehensive_spaghetti <- function(data,
                                    window_size = 3,
                                    Protein,
                                    Group) {
  av_data <- data %>%
    dplyr::arrange(subject_ref, baseline_visit) %>%
    dplyr::group_by(subject_ref) %>%
    dplyr::mutate(
      MovingAverage =
        zoo::rollapply({
          {
            Protein
          }
        },
        width = window_size,
        FUN = mean,
        align = "right",
        partial = T)
    )
  
  plot <- av_data %>%
    ggplot(aes(x = baseline_visit,
               y = MovingAverage,
               color = {{Group}})) +
    geom_line(lwd = 1) +
    theme_classic() +
    xlab('baseline_scale') +
    ylab(enquo(Protein)) +
    scale_color_manual(values = c("#216f1d", "#91211b")) +
    theme(legend.position = "top") +
    facet_wrap(. ~ subject_ref)
  
  
  plot <- av_data %>% 
    ggplot(aes(x = baseline_visit, 
               y = MovingAverage, 
               color = {{Group}}))+
    geom_line(aes(group = subject_ref), 
              lwd = 0.4,
              linetype = "dashed")+
    geom_smooth(method = "loess",
                fill = NA,
                lwd = 1)+
    theme_classic() +
    xlab('baseline_scale') +
    ylab(enquo(Protein)) +
    scale_color_brewer(palette = "Dark2")+
    theme(legend.position = "top")
  
  plot
  
  
}

test_save <- comprehensive_spaghetti(data = test_plot_dat, 
                                     Protein = IFNA1_IFNA13, 
                                     Group = treament_arm,
                                     window_size = 3)

ggsave(plot = test_save, 
       filename = paste0("IFNA1_IFNA13", ".jpg"),
       device = "jpg", 
       width = 8, 
       height = 7)


### Function intended only for per-individual graphs across all proteins for the individual
# moving averages within group vs moving averages within subject

# assumes wide format for data - usually following lcmm 
individual_spaghetti <- function(data, 
                                 window_size = 3,
                                 SubjectID,
                                 Group){
  
  
  
  
  per_ind_dat <- data %>% 
    na.omit() %>% 
    dplyr::filter(subject_ref == SubjectID) %>% 
    dplyr::arrange(number_prot, baseline_visit) %>% 
    dplyr::group_by(number_prot) %>% 
    dplyr::mutate(MovingAverage = 
                    zoo::rollapply(LogProtExp_Raw, 
                                   width = window_size,
                                   FUN = mean, 
                                   align = "right", 
                                   partial = T)) %>% 
    dplyr::ungroup() %>% 
    dplyr::arrange({{Group}}, 
                   baseline_visit) %>% 
    dplyr::group_by({{Group}}) %>% 
    dplyr::mutate(Group_Moving = 
                    zoo::rollapply(
                      LogProtExp_Raw,
                      width = window_size,
                      FUN = mean,
                      align = "right", 
                      partial = T
                    )) %>% 
    dplyr::ungroup()
  
  
  per_ind_dat %>% 
    na.omit() %>% 
    ggplot(aes(x = baseline_visit, 
               color = {{Group}}))+
    # geom_line(aes(group = number_prot,
    #               y = MovingAverage), lwd = 0.5)+
    geom_line(aes(group = {{Group}}, 
                  y = Group_Moving),
              lwd = 1
    )+
    theme_classic()
  
}