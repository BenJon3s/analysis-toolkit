### Percentage of analytes failed and warned per sample for Olink HT data
# just a repetitive task that needed to be written up and saved
level_1_analyte_pct <- function(data, 
                                sample_id, 
                                sample_qc){
  qc_dat <- data %>% 
    group_by({{sample_id}}, 
             {{sample_qc}}) %>% 
    summarise(Percentage = 100*prop.table(n()),
              .groups = 'drop') %>% 
    pivot_wider(names_from = {{sample_qc}}, 
                values_from = Percentage, 
                id_cols = {{sample_id}},
                values_fill = 0) %>%
    rename("Analyte_Failure_Pct" = "FAIL", 
           "Analyte_Warn_Pct" = "WARN") %>% 
    select({{sample_id}}, Analyte_Failure_Pct, Analyte_Warn_Pct)
}