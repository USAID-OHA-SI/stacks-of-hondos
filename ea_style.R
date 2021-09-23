library(gt)
library(tidyverse)

ea_style<-function(gt_obj){
    gt_obj%>%
  fmt_percent(
    columns = c(`budget_execution_2020`, `budget_execution_2021`),
    decimals = 0)%>%
  fmt_currency( # add dolar signs
    columns = c(`cop_budget_total_2020`,`expenditure_amt_2020`,`cop_budget_total_2021`,`expenditure_amt_2021` ),
    decimals = 0,
    currency = "USD")%>%
  tab_options(
    table.font.names = "Source Sans Pro"
  ) %>% 
  cols_width(
    everything() ~ px(90))%>%
  
  tab_style(
    style = cell_borders(
      sides = "right",
      weight = px(1.5),
    ),
    locations = cells_body(
      columns = everything(),
      rows = everything()
    ))%>%
  tab_style(
    style = cell_text(weight = 700),
    locations = cells_body(
      columns = tidyselect::contains("_execution_")
    )
  )%>%
  tab_spanner(
    label = "COP21 Performance",
    columns = c(
      expenditure_amt_2021,cop_budget_total_2021, budget_execution_2021,))%>%
  tab_spanner(
    label = "COP20 Performance",
    columns = c(
      expenditure_amt_2020,cop_budget_total_2020,budget_execution_2020))%>%
  gt::tab_style(
    style = list(
      gt::cell_text(weight = "bold")), 
    locations = gt::cells_column_spanners(spanners = tidyselect::everything())
  )%>%
  cols_align(
    align = "center",
    columns = everything()
  )%>%
  cols_align(
    align = "left",
    columns = tidyselect::contains("agency")
  )%>%
  tab_style(style = cell_fill(color = "#5bb5d5",alpha = .75),      
            locations = cells_body(               
              columns = (budget_execution_2020),
              rows = (budget_execution_2020) >= 0.9 & (budget_execution_2020) < 1.1)) %>%
  tab_style(style = cell_fill(color = "#ffcaa2",alpha = .75),      
            locations = cells_body(               
              columns = (budget_execution_2020),
              rows =(budget_execution_2020) < 0.9 ))%>%
  tab_style(style = cell_fill(color = "#ffcaa2",alpha = .75),      
            locations = cells_body(               
              columns = (budget_execution_2020),
              rows = (budget_execution_2020)>= 1.1 & (budget_execution_2020) < 1.2))%>%
  tab_style(style = cell_fill(color = "#ff989f",alpha = .75),      
            locations = cells_body(               
              columns = (budget_execution_2020),
              rows = (budget_execution_2020) >= 1.2 ))%>%
  
  tab_style(style = cell_fill(color = "#5bb5d5",alpha = .75),      
            locations = cells_body(               
              columns = (budget_execution_2021),
              rows = (budget_execution_2021) >= 0.9 & (budget_execution_2021) < 1.1)) %>%
  tab_style(style = cell_fill(color = "#ffcaa2",alpha = .75),      
            locations = cells_body(               
              columns = (budget_execution_2021),
              rows =(budget_execution_2021) < 0.9 ))%>%
  tab_style(style = cell_fill(color = "#ffcaa2",alpha = .75),      
            locations = cells_body(               
              columns = (budget_execution_2021),
              rows = (budget_execution_2021)>= 1.1 & (budget_execution_2021) < 1.2))%>%
  tab_style(style = cell_fill(color = "#ff989f",alpha = .75),      
            locations = cells_body(               
              columns = (budget_execution_2021),
              rows = (budget_execution_2021) >= 1.2 ))%>%
  
  gt::tab_options(
    source_notes.font.size = 8,
    table.font.size = 13, 
    data_row.padding = gt::px(5),
    source_notes.padding = gt::px(1),) %>%
  
  tab_footnote(
    footnote = "Excluding M&O",
    locations = cells_column_labels(
      columns =c(expenditure_amt_2020, expenditure_amt_2021)))%>%
  gt::tab_source_note(
    source_note = gt::md(glue::glue("**Source**: {source} | Please reach out to oha.ea@usaid.gov for questions"))
  )

}