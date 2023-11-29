#Use this function to create the base table style for the EA gt table


ea_style<-function(df){
  df%>%
    gt()%>%
  fmt_percent(
    columns = tidyselect::contains("_execution_"),
    decimals = 0)%>%
  fmt_currency( # add dolar signs
    columns = tidyselect::contains("_budget_total"),
    decimals = 0,
    currency = "USD")%>%
    fmt_currency( # add dolar signs
      columns = tidyselect::contains("expenditure_amt"),
      decimals = 0,
      currency = "USD")%>%
  tab_options(
    table.font.names = "Source Sans Pro"
  ) %>% 
    sub_missing(columns = everything(),
                missing_text = "-") %>%
  cols_width(
    everything() ~ px(90))%>%
  
  tab_style(
    style = cell_borders(
      sides = "all",
      weight = px(1),
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
    label = glue("COP{stringr::str_sub(fy_prev, -2)} Performance"),
    columns = tidyselect::matches(as.character(fy_beg)))%>%
  tab_spanner(
    label = glue("COP{stringr::str_sub(fy_beg, -2)} Performance"), #glue::glue("COP{stringr::str_sub(fy_beg, -2)} Performance")
    columns = tidyselect::matches(as.character(fy_end)))%>%
  gt::tab_style(
    style = list(
      gt::cell_text(weight = "bold")), 
    locations = gt::cells_column_spanners(spanners = tidyselect::everything())
  )%>%
  cols_align(
    align = "center",
    columns = everything())%>%
   cols_label(
    budget_execution_2022="Budget Execution",
    expenditure_amt_2022 = "Expenditure",
    cop_budget_total_2022 = "Budget",
     budget_execution_2023="Budget Execution",
    expenditure_amt_2023 = "Expenditure",
    cop_budget_total_2023 = "Budget",)%>%
  cols_align(
    align = "left",
    columns = 1
  )%>%


  tab_style(style = cell_fill(color = "#5bb5d5",alpha = .75),
            locations = cells_body(
              columns = (budget_execution_2022),
              rows = (budget_execution_2022) >= 0.9 & (budget_execution_2022) < 1.1)) %>%
  tab_style(style = cell_fill(color = "#ffcaa2",alpha = .75),
            locations = cells_body(
              columns = (budget_execution_2022),
              rows =(budget_execution_2022) < 0.9 ))%>%
  tab_style(style = cell_fill(color = "#ffcaa2",alpha = .75),
            locations = cells_body(
              columns = (budget_execution_2022),
              rows = (budget_execution_2022)>= 1.1 & (budget_execution_2022) < 1.2))%>%
  tab_style(style = cell_fill(color = "#ff989f",alpha = .75),
            locations = cells_body(
              columns = (budget_execution_2022),
              rows = (budget_execution_2022) >= 1.2 ))%>%
    tab_style(style = cell_fill(color = "#5bb5d5",alpha = .75),
              locations = cells_body(
                columns = (budget_execution_2023),
                rows = (budget_execution_2023) >= 0.9 & (budget_execution_2023) < 1.1)) %>%
    tab_style(style = cell_fill(color = "#ffcaa2",alpha = .75),
              locations = cells_body(
                columns = (budget_execution_2023),
                rows =(budget_execution_2023) < 0.9 ))%>%
    tab_style(style = cell_fill(color = "#ffcaa2",alpha = .75),
              locations = cells_body(
                columns = (budget_execution_2023),
                rows = (budget_execution_2023)>= 1.1 & (budget_execution_2023) < 1.2))%>%
    tab_style(style = cell_fill(color = "#ff989f",alpha = .75),
              locations = cells_body(
                columns = (budget_execution_2023),
                rows = (budget_execution_2023) >= 1.2 ))%>%
  
  gt::tab_options(
    source_notes.font.size = 8,
    table.font.size = 13, 
    data_row.padding = gt::px(5),
    source_notes.padding = gt::px(1),) %>%
  
  tab_footnote(
    footnote = "Excluding M&O",
    locations = cells_column_labels(
      columns = tidyselect::contains("_execution_")))%>%
    opt_table_outline()%>%
    
    opt_row_striping(row_striping = TRUE)%>%
  gt::tab_source_note(
    source_note = gt::md(glue::glue("**Source**: {source}. Ukraine has been excluded from this analysis | Please reach out to oha.ea@usaid.gov for questions."))
  )

}
