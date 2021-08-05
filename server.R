server = function(input, output, session) {

  ######################## Settings Related Options ################################
  options(shiny.maxRequestSize=30*1024^2) # Update max request size for RStudio connect to 30 MB per file uploaded in fileInputs
  options(readr.num_columns = 0) # to disable from printing column specs for readr-based tables
  options(readxl.num_columns = 0)
  VM = "10.29.128.4" # airflow VM
  stats_file <- "all_stats.csv" # file name to search when complete
  pollTimer <- 15000 # poll every 15 seconds

  datalogs = file.path(dirname(datadump), "logs", "r_dag", "omiq_pipeline")

  ######################## Inputted files - need to be used as "pin" in RStudio Connect ################################
  BV421_DF <- readxl::read_xlsx("bv421_test.xlsx") # copy of BV421 DB Excel being referenced (temporary)
  BV421_DF <- BV421_DF %>% add_row(Clone = "NA", .before = 1) # add row of NA at the top to set default value if no clone selected
  # print(BV421_DF_2)
  units_list <- unique(readr::read_csv('fixed_lists.csv')$units) # list of units for units dropdowns (only used in rhandsontable right now)
  clones <- c(as.character(BV421_DF$Clone)) # list of clones for clone dropdowns
  rows <- c('NA', 'A', 'B','C','D','E','F','G','H', plate_config_wells()$Well.ID) # list of rows for gating control dropdown
  default_fcs_filenames <- readr::read_csv('fixed_lists.csv')$`Default FCS Filenames`
  reactive_vals <- reactiveValues() #reactive values to hold 3 metadata values from fcs files

  ################################# ------------ PLATE LAYOUT TAB --------- ###################################################

  ######################## Sidebar panel render UIs based on experiment type selected ################################

  ## If Stability, render UI for selecting time point and single clone name
  output$stability_ui_time_clone <- renderUI({
    if(input$experiment_type == "S"){
      div(style= "display: grid; grid-template-columns: 150px 170px; grid-template-rows: auto; padding-left: 5px;",
          div(style = "text-align: left; grid-column: 1; grid-row: 1;",
              selectInput('time_study', 'Time Study (yrs)*', choices=c(2,5), selected=5)
          ),
          div(style = "text-align: left; grid-column: 2; grid-row: 1; padding-left: 10px",
              selectizeInput('clone_all', 'Clone*', choices = clones, selected = "104", options = list(create = TRUE))
          )
      )
    }

  })


  ## List of stability time points; dependent on time study selected (2 or 5 years)
  stability_time_point_list <- reactive({
    if (input$experiment_type == "S") {
      req(input$time_study)
      if (as.numeric(input$time_study) == 5) {
        df <-
          data.frame(letter = LETTERS[1:8],
                     time = c(0, 0.5, 1, 1.5, 2, 3, 4, 5))
      }
      else if (as.numeric(input$time_study) == 2) {
        df <-
          data.frame(letter = LETTERS[1:8],
                     time = c(0, 0.5, 1, 1.5, 2, NA, NA, NA))
      }
      return(df)
    }
    else{
      return(NULL)
    }
    return(data.frame(letter = LETTERS[1:8], time = rep(0, 8)))
  })

  ## Change selectInput dropdowns on sidebar based on experiment type
  ## If stability, time points; If OptiBuild, clones
  output$sidebar_selectInput_items <- renderUI({
    if(input$experiment_type == "S"){
      tagList(
        fluidRow(
          div(style = "display: grid; grid-template-columns: auto; padding-left: 5px;",
              div(style = "text-align: center;",
                  p(strong(h3('Time (years)*')))
              )
          )
        ),
        lapply(LETTERS[1:8], function(i){
          div(style = "display: grid; grid-template-columns: 150px 250px; padding-left: 10px; max-height: 60px;",
              div(style = "padding: 5px; text-align: center;",
                  p(strong(h3(paste0('Row ',i))))
              ),
              div(style = "padding: 5px; text-align: center;",
                  selectInput(
                    paste0('time_', i),
                    label = '',
                    choices = c(stability_time_point_list()$time),
                    selected = stability_time_point_list()$time[stability_time_point_list()$letter == i])
              )
          )
        })
      )
    }
    else if(input$experiment_type == "OB"){
      tagList(
        fluidRow(
          div(style = "display: grid; grid-template-columns: auto; padding-left: 5px;",
              div(style = "text-align: center;",
                  p(strong(h3('Clone*'))))
          )
        ),
        lapply(LETTERS[1:8], function(i){
          div(style = "display: grid; grid-template-columns: 150px 120px 180px; padding-left: 10px; max-height: 80px;",
              div(style = "padding: 5px; text-align: center;",
                  p(strong(h3(paste0('Row ',i))))
              ),
              div(style = "padding: 5px; text-align: center;",
                  radioButtons(paste0('sample_or_control_', i), label = '', choices = c('Sample', 'Control'), selected = 'Sample')
              ),
              div(style = "padding: 5px; text-align: left;",
                  selectizeInput(paste0('clone_', i), label = '', choices = clones, selected = "1041", options=list(create=TRUE))
              )
          )
        })
      )
    }
  })

  ######################## Main panel render UIs based on experiment type selected ################################
  output$titration_ui <- renderUI({
    fluidRow(
      wellPanel(width = 12, status = "success", solidHeader = TRUE, title = "Titrations",
                div(style = 'overflow-x: scroll;',
                    radioButtons("num_titrations", "# of Titrations*", choices=c(1,2,3,4,5,6,7,8), inline=TRUE, selected=7),
                    fluidRow(
                      column(3, radioButtons('titration_order', 'Titration Order',
                                             choices=c('high-to-low', 'low-to-high'),
                                             inline=TRUE, selected='low-to-high')),
                      column(3, radioButtons('units', 'Units',choices=c('ug/test', 'ng/test'), inline=TRUE, selected="ug/test"))),
                    actionButton("auto_divide", "Autofill Titrations", class = "btn-primary", style="color: #fff"),
                    actionButton("reset_titr", "Reset", class = "btn-primary", icon("undo-alt"), style="color: #fff"),
                    fluidRow(
                      lapply(1:8, function(i){
                        div(style = 'max-width: 200px; display: table-cell; padding-left: 15px;',
                            numericInput(paste0('titrate_',i), '', value = NULL, width = '150px'))
                      })

                    ),
                    uiOutput('titrations_missing'),
                    p(em(h6('Titrations starting at column 2 (1st column is unstained)')))

                )
      )
    )
  })

  ######################## Titration-related server-side values and variables ################################

  ## Convert values in auto-fill to appropriate units based on units selected
  unit_conversion <- reactive({ ifelse(input$units=='ug/test',1,1000) })

  ## If "Autofill Titrations" selected, will autofill fields based on # of titrations, order and units selected
  observeEvent(input$auto_divide, {
    if(input$titration_order == 'high-to-low'){
      if(input$num_titrations == 1){
        updateNumericInput(session, 'titrate_1', value = 2*as.numeric(unit_conversion()))
        lapply(2:8, function(i){
          updateNumericInput(session, paste0('titrate_',i), value = NA)
        })
      }
      else if(input$num_titrations == 2){
        updateNumericInput(session, 'titrate_1', value = 2*as.numeric(unit_conversion()))
        updateNumericInput(session, 'titrate_2', value = 1*as.numeric(unit_conversion()))
        lapply(3:8, function(i){
          updateNumericInput(session, paste0('titrate_',i), value = NA)
        })
      }
      else if(input$num_titrations == 3){
        updateNumericInput(session, 'titrate_1', value = 2*as.numeric(unit_conversion()))
        updateNumericInput(session, 'titrate_2', value = 1*as.numeric(unit_conversion()))
        updateNumericInput(session, 'titrate_3', value = 0.5*as.numeric(unit_conversion()))
        lapply(4:8, function(i){
          updateNumericInput(session, paste0('titrate_',i), value = NA)
        })
      }
      else if(input$num_titrations == 4){
        updateNumericInput(session, 'titrate_1', value = 2*as.numeric(unit_conversion()))
        updateNumericInput(session, 'titrate_2', value = 1*as.numeric(unit_conversion()))
        updateNumericInput(session, 'titrate_3', value = 0.5*as.numeric(unit_conversion()))
        updateNumericInput(session, 'titrate_4', value = 0.25*as.numeric(unit_conversion()))
        lapply(5:8, function(i){
          updateNumericInput(session, paste0('titrate_',i), value = NA)
        })
      }
      else if(input$num_titrations == 5){
        updateNumericInput(session, 'titrate_1', value = 2*as.numeric(unit_conversion()))
        updateNumericInput(session, 'titrate_2', value = 1*as.numeric(unit_conversion()))
        updateNumericInput(session, 'titrate_3', value = 0.5*as.numeric(unit_conversion()))
        updateNumericInput(session, 'titrate_4', value = 0.25*as.numeric(unit_conversion()))
        updateNumericInput(session, 'titrate_5', value = 0.125*as.numeric(unit_conversion()))
        lapply(6:8, function(i){
          updateNumericInput(session, paste0('titrate_',i), value = NA)
        })
      }
      else if(input$num_titrations == 6){
        updateNumericInput(session, 'titrate_1', value = 2*as.numeric(unit_conversion()))
        updateNumericInput(session, 'titrate_2', value = 1*as.numeric(unit_conversion()))
        updateNumericInput(session, 'titrate_3', value = 0.5*as.numeric(unit_conversion()))
        updateNumericInput(session, 'titrate_4', value = 0.25*as.numeric(unit_conversion()))
        updateNumericInput(session, 'titrate_5', value = 0.125*as.numeric(unit_conversion()))
        updateNumericInput(session, 'titrate_6', value = 0.06*as.numeric(unit_conversion()))
        lapply(7:8, function(i){
          updateNumericInput(session, paste0('titrate_',i), value = NA)
        })
      }
      else if(input$num_titrations == 7){
        updateNumericInput(session, 'titrate_1', value = 2*as.numeric(unit_conversion()))
        updateNumericInput(session, 'titrate_2', value = 1*as.numeric(unit_conversion()))
        updateNumericInput(session, 'titrate_3', value = 0.5*as.numeric(unit_conversion()))
        updateNumericInput(session, 'titrate_4', value = 0.25*as.numeric(unit_conversion()))
        updateNumericInput(session, 'titrate_5', value = 0.125*as.numeric(unit_conversion()))
        updateNumericInput(session, 'titrate_6', value = 0.06*as.numeric(unit_conversion()))
        updateNumericInput(session, 'titrate_7', value = 0.03*as.numeric(unit_conversion()))
        updateNumericInput(session, 'titrate_8', value = NA)
      }
      else if(input$num_titrations == 8){
        updateNumericInput(session, 'titrate_1', value = 2*as.numeric(unit_conversion()))
        updateNumericInput(session, 'titrate_2', value = 1*as.numeric(unit_conversion()))
        updateNumericInput(session, 'titrate_3', value = 0.5*as.numeric(unit_conversion()))
        updateNumericInput(session, 'titrate_4', value = 0.25*as.numeric(unit_conversion()))
        updateNumericInput(session, 'titrate_5', value = 0.125*as.numeric(unit_conversion()))
        updateNumericInput(session, 'titrate_6', value = 0.06*as.numeric(unit_conversion()))
        updateNumericInput(session, 'titrate_7', value = 0.03*as.numeric(unit_conversion()))
        updateNumericInput(session, 'titrate_8', value = 0.015*as.numeric(unit_conversion()))
      }

    }
    else{
      if(input$num_titrations == 1){
        updateNumericInput(session, 'titrate_1', value = 2*as.numeric(unit_conversion()))
        lapply(2:8, function(i){
          updateNumericInput(session, paste0('titrate_',i), value = NA)
        })
      }
      else if(input$num_titrations == 2){
        updateNumericInput(session, 'titrate_1', value = 1*as.numeric(unit_conversion()))
        updateNumericInput(session, 'titrate_2', value = 2*as.numeric(unit_conversion()))
        lapply(3:8, function(i){
          updateNumericInput(session, paste0('titrate_',i), value = NA)
        })
      }
      else if(input$num_titrations == 3){
        updateNumericInput(session, 'titrate_1', value = 0.5*as.numeric(unit_conversion()))
        updateNumericInput(session, 'titrate_2', value = 1*as.numeric(unit_conversion()))
        updateNumericInput(session, 'titrate_3', value = 2*as.numeric(unit_conversion()))
        lapply(4:8, function(i){
          updateNumericInput(session, paste0('titrate_',i), value = NA)
        })
      }
      else if(input$num_titrations == 4){
        updateNumericInput(session, 'titrate_1', value = 0.25*as.numeric(unit_conversion()))
        updateNumericInput(session, 'titrate_2', value = 0.5*as.numeric(unit_conversion()))
        updateNumericInput(session, 'titrate_3', value = 1*as.numeric(unit_conversion()))
        updateNumericInput(session, 'titrate_4', value = 2*as.numeric(unit_conversion()))
        lapply(5:8, function(i){
          updateNumericInput(session, paste0('titrate_',i), value = NA)
        })
      }
      else if(input$num_titrations == 5){
        updateNumericInput(session, 'titrate_1', value = 0.125*as.numeric(unit_conversion()))
        updateNumericInput(session, 'titrate_2', value = 0.25*as.numeric(unit_conversion()))
        updateNumericInput(session, 'titrate_3', value = 0.5*as.numeric(unit_conversion()))
        updateNumericInput(session, 'titrate_4', value = 1*as.numeric(unit_conversion()))
        updateNumericInput(session, 'titrate_5', value = 2*as.numeric(unit_conversion()))
        lapply(6:8, function(i){
          updateNumericInput(session, paste0('titrate_',i), value = NA)
        })
      }
      else if(input$num_titrations == 6){
        updateNumericInput(session, 'titrate_1', value = 0.06*as.numeric(unit_conversion()))
        updateNumericInput(session, 'titrate_2', value = 0.125*as.numeric(unit_conversion()))
        updateNumericInput(session, 'titrate_3', value = 0.25*as.numeric(unit_conversion()))
        updateNumericInput(session, 'titrate_4', value = 0.5*as.numeric(unit_conversion()))
        updateNumericInput(session, 'titrate_5', value = 1*as.numeric(unit_conversion()))
        updateNumericInput(session, 'titrate_6', value = 2*as.numeric(unit_conversion()))
        lapply(7:8, function(i){
          updateNumericInput(session, paste0('titrate_',i), value = NA)
        })
      }
      else if(input$num_titrations == 7){
        updateNumericInput(session, 'titrate_1', value = 0.03*as.numeric(unit_conversion()))
        updateNumericInput(session, 'titrate_2', value = 0.06*as.numeric(unit_conversion()))
        updateNumericInput(session, 'titrate_3', value = 0.125*as.numeric(unit_conversion()))
        updateNumericInput(session, 'titrate_4', value = 0.25*as.numeric(unit_conversion()))
        updateNumericInput(session, 'titrate_5', value = 0.5*as.numeric(unit_conversion()))
        updateNumericInput(session, 'titrate_6', value = 1*as.numeric(unit_conversion()))
        updateNumericInput(session, 'titrate_7', value = 2*as.numeric(unit_conversion()))
        updateNumericInput(session, 'titrate_8', value = NA)
      }
      else if(input$num_titrations == 8){
        updateNumericInput(session, 'titrate_1', value = 0.015*as.numeric(unit_conversion()))
        updateNumericInput(session, 'titrate_2', value = 0.03*as.numeric(unit_conversion()))
        updateNumericInput(session, 'titrate_3', value = 0.06*as.numeric(unit_conversion()))
        updateNumericInput(session, 'titrate_4', value = 0.125*as.numeric(unit_conversion()))
        updateNumericInput(session, 'titrate_5', value = 0.25*as.numeric(unit_conversion()))
        updateNumericInput(session, 'titrate_6', value = 0.5*as.numeric(unit_conversion()))
        updateNumericInput(session, 'titrate_7', value = 1*as.numeric(unit_conversion()))
        updateNumericInput(session, 'titrate_8', value = 2*as.numeric(unit_conversion()))
      }

    }
  })

  ## If "Reset" clicked, will erase all titrations inputted in UI
  observeEvent(input$reset_titr, {
    updateNumericInput(session, 'titrate_1', value = NA)
    updateNumericInput(session, 'titrate_2', value = NA)
    updateNumericInput(session, 'titrate_3', value = NA)
    updateNumericInput(session, 'titrate_4', value = NA)
    updateNumericInput(session, 'titrate_5', value = NA)
    updateNumericInput(session, 'titrate_6', value = NA)
    updateNumericInput(session, 'titrate_7', value = NA)
    updateNumericInput(session, 'titrate_8', value = NA)
  })

  ## Outputted list of titrations inputted or selected
  titrationsList <- reactive({
    c('NA',
      input$titrate_1,
      input$titrate_2,
      input$titrate_3,
      input$titrate_4,
      input$titrate_5,
      input$titrate_6,
      input$titrate_7,
      input$titrate_8
    )
  })

  ####### ExcelR table renderUI for Stability and OptiBuild experiments
  output$excel_table_ui <- renderUI({
    if(input$experiment_type == "S" | input$experiment_type == "OB"){
      fluidRow(
        div(style = 'overflow-x: scroll;',
            excelOutput("plate_layout_table_excelR", width = 'auto', height = 'auto')
        )
      )
    }

  })

  ####### Main Panel renderUI for all experiment types
  output$main_panel_ui <- renderUI({
    if(input$experiment_type == "S" | input$experiment_type == "OB"){
      tabsetPanel(id="tabs",
        tabPanel("Set Up Plate", id="setup_tab", # For inital plate set-up; includes titrations and main table of plate layout
           uiOutput('titration_ui'), # Titrations well panel
           h4(p(strong(class = "text-primary", "Fill in any remaining fields in the table below."))),
           br(),
           uiOutput('excel_table_ui'), # Main plate layout table
           br(),
           fluidRow(wellPanel(width = 12, div(
               style = 'overflow-x: scroll;', dataTableOutput('preview_metadata_outline_table')
           ))) #When "Preview Table" Clicked
          ),
          ## Set up custom gating controls - currently not configured
        tabPanel('Set Up Gating Controls', id="gating_tab",
            conditionalPanel(
                "input.experiment_type == 'S' ",
                wellPanel(
                  uiOutput('ui_gating'),
                  checkboxInput('use_custom_gating', "Check box if prefer to use custom gating controls from above",value = FALSE)
                )
            )
        )
      )
    }
    else if(input$experiment_type == "LEGO"){
      tabsetPanel(
        id = "lego_reagents",
        tabPanel(title = "Reagent 1", id = "reag1",
                 wellPanel(
                   clone_lego_ui("reag_1", "reag_1",  "reag_1",  "reag_1"),
                   br(),
                   antibody_table_ui("reag_1"),
                   save_ab_iso_table_ui('reag1_ab'),
                   br(),
                   isotype_table_ui("reag_1"),
                   save_ab_iso_table_ui('reag1_iso'),
                   br(),
                   titr_ui("reag_1")
                 )
        ),
        tabPanel(title = "Reagent 2", id = "reag2",
                 wellPanel(
                   clone_lego_ui("reag_2", "reag_2", "reag_2", "reag_2"),
                   br(),
                   antibody_table_ui("reag_2"),
                   save_ab_iso_table_ui('reag2_ab'),
                   br(),
                   isotype_table_ui("reag_2"),
                   save_ab_iso_table_ui('reag2_iso'),
                   br(),
                   titr_ui("reag_2")
                 )
        ),
        tabPanel(title = "Reagent 3", id = "reag3",
                 wellPanel(
                   clone_lego_ui("reag_3", "reag_3", "reag_3", "reag_3"),
                   br(),
                   antibody_table_ui("reag_3"),
                   save_ab_iso_table_ui('reag3_ab'),
                   br(),
                   isotype_table_ui("reag_3"),
                   save_ab_iso_table_ui('reag3_iso'),
                   br(),
                   titr_ui("reag_3")
                 )
        ),
        tabPanel(title = "Reagent 4", id = "reag4",
                 wellPanel(
                   clone_lego_ui("reag_4", "reag_4", "reag_4", "reag_4"),
                   br(),
                   antibody_table_ui("reag_4"),
                   save_ab_iso_table_ui('reag4_ab'),
                   br(),
                   isotype_table_ui("reag_4"),
                   save_ab_iso_table_ui('reag4_iso'),
                   br(),
                   titr_ui("reag_4")
                 )
        )
      )
    }
  })

  ############################ GENERAL FUNCTIONS AND EXPRESSIONS (EXPERIMENT-INDEPENDENT) ####################

  ## Pull Spec ranges based on sample type and sample names selected
  pull_spec_data <- reactive({ find_spec_data(BV421_DF, input$sample_type, input$spec1_name, input$spec2_name, input$spec3_name) })

  ## Metadata most basic outline with metadata headers and well IDs, Plate Columns and Plate Rows only
  metadata_headers_and_wellids <- reactive({ metadata_outline('metadata_headers.csv', plate_config_wells())  })


  ############################### LEGO EXPERIMENTS #####################################

  ##################### LEGO Server Modules for excelR tables ######################
  ## Reagent 1 ##

  # Clones for test antibody, reference A antibody, and reference B antibody
  lego_test_clone_1 <- test_clone_server("reag_1")
  lego_ref_a_clone_1 <- ref_a_clone_server("reag_1", lego_test_clone_1)
  lego_ref_b_clone_1 <- ref_b_clone_server("reag_1", lego_test_clone_1)

  # Tables for UI
  lego_table_from_module_ab_reag1 <- antibody_table_server("reag_1", 1, reagent_1_plate_data) # Antibody table
  lego_table_from_module_iso_reag1 <- isotype_table_server("reag_1", 1) # Isotype table
  titr_list_lego_1 <- titr_server("reag_1") # Titrations

  # Save buttons for each table
  reag1_ab_saved <- save_ab_iso_table_server('reag1_ab')
  reag1_iso_saved <- save_ab_iso_table_server('reag1_iso')

  # Data saved from antibody table and converted to R object
  saved_plate_per_row_data_lego_ab_reag1 <- eventReactive(reag1_ab_saved(), {
    data.frame(excel_to_R(lego_table_from_module_ab_reag1()), stringsAsFactors = FALSE)
  })

  # Data saved from isotype table and converted to R object
  saved_plate_per_row_data_lego_iso_reag1 <- eventReactive(reag1_iso_saved(), {
    data.frame(excel_to_R(lego_table_from_module_iso_reag1()), stringsAsFactors = FALSE)
  })



  ## Reagent 2 ##

  # Clones for test antibody, reference A antibody, and reference B antibody
  lego_test_clone_2 <- test_clone_server("reag_2")
  lego_ref_a_clone_2 <- ref_a_clone_server("reag_2", lego_test_clone_2)
  lego_ref_b_clone_2 <- ref_b_clone_server("reag_2", lego_test_clone_2)

  # Tables for UI
  lego_table_from_module_ab_reag2 <- antibody_table_server("reag_2", 2, reagent_2_plate_data) # Antibody table
  lego_table_from_module_iso_reag2 <- isotype_table_server("reag_2", 2) # Isotype table
  titr_list_lego_2 <- titr_server("reag_2") # Titrations

  # Save buttons for each table
  reag2_ab_saved <- save_ab_iso_table_server('reag2_ab')
  reag2_iso_saved <- save_ab_iso_table_server('reag2_iso')

  # Data saved from antibody table and converted to R object
  saved_plate_per_row_data_lego_ab_reag2 <- eventReactive(reag2_ab_saved(), {
    data.frame(excel_to_R(lego_table_from_module_ab_reag2()), stringsAsFactors = FALSE)
  })

  # Data saved from isotype table and converted to R object
  saved_plate_per_row_data_lego_iso_reag2 <- eventReactive(reag2_iso_saved(), {
    data.frame(excel_to_R(lego_table_from_module_iso_reag2()), stringsAsFactors = FALSE)
  })



  ## Reagent 3 ##

  # Clones for test antibody, reference A antibody, and reference B antibody
  lego_test_clone_3 <- test_clone_server("reag_3")
  lego_ref_a_clone_3 <- ref_a_clone_server("reag_3", lego_test_clone_3)
  lego_ref_b_clone_3 <- ref_b_clone_server("reag_3", lego_test_clone_3)

  # Tables for UI
  lego_table_from_module_ab_reag3 <- antibody_table_server("reag_3", 3, reagent_3_plate_data)
  lego_table_from_module_iso_reag3 <- isotype_table_server("reag_3", 3)
  titr_list_lego_3 <- titr_server("reag_3")

  # Save buttons for each table
  reag3_ab_saved <- save_ab_iso_table_server('reag3_ab')
  reag3_iso_saved <- save_ab_iso_table_server('reag3_iso')

  # Data saved from antibody table and converted to R object
  saved_plate_per_row_data_lego_ab_reag3 <- eventReactive(reag3_ab_saved(), {
    data.frame(excel_to_R(lego_table_from_module_ab_reag3()), stringsAsFactors = FALSE)
  })

  # Data saved from isotype table and converted to R object
  saved_plate_per_row_data_lego_iso_reag3 <- eventReactive(reag3_iso_saved(), {
    data.frame(excel_to_R(lego_table_from_module_iso_reag3()), stringsAsFactors = FALSE)
  })

  ## Reagent 4 ##

  # Clones for test antibody, reference A antibody, and reference B antibody
  lego_test_clone_4 <- test_clone_server("reag_4")
  lego_ref_a_clone_4 <- ref_a_clone_server("reag_4", lego_test_clone_4)
  lego_ref_b_clone_4 <- ref_b_clone_server("reag_4", lego_test_clone_4)

  # Tables for UI
  lego_table_from_module_ab_reag4 <- antibody_table_server("reag_4", 4, reagent_4_plate_data)
  lego_table_from_module_iso_reag4 <- isotype_table_server("reag_4", 4)
  titr_list_lego_4 <- titr_server("reag_4")

  # Save buttons for each table
  reag4_ab_saved <- save_ab_iso_table_server('reag4_ab')
  reag4_iso_saved <- save_ab_iso_table_server('reag4_iso')

  # Data saved from antibody table and converted to R object
  saved_plate_per_row_data_lego_ab_reag4 <- eventReactive(reag4_ab_saved(), {
    data.frame(excel_to_R(lego_table_from_module_ab_reag4()), stringsAsFactors = FALSE)
  })

  # Data saved from isotype table and converted to R object
  saved_plate_per_row_data_lego_iso_reag4 <- eventReactive(reag4_iso_saved(), {
    data.frame(excel_to_R(lego_table_from_module_iso_reag4()), stringsAsFactors = FALSE)
  })

  ## "Query" BV421 database based on clones selected and configure metadata for each reagent #
  reagent_1_plate_data <- reactive({ lego_metadata_table(BV421_DF,
                                                         lego_test_clone_1(),
                                                         lego_ref_a_clone_1(),
                                                         lego_ref_b_clone_1(),
                                                         input$spec1_name,
                                                         input$spec2_name,
                                                         input$spec3_name,
                                                         input$sample_type,
                                                         pull_spec_data())
  })

  reagent_2_plate_data <- reactive({ lego_metadata_table(BV421_DF,
                                                         lego_test_clone_2(),
                                                         lego_ref_a_clone_2(),
                                                         lego_ref_b_clone_2(),
                                                         input$spec1_name,
                                                         input$spec2_name,
                                                         input$spec3_name,
                                                         input$sample_type,
                                                         pull_spec_data())
  })

  reagent_3_plate_data <- reactive({ lego_metadata_table(BV421_DF,
                                                         lego_test_clone_3(),
                                                         lego_ref_a_clone_3(),
                                                         lego_ref_b_clone_3(),
                                                         input$spec1_name,
                                                         input$spec2_name,
                                                         input$spec3_name,
                                                         input$sample_type,
                                                         pull_spec_data())
  })

  reagent_4_plate_data <- reactive({ lego_metadata_table(BV421_DF,
                                                         lego_test_clone_4(),
                                                         lego_ref_a_clone_4(),
                                                         lego_ref_b_clone_4(),
                                                         input$spec1_name,
                                                         input$spec2_name,
                                                         input$spec3_name,
                                                         input$sample_type,
                                                         pull_spec_data())
  })

  ## Gather data from user inputs from UI
  plate_data_from_user_inputs <- reactive({

    create_metadata_lego_ui(
      metadata_headers_and_wellids(),
      input$plate_id,
      as.character(input$prep_date),
      # as.character(input$stain_date),
      input$experiment_type,
      input$sample_type,
      input$sample_species,
      input$sample_strain,
      # input$cst_lot_number
    )
  })



  # Create Final metadata outline using data from excelR tables, user inputs, metadata headers/well IDs outline, etc.
  plate_1_metadata_table <- reactive({
      req(saved_plate_per_row_data_lego_ab_reag1())
      req(saved_plate_per_row_data_lego_iso_reag1())

      create_metadata_table_lego(
        saved_plate_per_row_data_lego_ab_reag1(),
        saved_plate_per_row_data_lego_iso_reag1(),
        "1",
        plate_data_from_user_inputs(),
        as.numeric(titr_list_lego_1()[[1]]),
        titr_list_lego_1()[[2]],
        as.numeric(titr_list_lego_1()[[3]])
      )


  })

  plate_2_metadata_table <- reactive({
    req(plate_1_metadata_table())

    create_metadata_table_lego(
      saved_plate_per_row_data_lego_ab_reag2(),
      saved_plate_per_row_data_lego_iso_reag2(),
      "2",
      plate_1_metadata_table(),
      as.numeric(titr_list_lego_2()[[1]]),
      titr_list_lego_2()[[2]],
      as.numeric(titr_list_lego_2()[[3]])
    )


  })

  plate_3_metadata_table <- reactive({
    req(plate_2_metadata_table())

    create_metadata_table_lego(
      saved_plate_per_row_data_lego_ab_reag3(),
      saved_plate_per_row_data_lego_iso_reag3(),
      "3",
      plate_2_metadata_table(),
      as.numeric(titr_list_lego_3()[[1]]),
      titr_list_lego_3()[[2]],
      as.numeric(titr_list_lego_3()[[3]])
    )


  })

  plate_4_metadata_table <- reactive({
    req(plate_3_metadata_table())

    create_metadata_table_lego(
      saved_plate_per_row_data_lego_ab_reag4(),
      saved_plate_per_row_data_lego_iso_reag4(),
      "4",
      plate_3_metadata_table(),
      as.numeric(titr_list_lego_4()[[1]]),
      titr_list_lego_4()[[2]],
      as.numeric(titr_list_lego_4()[[3]])
    )


  })

  ## Apply s prefix to all specs from reagent #4 metadata table (with all reagent data configured)
  final_metadata_from_all_plates_lego <- reactive({
    apply_s_prefix_to_specs(plate_4_metadata_table())
  })

  ########################################## STABILITY/OPTIBUILD EXPERIMENTS #####################################

  ############################ STEP 1: "Query" BV421 DB for data related to selected clone ###################################

  ## Configure plate data per row based on the clone that was selected and the experiment type selected
  plate_data_per_row <- reactive({
    if(input$experiment_type == "OB") {
      validate(
        need(input$clone_A != "", "Wait for OptiBuild template to fully load") # display custom message in need
      )

      clone_A <- ifelse(is.null(input$clone_A) | input$clone_A == "", "NA", input$clone_A)
      clone_B <- ifelse(is.null(input$clone_B) | input$clone_B == "", "NA", input$clone_B)
      clone_C <- ifelse(is.null(input$clone_C) | input$clone_C == "", "NA", input$clone_C)
      clone_D <- ifelse(is.null(input$clone_D) | input$clone_D == "", "NA", input$clone_D)
      clone_E <- ifelse(is.null(input$clone_E) | input$clone_E == "", "NA", input$clone_E)
      clone_F <- ifelse(is.null(input$clone_F) | input$clone_F == "", "NA", input$clone_F)
      clone_G <- ifelse(is.null(input$clone_G) | input$clone_G == "", "NA", input$clone_G)
      clone_H <- ifelse(is.null(input$clone_H) | input$clone_H == "", "NA", input$clone_H)

      optibuild_plate_data <- optibuild_metadata_table(
        BV421_DF,
        clone_A,
        clone_B,
        clone_C,
        clone_D,
        clone_E,
        clone_F,
        clone_G,
        clone_H,
        input$spec1_name,
        input$spec2_name,
        input$spec3_name,
        input$sample_type,
        pull_spec_data(),
        input$sample_or_control_A,
        input$sample_or_control_B,
        input$sample_or_control_C,
        input$sample_or_control_D,
        input$sample_or_control_E,
        input$sample_or_control_F,
        input$sample_or_control_G,
        input$sample_or_control_H
      )

      return(optibuild_plate_data)
    }
    else if(input$experiment_type == "S"){

      # used to wait for renderUI for stability UI to load, otherwise gives temporary error
      validate(
        need(input$time_A != "", "Wait for Stability template to fully load") # display custom message in need
      )
      ## If selected clone is not null, uses whichever clone is selected from dropdown
      if(!is.null(input$clone_all)){
        stability_plate_data <- stability_metadata_table(
          BV421_DF,
          input$clone_all,
          input$time_A,
          input$time_B,
          input$time_C,
          input$time_D,
          input$time_E,
          input$time_F,
          input$time_G,
          input$time_H,
          input$spec1_name,
          input$spec2_name,
          input$spec3_name,
          input$sample_type,
          pull_spec_data()
        )
      }
      ## else if clone is null (either empty or hasn't loaded), will input to function clone = NA
      else{
        stability_plate_data <- stability_metadata_table(
          BV421_DF,
          NA,
          input$time_A,
          input$time_B,
          input$time_C,
          input$time_D,
          input$time_E,
          input$time_F,
          input$time_G,
          input$time_H,
          input$spec1_name,
          input$spec2_name,
          input$spec3_name,
          input$sample_type,
          pull_spec_data()
        )
      }
      return(stability_plate_data)
    }
    else{
      return(NULL)
    }

  })

  ############# STEP 2: Populates excelR table on UI with plate data found from BV421 DB based on clone selected ####################

  ## Get plate data per row from BV421 DB "query"
  plate_data_per_row_excelR_table <- reactive({
    if(input$experiment_type != "LEGO"){
      row_tibble(plate_data_per_row(), input$experiment_type, input$units)
    }
  })

  ## Configure excelR column names and types
  excelR_df_colnames_type <- data.frame(rbind(c('Row', 'text', 0),
                                              c('Gating Control', 'dropdown',0),
                                              c('Stability Time point', 'text',0),
                                              c('Target Species', 'dropdown', 0),
                                              c('Specificity (CD)', 'text', 0),
                                              c('Specificity (non-CD)', 'text', 0),
                                              c('Host Species', 'dropdown', 0),
                                              c('Isotype (Heavy Chain)', 'dropdown', 0),
                                              c('Isotype (Light Chain)', 'dropdown', 0),
                                              c('Clone', 'text', 0),
                                              c('Fluorochrome', 'dropdown', 0),
                                              c('Batch Number', 'text', 0),
                                              c('spec1_name', 'dropdown', 0),
                                              c('spec1_range', 'text', 0),
                                              c('spec1: Pos or Neg?', 'dropdown', 0),
                                              c('spec2_name', 'dropdown', 0),
                                              c('spec2_range', 'text', 0),
                                              c('spec2: Pos or Neg?', 'dropdown', 0),
                                              c('spec3_name', 'dropdown', 0),
                                              c('spec3_range', 'text', 0),
                                              c('spec3: Pos or Neg?', 'dropdown', 0),
                                              c('Gating Method', 'dropdown', 0),
                                              c('Gating Argument', 'text', 0),
                                              c('Optimal', 'text',0),
                                              c('Optimal units', 'dropdown', 0),
                                              c('BV421 Stain Index', 'text',0)
  ))
  names(excelR_df_colnames_type) <- c('title', 'type', 'source')

  ## Configure excelR columns (title, type, and source of dropdowns)
  excelR_columns = data.frame(title = excelR_df_colnames_type$title,
                              type = excelR_df_colnames_type$type,
                              source = I(list(0,
                                              rows,
                                              0, #stability_time_point
                                              target_species_list, #target_species
                                              0, #specificity_cd
                                              0, #specificity_noncd
                                              host_species_list, #host_species
                                              iso_heavy_list, #isotype_heavy
                                              iso_light_list,
                                              0, #clone
                                              fluorochrome, #fluorochrome
                                              0, #batch_number
                                              spec_name_list, #spec1_name
                                              0, #spec1_range
                                              c('Positive','Negative', 'NA'), #spec1: pos or neg
                                              spec_name_list, #spec2_name
                                              0, #spec2_range
                                              c('Positive','Negative', 'NA'), #spec2: pos or neg
                                              spec_name_list, #spec3_name
                                              0, #spec3_range
                                              c('Positive','Negative', 'NA'), #spec3: pos or neg
                                              gating_method_list,
                                              0, #gating_argument
                                              0, #optimal
                                              c("NA", "ug/test", "ng/test", "mg/mL"), #optimal_units
                                              0 #bv421_SI
                              )),
                              width = rep(100,dim(excelR_df_colnames_type)[1]))



  ## Output excelR table with plate data per row data
  output$plate_layout_table_excelR <-
    renderExcel({
      if(input$experiment_type != "LEGO"){
        excelTable(
          data = data.frame(plate_data_per_row_excelR_table()),
          columns = excelR_columns,
          getSelectedData = TRUE,
          allowComments = TRUE,
          autoFill = TRUE,
          autoWidth = FALSE,
          rowHeight = data.frame(c(0:7), rep(55, 8)),
          allowInsertRow = FALSE
        )
      }

    })



  ############################ STEP 3: When "Save Data" clicked, saves data inputted from excelR table on UI ###################################

  saved_plate_per_row_data <- eventReactive(input$save_plate_per_row_data_button & input$experiment_type != "LEGO", {
    data.frame(excel_to_R(input$plate_layout_table_excelR), stringsAsFactors = FALSE)
  })

  observeEvent(input$save_plate_per_row_data_button,{
    output$data_saved_text_tab1 <- renderText('Data Saved!')

    output$titrations_missing_bottom_msg <- renderUI({
      if((is.null(input$titrate_1) | is.na(input$titrate_1) | input$titrate_1 == "")  & (input$experiment_type != "LEGO")){
        div(style = "color: red;", p(strong(h4("Titrations Missing!"))))
      }

    })
  })

  ##################### STEP 4: Convert plate data per row from excelR to metadata outline table ##################

  ############################ Set Up Gating Controls ###################################
  plate_df <- data.frame(rep("Unstained",8),
                         rep("NA",8),
                         rep("NA",8),
                         rep("NA",8),
                         rep("NA",8),
                         rep("NA",8),
                         rep("NA",8),
                         rep("NA",8),
                         rep("NA",8),
                         rep("Wash",8),
                         rep("Wash",8),
                         rep("Wash",8))
  output$rhands_table <- renderRHandsontable({
    rhandsontable(plate_df, colHeaders=c(1:12), rowHeaders = c(LETTERS[1:8]), width = 750, height = "100%")
  })


  output$ui_gating <- renderUI({
    if(input$experiment_type == "S"){
      fluidRow(p(strong(h5('Select Controls for Each Well'))),
               rHandsontableOutput('rhands_table'))
    }
  })

  gating_reactive <- reactive({
    if(as_tibble(hot_to_r(input$rhands_table))){
      return(gating_control_from_table())
    }
    return(as_tibble("Gating Control"=rep(NA, 96)))
  })

  gating_control_from_table <- eventReactive(input$use_custom_gating == TRUE, {
    df <- as_tibble(hot_to_r(input$rhands_table))
    colnames(df) <- c("1", "2", "3", "4", "5", "6",
                      "7", "8", "9", "10", "11", "12")
    df <- bind_cols(df, "Row"=c("A", "B", "C", "D", "E", "F", "G", "H")) %>%
      relocate("Row", .before="1")
    df_melt <- melt(df, "Row", variable="Column")
    df_melt <- df_melt[order(df_melt$Row),] %>% add_column("Well ID"=rep(NA, 96)) %>% add_column("Gating Control"=rep(NA, 96))
    df_melt$`Well ID` <- sapply(tibble("Well ID"=rep(NA, 96)), function(i) paste0(df_melt$Row, df_melt$Column))
    df_melt$`Gating Control` <- ifelse(sapply(tibble("Gating Control"=rep(NA, 96)), function(i) {df_melt$value == "Unstained" | df_melt$value == "Wash"}), "NA", as.character(df_melt$value))
    return(df_melt$`Gating Control`)
  })


  ## Create Final metadata outline using data from excelR tables, user inputs, metadata headers/well IDs outline, etc.
  final_metadata_outline_from_inputs <- reactive({

    if(input$experiment_type != "LEGO"){
      if(input$use_custom_gating == TRUE){
        create_metadata_table(
          saved_plate_per_row_data(),
          metadata_headers_and_wellids(),
          input$plate_id,
          as.character(input$prep_date),
          # as.character(input$stain_date),
          input$experiment_type,
          titrationsList(),
          input$units,
          input$sample_type,
          input$sample_species,
          input$sample_strain,
          # input$cst_lot_number,
          gating_control_from_table(),
          input$use_custom_gating
        )
      }
      else{
        create_metadata_table(
          saved_plate_per_row_data(),
          metadata_headers_and_wellids(),
          input$plate_id,
          as.character(input$prep_date),
          # as.character(input$stain_date),
          input$experiment_type,
          titrationsList(),
          input$units,
          input$sample_type,
          input$sample_species,
          input$sample_strain,
          # input$cst_lot_number,
          c(NA),
          FALSE
        )
      }
    }
    else if(input$experiment_type == "LEGO"){
      final_metadata_from_all_plates_lego()
    }

  })


  ###################################### STEP 5: Preview metadata outline table #########################################

  ## Preview metadata outline if "Preview Table" button clicked; if not saved first, warning
  observeEvent(input$preview_metadata_outline_button, {
    validate( need(input$save_plate_per_row_data_button, "Make sure you save your results first!") )

    if(input$experiment_type != "LEGO"){
      output$preview_metadata_outline_table <- renderDataTable( final_metadata_outline_from_inputs(),options = list(
        pageLength = 10) )
    }
    else if(input$experiment_type == "LEGO"){
      output$preview_metadata_outline_table <- renderDataTable( final_metadata_from_all_plates_lego(),options = list(
        pageLength = 10) )
    }
  })

  ## Warning message on UI if data not saved before previewing
  output$error_msg <- renderText({
    "You need to save your results first!"
  })

  output$titrations_missing <- renderUI({
    if(is.null(input$titrate_1) | is.na(input$titrate_1) | input$titrate_1 == ""){
      div(style = "color: red;", p(strong(h4("Please select titrations"))))
    }

  })
  ## Download plate layout (by row) to xlsx file
  # output$download_plate_layout <- downloadHandler(
  #   filename = function() {
  #     paste(input$plate_id,"-plate_layout.xlsx", sep="")
  #   },
  #   content = function(file) {
  #     writexl::write_xlsx(saved_plate_per_row_data(), file)
  #   }
  # )

  ###################################### STEP 6: Download metadata outline file #########################################

  ## Download metadata only if data saved first, disabled if data hasn't been saved
  observe({
    # if(input$experiment_type != "LEGO"){
      if (input$save_plate_per_row_data_button > 0) {
        # POTENTIALLY ADD LATER: & !is.null(input$titrate_1) & !is.na(input$titrate_1) & input$titrate_1 != ""
        # notify the browser that the data is ready to download
        #session$sendCustomMessage("download_metadata_outline_ready", list(fileSize=floor(runif(1) * 10000)))
        message = "download outline file"
        session$sendCustomMessage("download_metadata_outline_ready", message)
      }
    # }
    # else if(input$experiment_type == "LEGO"){
    #   if (saved_plate_per_row_data_lego_ab_reag1() > 0) {
    #     # POTENTIALLY ADD LATER: & !is.null(input$titrate_1) & !is.na(input$titrate_1) & input$titrate_1 != ""
    #     # notify the browser that the data is ready to download
    #     session$sendCustomMessage("download_metadata_outline_ready", list(fileSize=floor(runif(1) * 10000)))
    #   }
    # }

  })


  ## Download metadata outline csv file
  # output$download_metadata_outline_file <- downloadHandler(
  #   filename = function() {
  #     paste(input$plate_id,"-metadata-outline.csv", sep="")
  #   },
  #   content = function(file) {
  #     write_csv(final_metadata_outline_from_inputs(), file)
  #   }
  # )

  observeEvent(input$download_metadata_outline_file, {
    file_name <- paste0(input$plate_id,"-metadata-outline.csv")
    path_exp <- file.path(datadump,
                          input$plate_id,
                          file_name
                          )
    msg = paste('Data saved to:', path_exp)
    message(msg)
    write.csv(final_metadata_outline_from_inputs(), path_exp, row.names = FALSE)
    observe(output$data_saved_text_tab1 <- renderText(HTML(msg)))
  })

  ############################################ ------------ RUN OMIQ TAB --------- #########################################

  has_new_dirs <- function() {
    ls <- unique(basename(list.dirs(datadump, recursive = FALSE)))
    return(ls)
  }

  get_dirs <- function() {
    ls <- basename(list.dirs(datadump, recursive = FALSE))
    print(ls)
    return(ls)
  }

  current_dirs <- reactivePoll(30000, session, checkFunc = has_new_dirs, valueFunc = get_dirs)

  observeEvent(current_dirs(), ignoreInit = T, ignoreNULL = T, {
                 Sys.sleep(5)
                 updateSelectInput(session, "plate_id_omiq", choices = current_dirs())
  })

  ###################################### STEP 1: Read in metadata outline csv #########################################

  ## Read in uploaded metadata outline
  uploaded_metadata_outline <- eventReactive(input$save_final_metadata_button, {
    readr::read_csv(file.path(datadump, plate_id_omiq_input(), paste0(plate_id_omiq_input(),"-metadata-outline.csv")))
  })

  ###################################### STEP 2: Map fcs filenames to well IDs in metadata outline #########################################

  ## If "Save" button clicked, get and return names of uploaded fcs files
  # uploaded_fcs_filenames <- eventReactive(input$save_final_metadata_button, {

  #   # if neither fcs files are uploaded, nor is default selected
  #   if(is.null(input$fcs_files_upload) & !input$default_fcs_files){
  #     return(rep(NA,96))
  #   }
  #   # if no files are uploaded and default selected
  #   else if(is.null(input$fcs_files_upload) & input$default_fcs_files){
  #     file_inputs <- default_fcs_filenames
  #   }
  #   # otherwise, whatever is uploaded will be used (even if default is selected)
  #   else{
  #     file_inputs <- input$fcs_files_upload$name
  #   }
  #   return(file_inputs)
  # })

  mapped_fcs_files <- eventReactive(input$pushData,{
    # inFile <- input$metadata_upload
    # if (is.null(inFile))
    #   return(NULL)
    message(paste0('number of fcs files: ', length(fcs_filenames)))
    map_fcs_filenames(fcs_filenames(), file.path(datadump, plate_id_omiq_input(), paste0(plate_id_omiq_input(), "-metadata-outline.csv")))
  })
  ###################################### STEP 3: Create table for parameters inputs #########################################

  ## Dropdown list of parameters conditional on cytometer selected
  # parameters_list <- reactive({
  #   filtered_df <- filter(cytometer_list, Nickname == input$cytometer)
  #   return(sort(filtered_df$Detector))

  # })
  # parameters_list <- sort(filter(cytometer_list, Nickname == "X-20A (Groucho)")$Detector)

  ## Create table of parameters with dropdowns from parameters_list
  # output$parameters_table <- renderRHandsontable({
  #   rhandsontable(
  #     data.frame("Parameter" = rep(NA_character_, 8)),
  #     rowHeaders = c(paste("Row", LETTERS[1:8])),
  #     rowHeaderWidth = 100
  #   ) %>%
  #     hot_col(col = "Parameter",
  #             type = "dropdown",
  #             source = parameters_list) %>%
  #     hot_col(1, width = 100)
  # })

  ## Convert parameters table to R object to input to finalized metadata function
  # parameters_obj <- eventReactive(input$save_final_metadata_button, {
  #   if (!is.null(input$parameters_table))
  #     # return(data.frame(hot_to_r(input$parameters_table), stringsAsFactors = FALSE))
  #     return(input$parameters_table)
  # })


  ###################################### STEP 4: Save data and download to final metadata csv #########################################

  ## Outputs text to UI indicating that data was saved
  ui_status <- reactiveValues(text = 'Idle')
  output$saved_final <- renderUI(
      h4(paste0('STATUS: ', ui_status$text), style="color:blue;")
      )

  observeEvent(input$save_final_metadata_button, {

    # ui_status$text <- 'Loading...'
    session$sendCustomMessage("saveBtnMessage", "Loading...")
    session$sendCustomMessage('disableButton', 'save_final_metadata_button')
    session$sendCustomMessage('disableButton', 'pushData')
    session$sendCustomMessage('disableButton', 'rerun')
    session$sendCustomMessage('addClass', message = list(id = 'buttonload', btn = 'save_final_metadata_button'))
    parameter <- rep(NA_character_, 8)
    file_path <- file.path(datadump, plate_id_omiq_input())
    check_max_vals_wellid <- fcs_filenames()[which(grepl("[A-H]08.*fcs$",fcs_filenames()))]
    extracted_mdata <- grab_meta_data(file_path, check_max_vals_wellid)
    param <- lapply(names(extracted_mdata), grep, pattern = "well", value=F)
    param_index <- names(extracted_mdata)[which(sapply(param, FUN=function(X) 1 %in% X))]
    param <- extracted_mdata[param_index]
    param <- unlist(param, use.names = F)
    param <- gsub("[\\.]", "-", param)

    # replace some or all of the NA's in the vector
    for (i in seq(1,length(param))) {
      parameter[i] <- param[i]
    }

    reactive_vals$cytometer_name <- extracted_mdata$CYT
    reactive_vals$serial_num <- extracted_mdata$CYTNUM
    reactive_vals$run_date <- extracted_mdata$DATE

    parameters_list <- sort(filter(cytometer_list, `Serial Number` == extracted_mdata$CYTNUM)$Detector)

    output$parameters_table <- renderRHandsontable({
      rhandsontable(
        data.frame("Parameter" = parameter),
        rowHeaders = c(paste("Row", LETTERS[1:8])),
        rowHeaderWidth = 172
      ) %>%
        hot_col(col = "Parameter",
                type = "dropdown",
                source = parameters_list) %>%
        hot_col(1, width = 172)
    })

    # ui_status$text <- 'Updated data!'
    session$sendCustomMessage("saveBtnMessage", "Updated data!")
    session$sendCustomMessage('enableButton', 'save_final_metadata_button')
    session$sendCustomMessage('enableButton', 'pushData')
    session$sendCustomMessage('enableButton', 'rerun')
    session$sendCustomMessage('removeClass', 'save_final_metadata_button')
  })


  ## Read plate id from metadata outline for final metadata file naming
  #plate_id_input <- reactive({ readr::read_csv(input$metadata_upload$datapath)$`Plate ID`[[1]] })
  plate_id_input <- reactive({ input$plate_id})

  plate_id_omiq_input <- reactive({ input$plate_id_omiq})

  fcs_filenames <- reactive({
          list.files(file.path(datadump, plate_id_omiq_input()), pattern = "*.fcs$")
  })

  # FINAL Metadata table for OMIQ
  # final_metadata_table_for_omiq <- reactive({ merge_metadata_for_OMIQ(mapped_fcs_files()$Filename, uploaded_metadata_outline(), input$plate_id_omiq, input$donor_id, input$cytometer, parameters_obj(), input$notes) })
  final_metadata_table_for_omiq <- reactive({
                                    merge_metadata_for_OMIQ(
                                      mapped_fcs_files()$Filename,
                                      uploaded_metadata_outline(),
                                      hot_to_r(input$parameters_table),
                                      reactive_vals$cytometer_name,
                                      reactive_vals$serial_num,
                                      reactive_vals$run_date,
                                      input$donor_id,
                                      input$notes)
                                })


  ############ OMIQ Main Panel - Configured with Airflow & Docker Container #############

  observeEvent(input$pushData, {

    ui_status$text <- 'Running...'
    session$sendCustomMessage('disableButton', 'pushData')
    session$sendCustomMessage('disableButton', 'rerun')
    session$sendCustomMessage('disableButton', 'save_final_metadata_button')
    session$sendCustomMessage('btnAppear', 'stop')
    session$sendCustomMessage('addClass', message = list(id = 'buttonload', btn = 'pushData'))


    folder_dir <- plate_id_omiq_input()
    message(folder_dir)
    path_exp <- file.path(datadump, folder_dir)
    path_run_params <- file.path(path_exp, "run-params")
    dir.create(path_run_params, showWarnings = FALSE)

    df <- final_metadata_table_for_omiq()

    WIDTH_BASIS <- input$biexSlider
    LMG_NAMES <- c(input$Pop1, input$Pop2, input$Pop3)
    POP_MINS <- list(x = input$PopCount1,
                     y = input$PopCount2,
                     z = input$PopCount3)
    POP_MINS <- setNames(POP_MINS, LMG_NAMES)
    print(paste0('width basis: ', WIDTH_BASIS))
    print(paste0('LMG names: ', LMG_NAMES))
    print(paste0('POP_MINS: ', POP_MINS))

    POP_COLORS <- list(x = input$col1,
                       y = input$col2,
                       z = input$col3)
    POP_COLORS <- setNames(POP_COLORS, LMG_NAMES)

    save(LMG_NAMES, file = file.path(path_run_params, "LMG_NAMES.RData"))
    save(POP_MINS, file = file.path(path_run_params, "POP_MINS.RData"))
    save(WIDTH_BASIS, file = file.path(path_run_params, "WIDTH_BASIS.RData"))
    save(POP_COLORS, file = file.path(path_run_params, "POP_COLORS.RData"))

    print(paste("number of columns:",
                ncol(df),
                "|",
                "number of rows:",
                nrow(df),
                sep=" "))


  # Write final metadata file to plate id folder in datadump folder
    write_csv(
      final_metadata_table_for_omiq(),
      file.path(
        path_exp,
        "metafile.csv"
      )
    )

  # Call Airflow

  current_time = Sys.time()
  dag_run_id = paste0(folder_dir, "_", format(current_time, "%Y-%m-%dT%H_%M_%S"))
  write(dag_run_id, file = file.path(datadump, plate_id_omiq_input(), "dag_run_id.txt"))
  print(paste0("dag run id: ", dag_run_id))
  attr(current_time, "tzone") <- "UTC"
  current_time <- format(current_time, '%Y-%m-%dT%H:%M:%SZ')
  print(paste("current UTC time:", current_time))

  body <- list(conf=list(EXP_ID=folder_dir, RE_RUN="FALSE"), dag_run_id=dag_run_id, execution_date=current_time)
  link <- paste0("http://", VM, ":8000/api/v1/dags/r_dag/dagRuns")
  res <- httr::POST(url = link,
                    config = authenticate("airflow", "airflow"),
                    body = jsonlite::toJSON(body, pretty = T, auto_unbox = T),
                    httr::add_headers(`accept` = 'application/json'),
                    httr::content_type('application/json'))

  # Output response to UI
  save_string_result <- content(res, "parsed")
  session$sendCustomMessage(type = "msgbox",
                            message = paste0("status: ", as.character(save_string_result$state)))

  output$airflow_response <- renderUI({
    save_string_result
  })

  check_log <- reactive({
    ct <- paste0(substr(current_time, 1, nchar(current_time)-1), "+00:00")
    fp <- file.path(datalogs, ct, "1.log")

    if (ui_status$text != "Stop button pressed" |
          substr(ui_status$text, 1,31) == "STATUS: OMIQ pipeline completed" ) {
      invalidateLater(12000, session) # 12 secs
      tryCatch({
        Sys.sleep(5)
        raw_text <- readLines(fp)
        split_text <- stringi::stri_split(str = raw_text, regex = "\\n")
        replaced_text <- lapply(split_text, p) # list with <p> html tags
        return(replaced_text)
      },
      error = function(e) {
            emsg <- paste0("Read log file error: ", e)
            print(emsg)
            return(emsg)
      })
    } else {
          msg <- "Stop button pressed, no longer checking log file"
          print(msg)
          return(msg)
      }
  })


  output$log_output <- renderUI({
    check_log()
  })

  # reactivePoll check - check rest API every 15 seconds
  observe({
      tryCatch( {
         res <- GET_airflow(VM, dag_run_id)
         print(ui_status$text)
         if (res$state == "success" | substr(ui_status$text, 1,31) == "STATUS: OMIQ pipeline completed") {
             session$sendCustomMessage('enableButton', 'pushData')
             session$sendCustomMessage('enableButton', 'rerun')
             session$sendCustomMessage('enableButton', 'save_final_metadata_button')
             session$sendCustomMessage('removeClass', 'pushData')
             session$sendCustomMessage('btnDisappear', 'stop')
             ui_status$text <- paste0('OMIQ pipeline completed for ', plate_id_omiq_input())
         } else {
             invalidateLater(pollTimer, session)
         }
      },
      error = function(e) {
            print(paste0("GET_airflow error: ", e))
      })
  })

}) # push omiq button observeEvent

  observeEvent(input$stop, {
          ui_status$text <- "Stop button pressed"
          session$sendCustomMessage("enableButton", "rerun")
          session$sendCustomMessage("enableButton", "pushData")
          session$sendCustomMessage('btnDisappear', 'stop')
          session$sendCustomMessage('enableButton', 'save_final_metadata_button')
          session$sendCustomMessage('removeClass', 'pushData')
          session$sendCustomMessage('removeClass', 'rerun')
          fp <- file.path(datadump, plate_id_omiq_input(), "dag_run_id.txt")
          dag_run_id <- read.csv(fp, header = F, stringsAsFactors = F)$V1
          print(paste0("dag run id (delete): ", dag_run_id))
          tryCatch( {
            req <- DELETE_airflow(VM, dag_run_id)
            print(req)
            # print(paste0('status code: ', as.character(req$status)))
            session$sendCustomMessage(type = "msgbox",
                                    message = "Stopped run")
                                    # message = paste0("status detail: ", as.character(req$detail)))
          },
          error = function(e) {
            print(e)
            session$sendCustomMessage(type = "msgbox",
                                    message = paste("Error:", e))
          }
          )
  })

  rerun_data_path <- reactive({
    folder_dir <- plate_id_omiq_input()
    message(folder_dir)
    path_exp <- file.path(datadump, folder_dir)
    check_dir <- dir.exists(path_exp)

    validate(
             need(check_dir, paste("Ensure you have already ran the pipeline and files exists in:", path_exp))
             )
    return(path_exp)
  })

  observeEvent(input$rerun, {


    ui_status$text <- 'Running...'
    session$sendCustomMessage('disableButton', 'pushData')
    session$sendCustomMessage('disableButton', 'save_final_metadata_button')
    session$sendCustomMessage('disableButton', 'rerun')
    session$sendCustomMessage('btnAppear', 'stop')
    session$sendCustomMessage('addClass', message = list(id = 'buttonload', btn = 'rerun'))

    WIDTH_BASIS <- input$biexSlider
    LMG_NAMES <- c(input$Pop1, input$Pop2, input$Pop3)
    POP_MINS <- list(x = input$PopCount1,
                     y = input$PopCount2,
                     z = input$PopCount3)
    POP_MINS <- setNames(POP_MINS, LMG_NAMES)
    print(paste0('width basis: ', WIDTH_BASIS))
    print(paste0('LMG names: ', LMG_NAMES))
    print(paste0('POP_MINS: ', POP_MINS))

    POP_COLORS <- list(x = input$col1,
                       y = input$col2,
                       z = input$col3)
    POP_COLORS <- setNames(POP_COLORS, LMG_NAMES)

    path_run_params <- file.path(rerun_data_path(), "run-params")

    save(LMG_NAMES, file = file.path(path_run_params, "LMG_NAMES.RData"))
    save(POP_MINS, file = file.path(path_run_params, "POP_MINS.RData"))
    save(WIDTH_BASIS, file = file.path(path_run_params, "WIDTH_BASIS.RData"))
    save(POP_COLORS, file = file.path(path_run_params, "POP_COLORS.RData"))

    # Call Airflow

    folder_dir <- basename(rerun_data_path())
    current_time = Sys.time()
    dag_run_id = paste0(folder_dir, "_", format(current_time, "%Y-%m-%dT%H_%M_%S"))
    write(dag_run_id, file = file.path(datadump, plate_id_omiq_input(), "rerun_dag_run_id.txt"))
    print(paste0("dag run id: ", dag_run_id))
    attr(current_time, "tzone") <- "UTC"
    current_time <- format(current_time, '%Y-%m-%dT%H:%M:%SZ')
    print(paste("current UTC time:", current_time))

    body <- list(conf=list(EXP_ID=folder_dir, RE_RUN="TRUE"), dag_run_id=dag_run_id, execution_date=current_time)
    link <- paste0("http://", VM, ":8000/api/v1/dags/r_dag/dagRuns")
    res <- httr::POST(url = link,
                      config = authenticate("airflow", "airflow"),
                      body = jsonlite::toJSON(body, pretty = T, auto_unbox = T),
                      httr::add_headers(`accept` = 'application/json'),
                      httr::content_type('application/json'))

    # Output response to UI
    save_string_result <- content(res, "parsed")
    session$sendCustomMessage(type = "msgbox",
                              message = paste0("status: ", as.character(save_string_result$state)))

    output$airflow_response <- renderUI({
      save_string_result
    })

    rerun_check_log <- reactive({
      ct <- paste0(substr(current_time, 1, nchar(current_time)-1), "+00:00")
      fp <- file.path(datalogs, ct, "1.log")

    if (ui_status$text != "Stop button pressed" |
          substr(ui_status$text, 1,31) == "STATUS: OMIQ pipeline completed") {
      invalidateLater(12000, session) # ~ 12 secs
      tryCatch({
        Sys.sleep(5)
        raw_text <- readLines(fp)
        split_text <- stringi::stri_split(str = raw_text, regex = "\\n")
        replaced_text <- lapply(split_text, p) # list with <p> html tags
        return(replaced_text)
      },
      error = function(e) {
            emsg <- paste0("Read log file error: ", e)
            print(emsg)
            return(emsg)
      })
    } else {
          msg <- "Stop button pressed, no longer checking log file"
          print(msg)
          return(msg)
      }
    })


    output$log_output <- renderUI({
      rerun_check_log()
    })

    # reactivePoll check - check rest API every 15 seconds
    observe({
        tryCatch( {
           res <- GET_airflow(VM, dag_run_id)
           print(ui_status$text)
           if (res$state == "success" | substr(ui_status$text, 1,31) == "STATUS: OMIQ pipeline completed") {
               session$sendCustomMessage('enableButton', 'pushData')
               session$sendCustomMessage('enableButton', 'rerun')
               session$sendCustomMessage('enableButton', 'save_final_metadata_button')
               session$sendCustomMessage('removeClass', 'pushData')
               session$sendCustomMessage('btnDisappear', 'stop')
               ui_status$text <- paste0('OMIQ pipeline completed for ', plate_id_omiq_input())
           } else {
               invalidateLater(pollTimer, session)
           }
        },
        error = function(e) {
              print(paste0("GET_airflow error: ", e))
        })
    })
  })
}
