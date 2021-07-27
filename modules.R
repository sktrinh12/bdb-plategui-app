BV421_DF <- readxl::read_xlsx("bv421_test.xlsx") # copy of BV421 DB Excel being referenced (temporary)
BV421_DF <- BV421_DF %>% add_row(Clone = "NA", .before = 1) # add row of NA at the top to set default value if no clone selected
clones <- c(as.character(BV421_DF$Clone)) # list of clones for clone dropdowns
isotype_clones <- sort(unique(readr::read_csv('fixed_lists.csv')$`Isotype Clones`))
rows <- c('NA', 'A', 'B','C','D','E','F','G','H', plate_config_wells()$Well.ID)
species <- c( 'cat', 'chicken', 'human', 'mouse', 'rat' )
target_species_list <- append("NA",sort(unique(readr::read_csv('fixed_lists.csv')$`Target Species`)))
host_species_list <- append("NA",sort(unique(readr::read_csv('fixed_lists.csv')$`Host Species`)))
spec_name_list <- append("NA",sort(unique(readr::read_csv('fixed_lists.csv')$Pops)))
iso_heavy_list <- append("NA",sort(unique(readr::read_csv('fixed_lists.csv')$`Isotype (Heavy Chain)`)))
iso_light_list <- append("NA",sort(unique(readr::read_csv('fixed_lists.csv')$`Isotype (Light Chain)`)))

gating_method_list <- c('sd', 'pct', 'nsd', 'sp')
fluorochrome <- sort(unique(readr::read_csv('fixed_lists.csv')$Fluorochromes))
experiment_type_list <- c('OB', 'LEGO', 'S')
cytometer_list <- readr::read_csv('cytometer_parameter_lists.csv')

## Configure excelR column names and types
excelR_df_colnames_type_LEGO <- data.frame(rbind(
    
    
    c('Reagent #', 'text', 0),
    c('Row', 'text', 0),
    c("Ab Type", "dropdown",0),
    c('Gating Control', 'dropdown',0),
    c("Concentration (mg/ml)", "text", 0),
    c("Concentration (ug/test)", "text", 0),
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
    c("Catalog # / Material #", "text", 0),
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
), stringsAsFactors = FALSE)

names(excelR_df_colnames_type_LEGO) <- c('title', 'type', 'source')

## Configure excelR columns (title, type, and source of dropdowns)
excelR_columns_LEGO = data.frame(title = excelR_df_colnames_type_LEGO$title, 
                                 type = excelR_df_colnames_type_LEGO$type, 
                                 source = I(list(
                                     0, # reagent number
                                     0, # row
                                     c("Test Ab", "Test Iso", "Ref Ab 1", "Ref Iso 1", "Ref Ab 2", "Ref Iso 2"),
                                     rows, #gating_control
                                     0, #conc mg/ml
                                     0, #conc ug/test
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
                                     0, #catalog material no
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
                                 width = rep(100,dim(excelR_df_colnames_type_LEGO)[1]),
                                 stringsAsFactors = FALSE)

excelR_df_colnames_type_LEGO_isotype <- data.frame(rbind(
    c("Reagent #", "text", 0),
    c('Row', 'text', 0),
    c("Ab Type", "dropdown",0),
    c("Clone", "dropdown",0),
    c("Concentration (mg/ml)", "text",0),
    c("Concentration (ug/test)", "text", 0),
    c("Batch Number", "text", 0),
    c("Catalog # / Material #", "text", 0)
    
), stringsAsFactors = FALSE)

names(excelR_df_colnames_type_LEGO_isotype) <- c('title', 'type', 'source')

## Configure excelR columns (title, type, and source of dropdowns)
excelR_columns_LEGO_isotype = data.frame(title = excelR_df_colnames_type_LEGO_isotype$title, 
                                         type = excelR_df_colnames_type_LEGO_isotype$type, 
                                         source = I(list(
                                             0,# reagent #
                                             rows, # row #
                                             c("Test Ab", "Test Iso", "Ref Ab 1", "Ref Iso 1", "Ref Ab 2", "Ref Iso 2"),
                                             isotype_clones, #clone
                                             0, #conc mg/ml
                                             0, #conc ug/test
                                             0, #batch no.
                                             0 #catalog/material #
                                             
                                             
                                         )),
                                         width = rep(100,dim(excelR_df_colnames_type_LEGO_isotype)[1]), stringsAsFactors = FALSE)

titr_ui <- function(id) {
    tagList(
    h4("Configure Titrations for Test Ab/Isotype"),
    fluidRow(
        div(style = 'overflow-x: scroll;',
            div(style = "display: grid; grid-template-columns: 130px 300px 180px; grid-template-rows: auto auto; max-height: 80px;",
                div(style = "padding: 5px; text-align: center;",
                    strong(h5("# of Titrations"))
                ),
                div(style = "padding: 5px; text-align: center;",
                    strong(h5("Titration Order"))
                ),
                div(style = "padding: 5px; text-align: center;",
                    strong(h5("Starting Titration"))
                )
            ),
            
            div(style = "display: grid; grid-template-columns: 130px 300px 180px; max-height: 80px;",
                div(style = "padding: 5px; text-align: center;",
                    numericInput(NS(id,'titr_lego_no'), label = '', value = 6, min = 1, max = 8)
                ),
                div(style = "padding: 5px; text-align: center;",
                    radioButtons(NS(id,'titration_order_lego'), '',
                                 choices=c('high-to-low', 'low-to-high'), 
                                 inline=TRUE, selected='low-to-high')
                ),
                div(style = "padding: 5px; text-align: center;",
                    numericInput(NS(id,'start_titr_no'), label = '', value = NULL, min = 0.015, max = 2)
                )
            )
        )
        
    ))

    
}

titr_server <- function(id) {
    
    moduleServer(id, function(input, output, session){

        observeEvent(input$titration_order_lego, {
            if(input$titration_order_lego == "high-to-low"){
                updateNumericInput(session, 'start_titr_no', value = 2)
            }
            else{
                updateNumericInput(session, 'start_titr_no', value = 0.03)
            }
        })
        reactive({ c(input$titr_lego_no, input$titration_order_lego, input$start_titr_no) })
        
    })
    
}

sliderTextUI <- function(id){
    ns <- NS(id)
    tagList(
        sliderInput(ns("slider"), "slide me", 0, 100, 1),
        textOutput(ns("number"))
    )
}

sliderText <- function(input, output, session){
    output$number <- renderText({
        input$slider
    })
    
}
clone_lego_ui <- function(id, reg_test, reg_refA, reg_refB, upd_selected) {
    tagList(
        h4("Select Clone for Each Antibody"),
        div(style = "display: grid; grid-template-columns: 130px 130px 130px; grid-template-rows: auto auto;",
            div(style = "text-align: center;",
                strong(h5("Test Antibody"))
            ),
            div(style = "text-align: center;",
                strong(h5("Reference #1"))
            ),
            div(style = "text-align: center;",
                strong(h5("Reference #2"))
            ),
            div(style = "padding: 5px; text-align: left;",
                test_clone_ui(reg_test)
            ),
            div(style = "padding: 5px; text-align: left;",
                ref_a_clone_ui(reg_refA)
            ),
            div(style = "padding: 5px; text-align: left;",
                ref_b_clone_ui(reg_refB)
            )
        )
    )
}

test_clone_ui <- function(id){
    selectizeInput(NS(id,'test_clone'), label = '', choices = clones, selected = NULL, options=list(create=TRUE))
}
test_clone_server <- function(id) {
    
    moduleServer(id, function(input, output, session){
        reactive({ input$test_clone })
    })
    
}
ref_a_clone_ui <- function(id){
    selectizeInput(NS(id,'ref_clone_a'), label = '', choices = clones, selected = "NA", options=list(create=TRUE))
}
ref_a_clone_server <- function(id, updated_selection) {
    
    moduleServer(id, function(input, output, session){
        observe({
            
            updateSelectizeInput(session, 
                                 "ref_clone_a", 
                                 choices = clones, 
                                 selected= updated_selection(), 
                                 server=TRUE
            )
        })
        reactive({ input$ref_clone_a })
    })
    
}

ref_b_clone_ui <- function(id){
    selectizeInput(NS(id,'ref_clone_b'), label = '', choices = clones, selected = "NA", options=list(create=TRUE))
}
ref_b_clone_server <- function(id, updated_selection) {
    
    moduleServer(id, function(input, output, session){
        observe({
            
            updateSelectizeInput(session, 
                                 "ref_clone_b", 
                                 choices = clones, 
                                 selected= updated_selection(), 
                                 server=TRUE
            )
        })
        reactive({ input$ref_clone_b })
    })
    
}

isotype_table_ui <- function(id){
    tagList(
        h4("Configure Isotypes"),
        fluidRow(
            div(style = 'overflow-x: scroll;',
                excelOutput(NS(id,"plate_layout_table_excelR_LEGO_iso"), width = 'auto',height = 'auto')
            )
        )
    )
}

isotype_table_server <- function(id, reagent_number){
    
    moduleServer(id, function(input, output, session){
        
        output$plate_layout_table_excelR_LEGO_iso <-
            renderExcel({
                excelTable(
                    data = row_tibble_lego_isotypes(reagent_number),
                    columns = excelR_columns_LEGO_isotype,
                    getSelectedData = TRUE,
                    allowComments = TRUE,
                    autoFill = TRUE,
                    autoWidth = FALSE,
                    rowHeight = data.frame(c(0:11), rep(55, 12)),
                    allowInsertRow = FALSE
                )
        })
        reactive({input$plate_layout_table_excelR_LEGO_iso})
    })
}

antibody_table_ui <- function(id){
    tagList(
        h4("Configure Antibodies"),
        fluidRow(
            div(style = 'overflow-x: scroll;',
                excelOutput(NS(id,"plate_layout_table_excelR_LEGO_ab"), width = 'auto',height = 'auto')
            )
        )
    )
}

antibody_table_server <- function(id, reagent_number, plate_per_row_data){
    
    moduleServer(id, function(input, output, session){
        output$plate_layout_table_excelR_LEGO_ab <-
            renderExcel({
                excelTable(
                    data = data.frame(row_tibble_lego(reagent_number, plate_per_row_data())),
                    columns = excelR_columns_LEGO,
                    getSelectedData = TRUE,
                    allowComments = TRUE,
                    autoFill = TRUE,
                    autoWidth = FALSE,
                    rowHeight = data.frame(c(0:2), rep(55, 3)),
                    allowInsertRow = FALSE
                )
            })
        reactive({input$plate_layout_table_excelR_LEGO_ab})
    })
}

save_ab_iso_table_ui <- function(id){
    tagList(
        div(style = "display: grid; grid-template-columns: 150px 180px; padding: 5px;",
            actionButton(NS(id,'save_lego_table'), label="Save Table", icon("save"), class="btn-primary"),
            div(style = "display: flex; align-items: center; justify-content: center;", textOutput(NS(id,"data_saved_text_tab_lego")) %>% 
                    tagAppendAttributes(style= 'color:#2A7BE1; font-size: 20px; font-weight: bold;'))),
    )
}

save_ab_iso_table_server <- function(id){
    
    moduleServer(id, function(input, output, session){
        observeEvent(input$save_lego_table,{
            output$data_saved_text_tab_lego <- renderText('Data Saved!')
        })
        
        reactive({input$save_lego_table})
    })
}

save_ab_iso_table_server_2 <- function(id){

    moduleServer(id, function(input, output, session){
        saved_plate_per_row_data_lego_iso <- eventReactive(reag_saved(), {
            data.frame(excel_to_R(table_data()), stringsAsFactors = FALSE)
        })
    })
}
