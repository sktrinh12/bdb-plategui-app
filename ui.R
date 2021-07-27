library(shiny)
library(readxl)
library(writexl)
library(readr)
library(tibble)
library(dplyr)
library(shinythemes)
library(colourpicker)
library(shinydashboard)
library(fs)
library(reshape2)
library(excelR)
library(httr) # for configuring to airflow
library(stringr)
library(tidyr)
library(rhandsontable)


source('global.R')
source('instructions_html.R')
source("modules.R")

# Layout variables
COLUMN_ID <- 4
OFFSET_ID <- 5
ROW_NAMES_ID <- 3
OMIQ_COLUMN <- 4

ui <- tagList(
    navbarPage(
        # includeHTML("analytics.html"),
        theme = shinytheme("readable"),  # app theme
        "Metadata App", # title of app

        # Instructions Panel
        tabPanel("Instructions",
                 navlistPanel(widths=c(3,8),
                     "Overview",
                     tabPanel("What is the Plate Layout App?",
                              overview_what()
                              ),
                     tabPanel("Who will use the app?",
                              overview_who()
                              ),
                     tabPanel("App Features",
                              overview_app_features()
                              ),
                     "Instructions",
                     tabPanel("Overview",
                          instructions_overview()
                     ),
                     tabPanel("Plate Layout tab",
                              instructions_plate_layout_tab(),
                              instructions_plate_layout_tab_stability(),
                              instructions_plate_layout_tab_optibuild(),
                              instructions_plate_layout_tab_lego()
                              ),
                     tabPanel("Run OMIQ tab",
                              instructions_run_omiq_tab(),
                              instructions_mapping_fcsfiles()
                              ),
                     "Access & Contacts",
                     tabPanel("How to Access the App",
                              access()
                              ),
                     tabPanel("Contact Info",
                              contacts()
                              )

                ) #end of Navlist

        ), #end of Instructions tabpanel
        ## Tab Panel for initial plate layout. Final output is metadata-outline csv file to be imported in next tab later ##
        tabPanel("Plate Layout",
                 sidebarPanel(width = 4,
                              fluidRow(div(style = 'overflow-x: scroll;',
                              # fluidRow(
                                  div(style = "display: grid; grid-template-columns: auto; padding: 5px;",
                                      textInput("plate_id", "Plate ID*", placeholder="YYYYMMDD-INITIALS-PLATE#")
                                      ), # will be used in default name of downloaded file
                                  div(style = "display: grid; grid-template-columns: 170px 170px 170px; grid-template-rows: auto; padding: 5px;",
                                      div(style = "text-align: left; padding: 5px",
                                          textInput("cst_lot_number", "CS&T Beads Lot #", placeholder="Ex. 123456")
                                      ),
                                      div(style = "text-align: left; padding: 5px",
                                          dateInput("prep_date", "Prep Date")
                                      ),
                                      div(style = "text-align: left; padding: 5px",
                                          dateInput("stain_date", "Stain Date")
                                      )),
                                  div(style = "display: grid; grid-template-columns: 170px auto; grid-template-rows: auto; padding: 5px;",
                                      div(style = "text-align: left; padding: 5px",
                                          selectInput('experiment_type', 'Experiment Type*', choices = experiment_type_list, selected='S')
                                      ),
                                      div(style = "text-align: left; padding: 5px 5px 5px 2.5px",
                                          uiOutput('stability_ui_time_clone')
                                      )),
                                  div(style = "display: grid; grid-template-columns: 170px 170px 170px; grid-template-rows: auto auto; padding: 5px;",
                                      div(style = "text-align: center; grid-column: 1 / 4; grid-row: 1",
                                          h4('Model System Data')
                                          ),
                                      div(style = "text-align: left; padding: 5px",
                                          selectInput('sample_type', 'Sample Type*',
                                                      choices = c('LWB', 'PBMC', 'Spleen', 'BM', 'Thymus', 'Cell line'),
                                                      selected = 'LWB')
                                      ),
                                      div(style = "text-align: left; padding: 5px",
                                          selectInput('sample_species', 'Sample Species',
                                                      choices = c('Human', 'Mouse', 'Rat', 'Monkey', 'Other'))
                                      ),
                                      div(style = "text-align: left; padding: 5px",
                                          selectizeInput('sample_strain', 'Strain/Sub-Type',
                                                         choices = c('NA','293F Untransfected', '293F Transient Transfection', '293F Stable Transfection','Balb/c', 'B6', 'Other'),
                                                         selected='NA',
                                                         options = list(create = TRUE))
                                      ),
                                      div(style = "text-align: left; padding: 5px",
                                          selectizeInput('spec1_name', 'Pop 1 Name*',
                                                         choices = spec_name_list,
                                                         selected = 'LWB-L',
                                                         options = list(create = TRUE))
                                      ),
                                      div(style = "text-align: left; padding: 5px",
                                          selectizeInput('spec2_name', 'Pop 2 Name*',
                                                         choices = spec_name_list,
                                                         selected = 'LWB-M',
                                                         options = list(create = TRUE))
                                      ),
                                      div(style = "text-align: left; padding: 5px",
                                          selectizeInput('spec3_name', 'Pop 3 Name*',
                                                         choices = spec_name_list,
                                                         selected = 'LWB-G',
                                                         options = list(create = TRUE))
                                      )
                                      ),

                              uiOutput('sidebar_selectInput_items'), # sidebar panel selectInputs corresponding to rows of plate
                              fluidRow(column(12,p(em(h6('* indicates required field')))))
                              ))

                 ),
                 mainPanel(width=8,
                     tags$head(
                         tags$link(rel = "stylesheet", type = "text/css", href = "custom.css") # custom css found in www/custom.css file
                     ),
                     uiOutput('main_panel_ui'),
                     fluidRow(
                         wellPanel(width = 12, status = "success", solidHeader = TRUE, title = "Generate Metadata CSV",
                                   actionButton("save_plate_per_row_data_button", "Save Results", class = "btn-primary", icon("save"), style="color: #fff"),
                                   actionButton("preview_metadata_outline_button", "Preview Table", class = "btn-primary", icon("eye"), style="color: #fff"),
                                   downloadButton('download_plate_layout', "Download Plate Layout", class = "btn-primary"),
                                   downloadButton("download_metadata_outline_file", "Download Metadata Outline", class = "btn-primary", style="color: #fff"),
                                   helpText("Download will be available after you have updated the table and saved your data."),
                                   br(),
                                   br(),
                                   uiOutput('titrations_missing_bottom_msg'),
                                   textOutput("data_saved_text_tab1") %>%
                                       tagAppendAttributes(style= 'color:#2A7BE1; font-size: 20px; font-weight: bold;'),
                                   conditionalPanel(condition = "((input.save < 1) && (input.preview >= 1)) | ((input.save < 1) && (input.download_metadata_outline_file >= 1))",
                                                    textOutput('error_msg') %>% # error message shown if preview table w/o saving
                                                        tagAppendAttributes(style= 'color:#FF0000; font-size: 20px; font-weight: bold;')
                                   ),
                                   # download button disabled if data hasn't been saved
                                   singleton(tags$head(HTML(
                                       '
                                      <script type="text/javascript">
                                        $(document).ready(function() {
                                          // disable download at startup. data_file is the id of the downloadButton
                                          $("#download_metadata_outline_file").attr("disabled", "true").attr("onclick", "return false;");

                                          Shiny.addCustomMessageHandler("download_metadata_outline_ready", function(message) {
                                            $("#download_metadata_outline_file").removeAttr("disabled").removeAttr("onclick").html(
                                              "<i class=\\"fa fa-download\\"></i>Download Metadata Outline");
                                                console.log(message);
                                          });
                                        })
                                      </script>
                                    '
                                   ))),
                         ))
                 ) #end of mainPanel
        ),
        tabPanel('Run OMIQ',
                 sidebarPanel(
                     fluidRow(
                             fluidRow(column(12,
                                             fileInput("metadata_upload", "Upload Metadata Outline CSV*", multiple=FALSE, icon("upload")))
                                      ),

                             fluidRow(column(8,textInput("donor_id", "Donor ID", value = 'NA'))),
                             fluidRow(
                                 column(6,selectizeInput("cytometer", "Cytometer*", choices = unique(cytometer_list$Nickname), options=list(create=TRUE))),
                                 column(6,rHandsontableOutput("parameters_table"))),
                             fluidRow(column(12, textAreaInput("notes", "Notes", placeholder="Add any notes regarding your experiment.", height="100px"))),
                             fluidRow(column(12,p(em(h6('* indicates required field'))))),
                             fluidRow(column(6,actionButton("save_final_metadata_button", "Save Data", icon("save"), class="btn-primary"))),
                             fluidRow(column(6,textOutput("saved_final") %>%
                                 tagAppendAttributes(style= 'color:#2A7BE1; font-size: 20px; font-weight: bold;'))

                             )
                         )
                 ),
                 mainPanel(
                     div(style="color: #B2B6BB;",h3(em("Feature below is currently disabled. Coming soon."))),
                     br(),
                     # button to push to R-OMIQ
                     actionButton('pushData', 'Push Metadata to OMIQ', class="btn-primary"),
                     actionButton('rerun', 'Re-run', class="btn-primary"),
                     uiOutput('airflow_response'),
                     br(),
                     fluidRow(
                         column(OMIQ_COLUMN, textInput('Pop1', 'Pop1', value='Lymph')),
                         column(OMIQ_COLUMN, textInput('Pop2', 'Pop2', value='Mono')),
                         column(OMIQ_COLUMN, textInput('Pop3', 'Pop3', value='Gran')),
                         width=12),
                     fluidRow(
                         column(OMIQ_COLUMN, numericInput('PopCount1', 'PopCount1', value=2000, min=100, max=5000, step=100)),
                         column(OMIQ_COLUMN, numericInput('PopCount2', 'PopCount2', value=300, min=100, max=3000, step=100)),
                         column(OMIQ_COLUMN, numericInput('PopCount3', 'PopCount3', value=1500, min=100, max=10000, step=100)),
                         width=12),
                     fluidRow(
                         column(OMIQ_COLUMN, colourpicker::colourInput('col1', 'Pop1 colour', "#fc1828ff")),
                         column(OMIQ_COLUMN, colourpicker::colourInput('col2', 'Pop2 colour', "#fd8628ff")),
                         column(OMIQ_COLUMN, colourpicker::colourInput('col3', 'Pop3 colour', "#2983ffff")),
                         width=12),
                     hr(),
                     sliderInput("biexSlider", label = h3("biex value"), min = -1000, max = -1, value = -300)
                 )))
)

