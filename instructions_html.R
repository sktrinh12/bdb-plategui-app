overview_what <- function(){
    return(div(
        div(class = "text-primary", h3(strong(
            "What is the Metadata App?\n"
        ))),
        div(
            class = "lead",
            style = "font-size: 20px;",
            p(
                "The Metadata App is a web application used to create downloadable metadata CSV files, which are files that store all the metadata associated with a plate experiment and is used as an input to the OMIQ automated data analysis pipeline. This app is a more automated and user-friendly alternative to filling out the metadata files manually using Excel. In the future, this app will also be an interface to run the OMIQ automated data analysis pipeline from."
            )
        )
    ))
}

overview_who <- function(){
    
    return(div(
        div(class = "text-primary", h3(strong(
            "Who Will Use the App?\n"
        ))),
        div(
            class = "lead",
            style = "font-size: 20px;",
            p(
                "Any team that will analyze their experiments using OMIQ will need to use this app to create metadata files. This app currently accommodates plate layout templates for OptiBuild and Stability experiments."
            )
        )
    ))
}


overview_app_features <- function(){
    return(div(
        div(class = "text-primary", h3(strong("Current Features of this App Include:"))),
        tags$ul(
            style="font-size: 20px;",
            tags$li("Ability to select from multiple experiment types (OptiBuild, Stability, LEGO), which are used to generate different plate layout templates"),
            tags$li("Autofill model system data based on clone selected (taken from BV421 Database, currently Excel copy of database but future to be hooked up to database)"),
            tags$li("Autofill titrations or manually enter"),
            tags$li("Excel-like table for easy drag and drop capabilities when copying data over from different cells"),
            tags$li("Drop down menus in table for more standardized inputs"),
            tags$li("Error handling"),
            tags$li("Downloadable metadata csv outline (for plate layout planning and saving before running experiment on cytometer and generating fcs files)"),
            tags$li("Automatically read fcs filenames and map to plate well IDs on metadata outline"),
            tags$li("Dropdown of parameters, conditional on cytometer selection"),
            tags$li("Ability to add notes post-experiment"),
            tags$li("Ability to download final metadata csv to location of your choice"),
            tags$li("Ability to select gating controls on well-to-well basis")
        ),
        div(class = "text-primary", h3(strong("Future Features of this App Will Include:"))),
        tags$ul(
            style="font-size: 20px;",
            tags$li("Ability to save plate data to database from the app, rather than download csv outline"),
            tags$li("Ability to run OMIQ pipeline from the app")
        ),
        div(class = "text-primary", h3(strong("Out of Scope for this App:"))),
        tags$ul(
            style="font-size: 20px;",
            tags$li("Automatically designing and optimizing plate layout (initial experimental planning)")
        )
        
    ))
}

instructions_overview <- function(){
    return(div(
        div(class = "text-primary", h3(strong("Overview Instructions:"))),
        
        tags$ol(
            style = "font-size: 20px;",
            tags$li(
                "Navigate to",
                span("Plate Layout", style = "font-style: italic;"),
                " tab and fill out all experimental data related to the layout of your plate and experiment.",
                span(
                    "See Plate Layout Instructions for more details.",
                    class = "text-primary",
                    style = "font-style: italic;"
                )
            ),
            
            tags$li("Save data and download metadata outline."),
            
            tags$li(
                "Navigate to",
                span("Run OMIQ", style = "font-style: italic;"),
                " tab and fill out remaining data related to experiment post-Hamilton/cytometer run.span",
                span(
                    "See Run OMIQ Instructions for more details.",
                    class = "text-primary",
                    style = "font-style: italic;"
                )
            ),
            
            tags$li("Save data and download final metadata CSV file. Save file to local directory where you will be running OMIQ analysis.")
        )
    ))
}

instructions_plate_layout_tab <- function(){
    return(div(
        div(class = "text-primary",
            h3(strong(
                span("Plate Layout", style = "font-style: italic;"),
                " Instructions:"
            ))),
        tags$ol(
            style = "font-size: 20px;",
            tags$li("Select"),
            tags$ol(type="a",
                    tags$li("Enter Plate ID, Prep Date and Stain Date."),
                    tags$li("Select experiment type (OB = OptiBuild, S = Stability, NCD = New Content Development). This will dictate the plate layout template used in the app."),
                    tags$li("Fill in your model system data. This data is based on BV421 Database data. The population names will determine where the app is reading the spec range values from and which spec ranges correspond to which cell populations."),
                    tags$li("Select your clone(s). If running a Stability plate, you will select 1 clone from the second row of the sidebar panel, and each row of the plate will reflect a different time point. If running an OptiBuild plate, you will select a different clone per row.", span(strong("Note: Select Clone and Model System Data from drop downs before filling in data in the table. Every time you select a new clone, sample type, or population name, the table data will reset.")))
            ),
            tags$li("Enter your titrations and units. The first column will automatically be assigned unstained."),
            tags$li("Fill in all plate data in the Excel-like table. Make sure you only enter 8 rows of data (if you accidentally create a new row, delete it)."),
            tags$li("When you are done, save your data and preview the table. Then, download your metadata outline csv, and save it to a location you will remember. You will upload this file in the Run OMIQ tab after you have completed running your experiment and are ready to analyze your data.")
        )

    ))
}

instructions_plate_layout_tab_stability <- function(){
    return(div(
        div(class = "text-primary",
            h3(strong(
                span("Stability", style = "font-style: italic;"),
                " Template Instructions:"
            ))),
        
        tags$ol(
            style = "font-size: 20px;",
            tags$li("For the Experiment Type, select S for the Stability template."),
            tags$li("Select whether the time study is for 2 or 5 years."),
            tags$li("Select the Clone name. This applies to the entire plate."),
            tags$li("Fill in the model system data. This will apply to all rows of the plate."),
            tags$li("Select the time point for each row of the plate."),
            tags$li("Select the titrations, order, and units. This applies to each column of the plate."),
            tags$li("In the Excel-like table, fill in any missing columns. The following columns are required to run OMIQ:"),
            tags$ol(type="a",
                    
                    tags$li("Gating Control"),
                    tags$li("Stability Time point"),
                    tags$li("Specificity (CD)"),
                    tags$li("Clone"),
                    tags$li("Fluorochrome"),
                    tags$li("spec1_range, spec2_range, spec3_range"),
                    tags$li("Gating Method"),
                    tags$li("Gating Argument")),
            tags$li("Save results."),
            tags$li("Preview table (optional), then download Metadata Outline."),
            tags$li("Optionally, download the Plate Layout data (data in the Excel-like table) for reference."),
            tags$li("Go to Run OMIQ tab after you have run experiment on cytometer and are ready to analyze data.")
            
        )
        
    ))
}

instructions_plate_layout_tab_optibuild <- function(){
    return(div(
        div(class = "text-primary",
            h3(strong(
                span("OptiBuild", style = "font-style: italic;"),
                " Template Instructions:"
            ))),
        tags$ol(
            style = "font-size: 20px;",
            tags$li("For the Experiment Type, select OB for the OptiBuild template."),
            tags$li("For each row of the plate, select Sample or Control on the sidebar panel."),
            tags$li("For each row of the plate, select the Clone name on the sidebar panel. Most data from the BV421 database will auto-populate to the plate on the main panel. If the clone is not listed, type in the clone name and select 'Add' when the option comes up. You will then have to manually fill in the model system data. If it is a control row, a clone name should still be listed. However, the Specificity (CD) and (non-CD) columns should be 'NA'."),
            tags$li("Fill in the model system data. This will apply to all rows of the plate."),
            tags$li("Select the titrations, order, and units. This applies to each column of the plate."),
            tags$li("In the Excel-like table, fill in any missing columns. The following columns are required to run OMIQ:"),
            tags$ol(type="a",
                    
                    tags$li("Gating Control"),
                    tags$li("Stability Time point"),
                    tags$li("Specificity (CD)"),
                    tags$li("Clone"),
                    tags$li("Fluorochrome"),
                    tags$li("spec1_range, spec2_range, spec3_range"),
                    tags$li("Gating Method"),
                    tags$li("Gating Argument")),
            tags$li("Save results."),
            tags$li("Preview table (optional), then download Metadata Outline."),
            tags$li("Optionally, download the Plate Layout data (data in the Excel-like table) for reference."),
            tags$li("Go to Run OMIQ tab after you have run experiment on cytometer and are ready to analyze data.")
            
        )
        
    ))
}

instructions_plate_layout_tab_lego <- function(){
    return(div(
        div(class = "text-primary",
            h3(strong(
                span("LEGO", style = "font-style: italic;"),
                " Template Instructions:"
            ))),
        tags$ol(
            style = "font-size: 20px;",
            tags$li("For the Experiment Type, select LEGO for the LEGO template."),
            tags$li("Each tab refers to a different reagent # (which includes a titration of the test antibody, test isotype, 1 concentration of a Reference A antibody and isotype, and 1 concentration of a Reference B antibody and isotype. For each tab, the antibody and isotype tables must be filled in AND SAVED before the metadata outline file can be downloaded."),
            tags$li("For each tab, select a clone name for the test antibody and both references. The reference clones will default to the test antibody clone name, but can be changed."),
            tags$li("When the clone names are selected, the model system data will be auto-populated into the antibody table below, where each row refers to data for the antibody and both references. Once any missing data is filled in, click Save."),
            tags$li("Fill in any missing data in the isotype tables as well, where each row refers to the isotype for the test and two references. Most other model system data for the isotypes will be taken from the antibody table above on the backend. Once any missing data is filled in, click Save."),
            tags$li("Select the # of titrations, the titration order, and the starting titration concentration. The units must be in ug/test. The titration options are: 0.015, 0.03, 0.06, 0.125, 0.25, 0.5, 1, 2 ug/test."),
            tags$li("Repeat these steps for each Reagent tab. Ensure that the following columns in the Antibody tables are filled in, since the following columns are required to run OMIQ:"),
            tags$ol(type="a",
                    tags$li("Gating Control"),
                    tags$li("Stability Time point"),
                    tags$li("Specificity (CD)"),
                    tags$li("Clone"),
                    tags$li("Fluorochrome"),
                    tags$li("spec1_range, spec2_range, spec3_range"),
                    tags$li("Gating Method"),
                    tags$li("Gating Argument")),
            tags$li("Save results at the bottom, once all 4 tabs have been populated with data and the Save buttons were clicked for all 8 tables."),
            tags$li("Preview table (optional), then download Metadata Outline."),
            tags$li("Optionally, download the Plate Layout data (data in the Excel-like table) for reference."),
            tags$li("Go to Run OMIQ tab after you have run experiment on cytometer and are ready to analyze data.")
            
        )
        
    ))
}

instructions_run_omiq_tab <- function(){
    return(div(
        div(class = "text-primary",
            h3(strong(
                span("Run OMIQ", style = "font-style: italic;"),
                " Instructions:"
            ))),
        tags$ol(
            style = "font-size: 20px;",
            tags$li("Upload your metadata outline csv file generated from the previous tab."),
            tags$li("Upload all fcs files used in this experiment."),
            tags$li("Enter the Donor ID (if applicable)."),
            tags$li("Select the Cytometer used in the experiment, and select the parameter name for each row in the table to the right of the Cytometer dropdown."),
            tags$li("Add any experiment-related notes (e.g. Did you have to adjust the voltage on the cytometer?)."),
            tags$li("Save your data."),
            tags$li("Download your final metadata csv to the local directory from where you will analyze your experimental data using OMIQ.")
        )
    ))
}

instructions_mapping_fcsfiles <- function(){
    return(div(
        div(class = "text-primary",
            h3(strong(
                span("Mapping FCS Filenames to Well IDs", style = "font-style: italic;"),
                " - How It Works:"
            ))),
        tags$ol(
            style = "font-size: 20px;",
            tags$li("User either uploads all FCS files used in experiment OR selects to use default naming."),
                tags$ol(type="a",
                        tags$li("Default naming takes the convention: ", 
                                span(class = "text-primary", "Specimen_001_A1_A01_002.fcs"), 
                                "to ", 
                                span(class = "text-primary", "Specimen_001_H11_H11_096.fcs"),
                                " from wells ",
                                span(class = "text-primary", "A1"),
                                " to ",
                                span(class = "text-primary", "H11"),
                                " (",
                                span(class = "text-primary", "H12"),
                                " excluded because usually for CS&T beads and naming varies)")
                ),
            
            tags$li("Given list of 96 well IDs (from ",
                span(class = "text-primary", "A1 to H12"),
                "), script will loop through each file name and well ID and check if one of the well IDs + ",
                span(class = "text-primary", "_"),
                " is found in the filename (for example, filename", 
                span(class = "text-primary", "Specimen_001_A1_A01_002.fcs"),
                " will map to Well ID ",
                span(class = "text-primary", "A1"),
                " because it has found ",
                span(class = "text-primary", 'A1_'),
                " in the filename). If it finds a match, it will assign the Well ID with the matched filename. If no match is found, it will assign the Well ID with ",
                span(class = "text-primary", 'NA'),
                ".")
           
        )
    ))
}

access <- function(){
    return(div(div(class="text-primary", h3(strong("How to Access the App:\n"))),
               div(class="lead", style="font-size: 20px;",p("The app is hosted on RStudio Connect for the time being (will eventually be moved to a Docker container on an Azure Virtual Machine). You can access the app using this URL: ",a("https://bdstattools.bdx.com:3939/bdb-metadata-file-shiny-app/",href="https://bdstattools.bdx.com:3939/bdb-metadata-file-shiny-app/")," (must be on VPN)."),
                   br(),
                   p("You can also access it via Maxwell: ",a("https://maxwell.bd.com/sites/statistics/SitePage/242101/statistics-engineering-apps",href="https://maxwell.bd.com/sites/statistics/SitePage/242101/statistics-engineering-apps"),". Or you can search for Custom Apps on Maxwell. Under Apps Under Development (Not Validated), you will find a button that also takes you to the Metadata App:"),
                   img(src = "maxwell_app_image.png", height = 250, width = 250))))
}

contacts <- function(){
    return(div(div(class="lead",
                   p(span(class="text-primary",strong("For App-Related Questions: ")), 
                     span("Contact Rachel Molloy (Rachel.Molloy@bd.com)"))
    ),
    div(class="lead",
        p(span(class="text-primary",strong("For OMIQ/Plate Layout Questions: ")), 
          span("Contact Jennifer Chuddy (Jennifer.Chuddy@bd.com)"))
    ),
    div(class="lead",
        p(span(class="text-primary",strong("For Feedback/Feature Requests: ")), 
          span("Contact Rachel Molloy (Rachel.Molloy@bd.com) or put in a ticket in the ",
               a(style="font-style: bold;","Plate Layout Tasks Board",href="http://shiny.rstudio.com")," on the Reagents Automation Workstream Teams Page.")))))
}