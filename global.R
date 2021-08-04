# Fixed lists for table
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

## check azure file disk 'datadump' for testing locally and production
datadump = "/datadump/data"
if (!dir.exists(datadump)) {
    datadump = "Z:/data"
}

########################  FIND SPEC DATA FROM BV421 DB ############################
find_spec_data <- function(df, sample_type, pop_1, pop_2, pop_3){


    df <- df %>% mutate_if(is.factor, as.character)

    df <- cbind(df, spec1_name=rep(NA, nrow(df)), spec1_range=rep(NA, nrow(df)), spec1_pos_neg=rep(NA, nrow(df)), spec2_name=rep(NA, nrow(df)), spec2_range=rep(NA, nrow(df)), spec2_pos_neg=rep(NA, nrow(df)), spec3_name=rep(NA, nrow(df)), spec3_range=rep(NA, nrow(df)), spec3_pos_neg=rep(NA, nrow(df)))

    specs_col_ind_start <- which(colnames(df) == "Positive1_Name")
    specs_col_ind_end <- which(colnames(df) == "%Negative3")


    for(clo in df$Clone){
        df_filt <- filter(df, Clone == clo)

        if(dim(df_filt)[1] > 0){ #as long as clone selected exists in bv421 reference db

            ## Filter out subset of the BV421 database, including the Sample Type, and all columns that list spec names and ranges
            model_sys_filt <- tibble('Sample_Type'=df_filt$`Sample Type`, df_filt[specs_col_ind_start:specs_col_ind_end])

            ## Will find the column index in the BV421 subset where the cell pop name matches the cell pop name selected from the UI
            ## Outputs a vector with the spec name and whether it was from the Positive or Negative columns
            find_spec_name <- function(cell_pop){

                spec_name_ind <- which(grepl(as.character(cell_pop), model_sys_filt[2:ncol(model_sys_filt)], fixed=TRUE), arr.ind = TRUE)

                if(length(spec_name_ind)==0){
                    spec_name <- NA
                    spec_pos_neg <- NA
                }
                else{
                    spec_name <- model_sys_filt[2:ncol(model_sys_filt)][[spec_name_ind]]
                    spec_pos_neg <- ifelse(grepl("Positive", colnames(model_sys_filt[,spec_name_ind+1])), "Positive", "Negative")
                }
                spec_data <- c(spec_name, spec_pos_neg)
                return(spec_data)

            }

            ## Will find the column index in the BV421 subset where the cell pop name matches the cell pop name selected from the UI
            ## Then pulls the spec range from the column index to the right of the matched cell pop name index
            ## Outputs the spec range
            find_spec_value <- function(cell_pop){
                spec_name_ind <- which(grepl(as.character(cell_pop), model_sys_filt[2:ncol(model_sys_filt)], fixed=TRUE), arr.ind = TRUE)
                if(length(spec_name_ind)==0){
                    spec_range <- NA
                }
                else{
                    spec_range_ind <- spec_name_ind + 1
                    spec_range <- model_sys_filt[2:ncol(model_sys_filt)][[spec_range_ind]]
                }
                return(spec_range)

            }

            ## As long as a clone match exists in the BV421 DB and the spec names/range columns are not all NA
            if(!is.na(model_sys_filt$`Sample_Type`) && !is.na(model_sys_filt[2:ncol(model_sys_filt)])){

                ## Check if Sample Type column of BV421 subset matches the sample type selected from the UI
                ## If so, assign find_spec_name() and find_spec_value() outputs to appropriate columns
                if(grepl(sample_type, model_sys_filt$`Sample_Type`, fixed = TRUE)){
                    spec1_name <- ifelse(pop_1=="NA", "NA", find_spec_name(as.character(pop_1))[[1]])
                    spec1_range <- ifelse(pop_1=="NA", "NA", find_spec_value(as.character(pop_1)))
                    spec1_pos_neg <- ifelse(pop_1=="NA", "NA", find_spec_name(as.character(pop_1))[[2]])

                    spec2_name <- ifelse(pop_2=="NA", "NA", find_spec_name(as.character(pop_2))[[1]])
                    spec2_range <- ifelse(pop_2=="NA", "NA", find_spec_value(as.character(pop_2)))
                    spec2_pos_neg <- ifelse(pop_2=="NA", "NA", find_spec_name(as.character(pop_2))[[2]])

                    spec3_name <- ifelse(pop_3=="NA", "NA", find_spec_name(as.character(pop_3))[[1]])
                    spec3_range <- ifelse(pop_3=="NA", "NA", find_spec_value(as.character(pop_3)))
                    spec3_pos_neg <- ifelse(pop_3=="NA", "NA", find_spec_name(as.character(pop_3))[[2]])

                    # Add spec data to table that is passed back to shiny app
                    df$spec1_name[df$Clone == clo] <- spec1_name
                    df$spec1_range[df$Clone == clo] <- spec1_range
                    df$spec1_pos_neg[df$Clone == clo] <- spec1_pos_neg

                    df$spec2_name[df$Clone == clo] <- spec2_name
                    df$spec2_range[df$Clone == clo] <- spec2_range
                    df$spec2_pos_neg[df$Clone == clo] <- spec2_pos_neg

                    df$spec3_name[df$Clone == clo] <- spec3_name
                    df$spec3_range[df$Clone == clo] <- spec3_range
                    df$spec3_pos_neg[df$Clone == clo] <- spec3_pos_neg

                }
            }
        }
    }
    return(df)
}

########################  Configure metadata for Stability experiments ############################
stability_metadata_table <- function(bv421_df, clone_all='NA', time_A=0, time_B=0, time_C=0, time_D=0, time_E=0, time_F=0, time_G=0, time_H=0, pop1, pop2, pop3, sample_type, spec_data_function){

    ## if selected clone is listed in database, will search all other criteria based on that
    if(clone_all %in% bv421_df$Clone){
        df <- tibble(
            # 'row' = c("Row A", "Row B", "Row C", "Row D", "Row E", "Row F", "Row G", "Row H"),
            # 'gating_control' = rep(NA,8),
            'gating_control' = rep("A",8),
            'stability_time_point' = c(as.numeric(time_A), as.numeric(time_B), as.numeric(time_C), as.numeric(time_D), as.numeric(time_E), as.numeric(time_F), as.numeric(time_G), as.numeric(time_H)),
            'target_species' = rep(bv421_df$`Target Species`[bv421_df$Clone == clone_all], 8),
            'specificity_cd' = rep(bv421_df$`Specificity (CD)`[bv421_df$Clone == clone_all], 8),
            'specificity_noncd' = rep(bv421_df$`Specificity (non-CD)`[bv421_df$Clone == clone_all], 8),
            'host_spec' = rep(bv421_df$`Host Species`[bv421_df$Clone == clone_all], 8),
            'iso_heavy' = rep(bv421_df$`Isotype (Heavy Chain)`[bv421_df$Clone == clone_all], 8),
            'iso_light' = rep(bv421_df$`Isotype (Light Chain)`[bv421_df$Clone == clone_all], 8),
            'clone' = rep(bv421_df$Clone[bv421_df$Clone == clone_all], 8),
            'fluor' = rep(NA, 8),
            # 'parameter' = rep(NA,8),
            'batch_no' = rep(NA, 8),
            'spec1_name' = rep(pop1, 8),
            'spec1_range' = rep(spec_data_function$spec1_range[bv421_df$Clone == clone_all], 8),
            'pos_neg_1' = rep(spec_data_function$spec1_pos_neg[bv421_df$Clone == clone_all], 8),
            'spec2_name' = rep(pop2, 8),
            'spec2_range' = rep(spec_data_function$spec2_range[bv421_df$Clone == clone_all], 8),
            'pos_neg_2' = rep(spec_data_function$spec2_pos_neg[bv421_df$Clone == clone_all], 8),
            'spec3_name' = rep(pop3, 8),
            'spec3_range' = rep(spec_data_function$spec3_range[bv421_df$Clone == clone_all], 8),
            'pos_neg_3' = rep(spec_data_function$spec3_pos_neg[bv421_df$Clone == clone_all], 8),
            'gating_method' = rep("sd", 8),
            'gating_argument' = rep(3, 8),
            'optimal' = rep(bv421_df$`BV421 Optimal`[bv421_df$Clone == clone_all], 8),
            # 'optimal_units' = rep(NA, 8),
            'bv421_SI' = rep(bv421_df$`BV421 SI`[bv421_df$Clone == clone_all], 8)
        )
    }
    ## if clone not found in database (either null, "" or a new clone added to dropdown), will fill in every other field as NA except clone, which will be whatever was entered by user
    else{
        df <- tibble(
            'gating_control' = rep("A",8),
            'stability_time_point' = c(as.numeric(time_A), as.numeric(time_B), as.numeric(time_C), as.numeric(time_D), as.numeric(time_E), as.numeric(time_F), as.numeric(time_G), as.numeric(time_H)),
            'target_species' = rep(NA, 8),
            'specificity_cd' = rep(NA, 8),
            'specificity_noncd' = rep(NA, 8),
            'host_spec' = rep(NA, 8),
            'iso_heavy' = rep(NA, 8),
            'iso_light' = rep(NA, 8),
            'clone' = rep(clone_all, 8),
            'fluor' = rep(NA, 8),
            # 'parameter' = rep(NA,8),
            'batch_no' = rep(NA, 8),
            'spec1_name' = rep(pop1, 8),
            'spec1_range' = rep(NA, 8),
            'pos_neg_1' = rep(NA, 8),
            'spec2_name' = rep(pop2, 8),
            'spec2_range' = rep(NA, 8),
            'pos_neg_2' = rep(NA, 8),
            'spec3_name' = rep(pop3, 8),
            'spec3_range' = rep(NA, 8),
            'pos_neg_3' = rep(NA, 8),
            'gating_method' = rep("sd", 8),
            'gating_argument' = rep(3, 8),
            'optimal' = rep(NA, 8),
            # 'optimal_units' = rep(NA, 8),
            'bv421_SI' = rep(NA, 8)
        )
    }


    return(df)
}

optibuild_metadata_table <- function(bv421_df, clone_A='NA', clone_B='NA', clone_C='NA', clone_D='NA', clone_E='NA', clone_F='NA', clone_G='NA', clone_H='NA', pop1, pop2, pop3, sample_type, spec_data_function, sample_or_control_A, sample_or_control_B,sample_or_control_C,sample_or_control_D,sample_or_control_E,sample_or_control_F,sample_or_control_G,sample_or_control_H){

    ## If user adds clone name to drop down list, will fill in clone column as whateever the user adds in dropdown
    clone_A_OG <- ifelse(sample_or_control_A == "Sample", clone_A, "NA")
    clone_B_OG <- ifelse(sample_or_control_B == "Sample", clone_B, "NA")
    clone_C_OG <- ifelse(sample_or_control_C == "Sample", clone_C, "NA")
    clone_D_OG <- ifelse(sample_or_control_D == "Sample", clone_D, "NA")
    clone_E_OG <- ifelse(sample_or_control_E == "Sample", clone_E, "NA")
    clone_F_OG <- ifelse(sample_or_control_F == "Sample", clone_F, "NA")
    clone_G_OG <- ifelse(sample_or_control_G == "Sample", clone_G, "NA")
    clone_H_OG <- ifelse(sample_or_control_H == "Sample", clone_H, "NA")

    ## Referenced in all other fields so that if new clone added or no clone listed, will default to clone = NA, where all other columns also = NA
    clone_A <- ifelse(clone_A %in% bv421_df$Clone, clone_A, "NA")
    clone_B <- ifelse(clone_B %in% bv421_df$Clone, clone_B, "NA")
    clone_C <- ifelse(clone_C %in% bv421_df$Clone, clone_C, "NA")
    clone_D <- ifelse(clone_D %in% bv421_df$Clone, clone_D, "NA")
    clone_E <- ifelse(clone_E %in% bv421_df$Clone, clone_E, "NA")
    clone_F <- ifelse(clone_F %in% bv421_df$Clone, clone_F, "NA")
    clone_G <- ifelse(clone_G %in% bv421_df$Clone, clone_G, "NA")
    clone_H <- ifelse(clone_H %in% bv421_df$Clone, clone_H, "NA")

    clone_A <- ifelse(sample_or_control_A == "Sample", clone_A, "NA")
    clone_B <- ifelse(sample_or_control_B == "Sample", clone_B, "NA")
    clone_C <- ifelse(sample_or_control_C == "Sample", clone_C, "NA")
    clone_D <- ifelse(sample_or_control_D == "Sample", clone_D, "NA")
    clone_E <- ifelse(sample_or_control_E == "Sample", clone_E, "NA")
    clone_F <- ifelse(sample_or_control_F == "Sample", clone_F, "NA")
    clone_G <- ifelse(sample_or_control_G == "Sample", clone_G, "NA")
    clone_H <- ifelse(sample_or_control_H == "Sample", clone_H, "NA")

    df <- tibble(

        'gating_control'=c(
            ifelse(sample_or_control_A == "Sample", NA, "NA"),
            ifelse(sample_or_control_B == "Sample", NA, "NA"),
            ifelse(sample_or_control_C == "Sample", NA, "NA"),
            ifelse(sample_or_control_D == "Sample", NA, "NA"),
            ifelse(sample_or_control_E == "Sample", NA, "NA"),
            ifelse(sample_or_control_F == "Sample", NA, "NA"),
            ifelse(sample_or_control_G == "Sample", NA, "NA"),
            ifelse(sample_or_control_H == "Sample", NA, "NA")
        ),
        'stability_time_point' = rep(0,8),
        'target_species' = c(
            bv421_df$`Target Species`[bv421_df$Clone == clone_A],
            bv421_df$`Target Species`[bv421_df$Clone == clone_B],
            bv421_df$`Target Species`[bv421_df$Clone == clone_C],
            bv421_df$`Target Species`[bv421_df$Clone == clone_D],
            bv421_df$`Target Species`[bv421_df$Clone == clone_E],
            bv421_df$`Target Species`[bv421_df$Clone == clone_F],
            bv421_df$`Target Species`[bv421_df$Clone == clone_G],
            bv421_df$`Target Species`[bv421_df$Clone == clone_H]
        ),
        'specificity_cd' = c(
            bv421_df$`Specificity (CD)`[bv421_df$Clone == clone_A],
            bv421_df$`Specificity (CD)`[bv421_df$Clone == clone_B],
            bv421_df$`Specificity (CD)`[bv421_df$Clone == clone_C],
            bv421_df$`Specificity (CD)`[bv421_df$Clone == clone_D],
            bv421_df$`Specificity (CD)`[bv421_df$Clone == clone_E],
            bv421_df$`Specificity (CD)`[bv421_df$Clone == clone_F],
            bv421_df$`Specificity (CD)`[bv421_df$Clone == clone_G],
            bv421_df$`Specificity (CD)`[bv421_df$Clone == clone_H]
        ),
        'specificity_noncd' = c(
            bv421_df$`Specificity (non-CD)`[bv421_df$Clone == clone_A],
            bv421_df$`Specificity (non-CD)`[bv421_df$Clone == clone_B],
            bv421_df$`Specificity (non-CD)`[bv421_df$Clone == clone_C],
            bv421_df$`Specificity (non-CD)`[bv421_df$Clone == clone_D],
            bv421_df$`Specificity (non-CD)`[bv421_df$Clone == clone_E],
            bv421_df$`Specificity (non-CD)`[bv421_df$Clone == clone_F],
            bv421_df$`Specificity (non-CD)`[bv421_df$Clone == clone_G],
            bv421_df$`Specificity (non-CD)`[bv421_df$Clone == clone_H]
        ),
        'host_spec' = c(
            bv421_df$`Host Species`[bv421_df$Clone == clone_A],
            bv421_df$`Host Species`[bv421_df$Clone == clone_B],
            bv421_df$`Host Species`[bv421_df$Clone == clone_C],
            bv421_df$`Host Species`[bv421_df$Clone == clone_D],
            bv421_df$`Host Species`[bv421_df$Clone == clone_E],
            bv421_df$`Host Species`[bv421_df$Clone == clone_F],
            bv421_df$`Host Species`[bv421_df$Clone == clone_G],
            bv421_df$`Host Species`[bv421_df$Clone == clone_H]
        ),
        'iso_heavy' = c(
            bv421_df$`Isotype (Heavy Chain)`[bv421_df$Clone == clone_A],
            bv421_df$`Isotype (Heavy Chain)`[bv421_df$Clone == clone_B],
            bv421_df$`Isotype (Heavy Chain)`[bv421_df$Clone == clone_C],
            bv421_df$`Isotype (Heavy Chain)`[bv421_df$Clone == clone_D],
            bv421_df$`Isotype (Heavy Chain)`[bv421_df$Clone == clone_E],
            bv421_df$`Isotype (Heavy Chain)`[bv421_df$Clone == clone_F],
            bv421_df$`Isotype (Heavy Chain)`[bv421_df$Clone == clone_G],
            bv421_df$`Isotype (Heavy Chain)`[bv421_df$Clone == clone_H]
        ),
        'iso_light' = c(
            bv421_df$`Isotype (Light Chain)`[bv421_df$Clone == clone_A],
            bv421_df$`Isotype (Light Chain)`[bv421_df$Clone == clone_B],
            bv421_df$`Isotype (Light Chain)`[bv421_df$Clone == clone_C],
            bv421_df$`Isotype (Light Chain)`[bv421_df$Clone == clone_D],
            bv421_df$`Isotype (Light Chain)`[bv421_df$Clone == clone_E],
            bv421_df$`Isotype (Light Chain)`[bv421_df$Clone == clone_F],
            bv421_df$`Isotype (Light Chain)`[bv421_df$Clone == clone_G],
            bv421_df$`Isotype (Light Chain)`[bv421_df$Clone == clone_H]
        ),
        'clone' = c(
            clone_A_OG,
            clone_B_OG,
            clone_C_OG,
            clone_D_OG,
            clone_E_OG,
            clone_F_OG,
            clone_G_OG,
            clone_H_OG
        ),
        'fluor' = rep(NA, 8),
        'batch_no' = rep(NA, 8),
        'spec1_name' = c(
            ifelse(sample_or_control_A == "Sample", pop1, "NA"),
            ifelse(sample_or_control_B == "Sample", pop1, "NA"),
            ifelse(sample_or_control_C == "Sample", pop1, "NA"),
            ifelse(sample_or_control_D == "Sample", pop1, "NA"),
            ifelse(sample_or_control_E == "Sample", pop1, "NA"),
            ifelse(sample_or_control_F == "Sample", pop1, "NA"),
            ifelse(sample_or_control_G == "Sample", pop1, "NA"),
            ifelse(sample_or_control_H == "Sample", pop1, "NA")
        ),

        'spec1_range' = c(
            spec_data_function$spec1_range[bv421_df$Clone == clone_A],
            spec_data_function$spec1_range[bv421_df$Clone == clone_B],
            spec_data_function$spec1_range[bv421_df$Clone == clone_C],
            spec_data_function$spec1_range[bv421_df$Clone == clone_D],
            spec_data_function$spec1_range[bv421_df$Clone == clone_E],
            spec_data_function$spec1_range[bv421_df$Clone == clone_F],
            spec_data_function$spec1_range[bv421_df$Clone == clone_G],
            spec_data_function$spec1_range[bv421_df$Clone == clone_H]
        ),
        'pos_neg_1' = c(
            spec_data_function$spec1_pos_neg[bv421_df$Clone == clone_A],
            spec_data_function$spec1_pos_neg[bv421_df$Clone == clone_B],
            spec_data_function$spec1_pos_neg[bv421_df$Clone == clone_C],
            spec_data_function$spec1_pos_neg[bv421_df$Clone == clone_D],
            spec_data_function$spec1_pos_neg[bv421_df$Clone == clone_E],
            spec_data_function$spec1_pos_neg[bv421_df$Clone == clone_F],
            spec_data_function$spec1_pos_neg[bv421_df$Clone == clone_G],
            spec_data_function$spec1_pos_neg[bv421_df$Clone == clone_H]
        ),
        'spec2_name' = c(
            ifelse(sample_or_control_A == "Sample", pop2, "NA"),
            ifelse(sample_or_control_B == "Sample", pop2, "NA"),
            ifelse(sample_or_control_C == "Sample", pop2, "NA"),
            ifelse(sample_or_control_D == "Sample", pop2, "NA"),
            ifelse(sample_or_control_E == "Sample", pop2, "NA"),
            ifelse(sample_or_control_F == "Sample", pop2, "NA"),
            ifelse(sample_or_control_G == "Sample", pop2, "NA"),
            ifelse(sample_or_control_H == "Sample", pop2, "NA")
        ),
        'spec2_range' = c(
            spec_data_function$spec2_range[bv421_df$Clone == clone_A],
            spec_data_function$spec2_range[bv421_df$Clone == clone_B],
            spec_data_function$spec2_range[bv421_df$Clone == clone_C],
            spec_data_function$spec2_range[bv421_df$Clone == clone_D],
            spec_data_function$spec2_range[bv421_df$Clone == clone_E],
            spec_data_function$spec2_range[bv421_df$Clone == clone_F],
            spec_data_function$spec2_range[bv421_df$Clone == clone_G],
            spec_data_function$spec2_range[bv421_df$Clone == clone_H]
        ),
        'pos_neg_2' = c(
            spec_data_function$spec2_pos_neg[bv421_df$Clone == clone_A],
            spec_data_function$spec2_pos_neg[bv421_df$Clone == clone_B],
            spec_data_function$spec2_pos_neg[bv421_df$Clone == clone_C],
            spec_data_function$spec2_pos_neg[bv421_df$Clone == clone_D],
            spec_data_function$spec2_pos_neg[bv421_df$Clone == clone_E],
            spec_data_function$spec2_pos_neg[bv421_df$Clone == clone_F],
            spec_data_function$spec2_pos_neg[bv421_df$Clone == clone_G],
            spec_data_function$spec2_pos_neg[bv421_df$Clone == clone_H]
        ),
        'spec3_name' = c(
            ifelse(sample_or_control_A == "Sample", pop3, "NA"),
            ifelse(sample_or_control_B == "Sample", pop3, "NA"),
            ifelse(sample_or_control_C == "Sample", pop3, "NA"),
            ifelse(sample_or_control_D == "Sample", pop3, "NA"),
            ifelse(sample_or_control_E == "Sample", pop3, "NA"),
            ifelse(sample_or_control_F == "Sample", pop3, "NA"),
            ifelse(sample_or_control_G == "Sample", pop3, "NA"),
            ifelse(sample_or_control_H == "Sample", pop3, "NA")
        ),
        'spec3_range' = c(
            spec_data_function$spec3_range[bv421_df$Clone == clone_A],
            spec_data_function$spec3_range[bv421_df$Clone == clone_B],
            spec_data_function$spec3_range[bv421_df$Clone == clone_C],
            spec_data_function$spec3_range[bv421_df$Clone == clone_D],
            spec_data_function$spec3_range[bv421_df$Clone == clone_E],
            spec_data_function$spec3_range[bv421_df$Clone == clone_F],
            spec_data_function$spec3_range[bv421_df$Clone == clone_G],
            spec_data_function$spec3_range[bv421_df$Clone == clone_H]
        ),
        'pos_neg_3' = c(
            spec_data_function$spec3_pos_neg[bv421_df$Clone == clone_A],
            spec_data_function$spec3_pos_neg[bv421_df$Clone == clone_B],
            spec_data_function$spec3_pos_neg[bv421_df$Clone == clone_C],
            spec_data_function$spec3_pos_neg[bv421_df$Clone == clone_D],
            spec_data_function$spec3_pos_neg[bv421_df$Clone == clone_E],
            spec_data_function$spec3_pos_neg[bv421_df$Clone == clone_F],
            spec_data_function$spec3_pos_neg[bv421_df$Clone == clone_G],
            spec_data_function$spec3_pos_neg[bv421_df$Clone == clone_H]
        ),
        'gating_method' = rep("sd", 8),
        'gating_argument' = rep(3, 8),
        'optimal' = rep(NA, 8),
        'bv421_SI' = c(
            bv421_df$`BV421 SI`[bv421_df$Clone == clone_A],
            bv421_df$`BV421 SI`[bv421_df$Clone == clone_B],
            bv421_df$`BV421 SI`[bv421_df$Clone == clone_C],
            bv421_df$`BV421 SI`[bv421_df$Clone == clone_D],
            bv421_df$`BV421 SI`[bv421_df$Clone == clone_E],
            bv421_df$`BV421 SI`[bv421_df$Clone == clone_F],
            bv421_df$`BV421 SI`[bv421_df$Clone == clone_G],
            bv421_df$`BV421 SI`[bv421_df$Clone == clone_H]
        )
    )

    return(df)
}

lego_metadata_table <- function(bv421_df, clone_test='NA', clone_ref_a='NA', clone_ref_b='NA', pop1, pop2, pop3, sample_type, spec_data_function){

    num_rows <- 3
    ## Referenced in all other fields so that if new clone added or no clone listed, will default to clone = NA, where all other columns also = NA
    clone_test <- ifelse(clone_test %in% bv421_df$Clone, clone_test, "NA")
    clone_ref_a <- ifelse(clone_ref_a %in% bv421_df$Clone, clone_ref_a, "NA")
    clone_ref_b <- ifelse(clone_ref_b %in% bv421_df$Clone, clone_ref_b, "NA")

    df <- tibble(

        'target_species' = c(
            bv421_df$`Target Species`[bv421_df$Clone == clone_test],
            bv421_df$`Target Species`[bv421_df$Clone == clone_ref_a],
            bv421_df$`Target Species`[bv421_df$Clone == clone_ref_b]
        ),
        'specificity_cd' = c(
            bv421_df$`Specificity (CD)`[bv421_df$Clone == clone_test],
            bv421_df$`Specificity (CD)`[bv421_df$Clone == clone_ref_a],
            bv421_df$`Specificity (CD)`[bv421_df$Clone == clone_ref_b]
        ),
        'specificity_noncd' = c(
            bv421_df$`Specificity (non-CD)`[bv421_df$Clone == clone_test],
            bv421_df$`Specificity (non-CD)`[bv421_df$Clone == clone_ref_a],
            bv421_df$`Specificity (non-CD)`[bv421_df$Clone == clone_ref_b]
        ),
        'host_spec' = c(
            bv421_df$`Host Species`[bv421_df$Clone == clone_test],
            bv421_df$`Host Species`[bv421_df$Clone == clone_ref_a],
            bv421_df$`Host Species`[bv421_df$Clone == clone_ref_b]
        ),
        'iso_heavy' = c(
            bv421_df$`Isotype (Heavy Chain)`[bv421_df$Clone == clone_test],
            bv421_df$`Isotype (Heavy Chain)`[bv421_df$Clone == clone_ref_a],
            bv421_df$`Isotype (Heavy Chain)`[bv421_df$Clone == clone_ref_b]
        ),
        'iso_light' = c(
            bv421_df$`Isotype (Light Chain)`[bv421_df$Clone == clone_test],
            bv421_df$`Isotype (Light Chain)`[bv421_df$Clone == clone_ref_a],
            bv421_df$`Isotype (Light Chain)`[bv421_df$Clone == clone_ref_b]
        ),
        'clone' = c(
            clone_test,
            clone_ref_a,
            clone_ref_b
        ),
        'spec1_name' = rep(pop1, num_rows),

        'spec1_range' = c(
            spec_data_function$spec1_range[bv421_df$Clone == clone_test],
            spec_data_function$spec1_range[bv421_df$Clone == clone_ref_a],
            spec_data_function$spec1_range[bv421_df$Clone == clone_ref_b]
        ),
        'pos_neg_1' = c(
            spec_data_function$spec1_pos_neg[bv421_df$Clone == clone_test],
            spec_data_function$spec1_pos_neg[bv421_df$Clone == clone_ref_a],
            spec_data_function$spec1_pos_neg[bv421_df$Clone == clone_ref_b]
        ),
        'spec2_name' = rep(pop2, num_rows),
        'spec2_range' = c(
            spec_data_function$spec2_range[bv421_df$Clone == clone_test],
            spec_data_function$spec2_range[bv421_df$Clone == clone_ref_a],
            spec_data_function$spec2_range[bv421_df$Clone == clone_ref_b]
        ),
        'pos_neg_2' = c(
            spec_data_function$spec2_pos_neg[bv421_df$Clone == clone_test],
            spec_data_function$spec2_pos_neg[bv421_df$Clone == clone_ref_a],
            spec_data_function$spec2_pos_neg[bv421_df$Clone == clone_ref_b]
        ),
        'spec3_name' = rep(pop3, num_rows),
        'spec3_range' = c(
            spec_data_function$spec3_range[bv421_df$Clone == clone_test],
            spec_data_function$spec3_range[bv421_df$Clone == clone_ref_a],
            spec_data_function$spec3_range[bv421_df$Clone == clone_ref_b]
        ),
        'pos_neg_3' = c(
            spec_data_function$spec3_pos_neg[bv421_df$Clone == clone_test],
            spec_data_function$spec3_pos_neg[bv421_df$Clone == clone_ref_a],
            spec_data_function$spec3_pos_neg[bv421_df$Clone == clone_ref_b]
        ),
        'gating_method' = rep("sd", num_rows),
        'gating_argument' = rep(3, num_rows),
        'bv421_SI' = c(
            bv421_df$`BV421 SI`[bv421_df$Clone == clone_test],
            bv421_df$`BV421 SI`[bv421_df$Clone == clone_ref_a],
            bv421_df$`BV421 SI`[bv421_df$Clone == clone_ref_b]
        )
    )
    return(df)
}

row_tibble <- function(plate_data_per_row, experiment_type, units){

    df <- tibble(
        row_list = c('Row A', 'Row B', 'Row C', 'Row D', 'Row E', 'Row F', 'Row G', 'Row H'),
        gating_control = plate_data_per_row$gating_control,
        stability_time_point = plate_data_per_row$stability_time_point,
        target_sp = plate_data_per_row$target_species,
        spec_cd = plate_data_per_row$specificity_cd,
        spec_noncd = plate_data_per_row$specificity_noncd,
        host_spec = plate_data_per_row$host_spec,
        iso_heavy = plate_data_per_row$iso_heavy,
        iso_light = plate_data_per_row$iso_light,
        clone = plate_data_per_row$clone,
        fluor = plate_data_per_row$fluor,
        batch_no = plate_data_per_row$batch_no,
        spec1_name = plate_data_per_row$spec1_name,
        spec1_range = plate_data_per_row$spec1_range,
        pos_neg_1 = plate_data_per_row$pos_neg_1,
        spec2_name = plate_data_per_row$spec2_name,
        spec2_range = plate_data_per_row$spec2_range,
        pos_neg_2 = plate_data_per_row$pos_neg_2,
        spec3_name = plate_data_per_row$spec3_name,
        spec3_range = plate_data_per_row$spec3_range,
        pos_neg_3 = plate_data_per_row$pos_neg_3,
        gating_method = plate_data_per_row$gating_method,
        gating_arg = plate_data_per_row$gating_argument,
        optimal = ifelse(experiment_type == "OB", rep("NA", 8), plate_data_per_row$optimal),
        optimal_units = ifelse(experiment_type == "OB", rep("NA", 8), rep("ug/test", 8)),
        bv421_SI = plate_data_per_row$bv421_SI
    )

    colnames(df) <- c("Row", "Gating Control", "Stability Time point", "Target Species", "Specificity (CD)", "Specificity (non-CD)", "Host Species", "Isotype (Heavy Chain)", "Isotype (Light Chain)", "Clone", "Fluorochrome", "Batch Number", "spec1_name", "spec1_range", "spec1: Pos or Neg?", "spec2_name", "spec2_range", "spec2: Pos or Neg?", "spec3_name", "spec3_range", "spec3: Pos or Neg?", "Gating Method", "Gating Argument", "Optimal", "Optimal units", "BV421 Stain Index")

    return(df)
}

row_tibble_lego <- function(reag_no, plate_data_per_row){

    num_rows <- 3

    if(reag_no == 1){
        reag_number <- rep("Reagent 1", 3)
        row_list <- rep('Row A', 3)
        gating_control <- c('B', 'A10', 'A12')
    }
    else if(reag_no == 2){
        reag_number <- rep("Reagent 2", 3)
        row_list <- rep('Row C', 3)
        gating_control <- c('D', 'C10', 'C12')
    }
    else if(reag_no == 3){
        reag_number <- rep("Reagent 3", 3)
        row_list <- rep('Row E', 3)
        gating_control <- c('F','E10', 'E12')
    }
    else if(reag_no == 4){
        reag_number <- rep("Reagent 4", 3)
        row_list <- rep('Row G', 3)
        gating_control <- c('H', 'G10', 'G12')
    }

    df <- tibble(
        reagent_num = reag_number,
        row_list = row_list,
        ab_type = c("Test Ab", "Ref Ab 1", "Ref Ab 2"),
        gating_control = gating_control,
        conc_mg_ml = rep(NA, num_rows),
        conc_ug_test = c("NA", rep(NA, 2)),
        stability_time_point = rep(0,num_rows),
        target_sp = plate_data_per_row$target_species,
        spec_cd = plate_data_per_row$specificity_cd,
        spec_noncd = plate_data_per_row$specificity_noncd,
        host_spec = plate_data_per_row$host_spec,
        iso_heavy = plate_data_per_row$iso_heavy,
        iso_light = plate_data_per_row$iso_light,
        clone = plate_data_per_row$clone,
        fluor = rep(NA,num_rows),
        batch_no = rep(NA,num_rows),
        catalog_material_no = rep(NA, num_rows),
        spec1_name = plate_data_per_row$spec1_name,
        spec1_range = plate_data_per_row$spec1_range,
        pos_neg_1 = plate_data_per_row$pos_neg_1,
        spec2_name = plate_data_per_row$spec2_name,
        spec2_range = plate_data_per_row$spec2_range,
        pos_neg_2 = plate_data_per_row$pos_neg_2,
        spec3_name = plate_data_per_row$spec3_name,
        spec3_range = plate_data_per_row$spec3_range,
        pos_neg_3 = plate_data_per_row$pos_neg_3,
        gating_method = plate_data_per_row$gating_method,
        gating_arg = plate_data_per_row$gating_argument,
        optimal = rep(NA,num_rows),
        optimal_units = rep("ug/test",num_rows),
        bv421_SI = plate_data_per_row$bv421_SI
    )

    colnames(df) <- c("Reagent #", "Row", "Ab Type", "Gating Control", "Concentration (mg/ml)", "Concentration (ug/test)", "Stability Time point", "Target Species", "Specificity (CD)", "Specificity (non-CD)", "Host Species", "Isotype (Heavy Chain)", "Isotype (Light Chain)", "Clone", "Fluorochrome", "Batch Number", "spec1_name", "spec1_range", "spec1: Pos or Neg?", "spec2_name", "spec2_range", "spec2: Pos or Neg?", "spec3_name", "spec3_range", "spec3: Pos or Neg?", "Gating Method", "Gating Argument", "Optimal", "Optimal units", "BV421 Stain Index")

    return(df)
}

row_tibble_lego_isotypes <- function(reag_no){
    num_rows <- 3

    if(reag_no == 1){
        reag_number <- rep("Reagent 1", 3)
        row_list <- c('Row B', "Row A", "Row A")
    }
    else if(reag_no == 2){
        reag_number <- rep("Reagent 2", 3)
        row_list <- c('Row D', "Row C", "Row C")
    }
    else if(reag_no == 3){
        reag_number <- rep("Reagent 3", 3)
        row_list <- c('Row F', "Row E", "Row E")
    }
    else if(reag_no == 4){
        reag_number <- rep("Reagent 4", 3)
        row_list <- c('Row H', "Row G", "Row G")
    }

    df <- tibble(
        reagent_num = reag_number,
        row_list = row_list,
        ab_type = c("Test Iso", "Ref Iso 1", "Ref Iso 2"),
        clone = rep(NA, num_rows),
        conc_mg = rep(2, num_rows),
        conc_ug = c("NA", rep(0.03, 2)),
        batch_no = rep(NA, num_rows),
        catalog_material_no = rep(NA, num_rows)
    )
    colnames(df) <- c("Reagent #", "Row", "Iso Type", "Clone", "Concentration (mg/ml)", "Concentration (ug/test)", "Batch Number", "Catalog #/Material #")
    return(df)
}


########################  METADATA CSV FILE OUTLINE ###############################
metadata_outline <- function(metadata_headers_filename, well_ID_columns){

    headers <- colnames(readr::read_csv(metadata_headers_filename))
    metadata <- as_tibble(data.frame(matrix(nrow=96,ncol=length(headers))))
    colnames(metadata) <- headers

    metadata$`Well ID` <- well_ID_columns$Well.ID
    metadata$`Plate Row` <- well_ID_columns$Plate.Row
    metadata$`Plate Column` <- well_ID_columns$Plate.Column %>% as.integer()

    return(metadata)
}


########################  CONFIGURE 96-WELL PLATE ################################
plate_config_wells <- function(){
    numOfColumns=c(1:12)
    wellPlateInfo <- data.frame("Well ID"=NA, "Plate Row"=NA, "Plate Column"=NA)
    controlRowList <- c("NA","A", "B", "C", "D", "E", "F", "G", "H")

    for (plateRow in controlRowList[2:length(controlRowList)]){
        for (plateCol in numOfColumns){
            wellPlateInfo <- add_row(wellPlateInfo, Well.ID=paste0(plateRow,plateCol), Plate.Row=plateRow, Plate.Column=plateCol)

        }
    }

    # Remove first row with NA
    wellPlateInfo <- drop_na(wellPlateInfo)
    return(wellPlateInfo)
}




#################### SLICE DATA ENTERED BY USER ON CLIENT SIDE AND WRITE TO NEW METADATA TIBBLE ######################
create_metadata_table <- function(df, metadata, plate_id, prep_date, experiment_type, titrateList, units, sample_type, sample_species, sample_strain, gating_control_list, use_gating_button){

    # Repeat each plate row 12 times for data for each column
    df_sliced <- df %>% slice(rep(1:n(), each=12))

    gating_control_tibble <- tibble("Gating Control" = gating_control_list)

    if(use_gating_button == TRUE){
        gating <- c(gating_control_tibble$`Gating Control`)
    }
    else{
        gating <- df_sliced$Gating.Control
    }

    # Write sliced data to metadata tibble
    metadata$`Plate ID` <- rep(plate_id, 96)
    metadata$`Prep Date` <- rep(prep_date, 96)
    # metadata$`Stain Date` <- rep(stain_date, 96)
    metadata$`Run Date` <- rep(NA_character_, 96)
    metadata$`Experiment Type` <- rep(experiment_type, 96)
    metadata$`Stability Time point` <- df_sliced$Stability.Time.point
    metadata$`Gating Control` <- gating
    metadata$`Target Species` <- df_sliced$Target.Species
    metadata$`Specificity (CD)` <- df_sliced$Specificity..CD.
    metadata$`Specificity (non-CD)` <- df_sliced$Specificity..non.CD.
    metadata$`Isotype/Host Species` <- paste(df_sliced$Host.Species, df_sliced$Isotype..Heavy.Chain., df_sliced$Isotype..Light.Chain.)
    metadata$`Host Species` <- df_sliced$Host.Species
    metadata$`Isotype (Heavy Chain)` <- df_sliced$Isotype..Heavy.Chain.
    metadata$`Isotype (Light Chain)` <- df_sliced$Isotype..Light.Chain.
    metadata$Clone <- df_sliced$Clone
    metadata$`Fluorochrome` <- df_sliced$Fluorochrome
    metadata$`Batch Number` <- df_sliced$Batch.Number
    metadata$`ug/test` <- rep(c(titrateList, rep(NA, 12-length(titrateList))),nrow(df))
    metadata$units <- rep(units, nrow(metadata))
    metadata$`Sample Type` <- rep(sample_type, 96)
    metadata$`Sample Species` <- rep(sample_species, 96)
    metadata$`Sample Strain` <- rep(sample_strain, 96)

    metadata$spec1_name <- df_sliced$spec1_name
    metadata$spec1_range <-
        ifelse(sapply(tibble(rep(NA, 96)), function(i) {
            is.na(df_sliced$spec1_range) |
                is.null(df_sliced$spec1_range) | df_sliced$spec1_range == "" | df_sliced$spec1_range == "NA"
        }),
        "NA",
        paste0('s', df_sliced$spec1_range))
    metadata$`Positive / Negative_1` <- df_sliced$spec1..Pos.or.Neg.

    metadata$spec2_name <- df_sliced$spec2_name
    metadata$spec2_range <-
        ifelse(sapply(tibble(rep(NA, 96)), function(i) {
            is.na(df_sliced$spec2_range) |
                is.null(df_sliced$spec2_range) | df_sliced$spec2_range == "" | df_sliced$spec2_range == "NA"
        }),
        "NA",
        paste0('s', df_sliced$spec2_range))
    metadata$`Positive / Negative_2` <- df_sliced$spec2..Pos.or.Neg.


    metadata$spec3_name <- df_sliced$spec3_name
    metadata$spec3_range <-
        ifelse(sapply(tibble(rep(NA, 96)), function(i) {
            is.na(df_sliced$spec3_range) |
                is.null(df_sliced$spec3_range) | df_sliced$spec3_range == "" | df_sliced$spec3_range == "NA"
        }),
        "NA",
        paste0('s', df_sliced$spec3_range))

    metadata$`Positive / Negative_3` <- df_sliced$spec3..Pos.or.Neg.

    metadata$gating_method <- df_sliced$Gating.Method
    metadata$gating_argument <- df_sliced$Gating.Argument

    metadata$`Optimal with units` <- paste(df_sliced$Optimal, df_sliced$Optimal.units)
    metadata$`Optimal with units` <- ifelse(sapply(tibble(rep(NA, 96)), function(i) {
        metadata$`Optimal with units` == "NA NA"
    }),
    "NA",
    paste(df_sliced$Optimal, df_sliced$Optimal.units))

    metadata$`CS&T Beads Lot #` <- rep(NA_character_, 96)

    ## Changing column 1 of plate to NA for all fields because unstained and not used
    ## For final metadata creation, all fields NA for any wash columns as well
    if(experiment_type == "S"){
        col_ind_start <- which(colnames(metadata) == "Gating Control")
        col_ind_end <- which(colnames(metadata) == "Batch Number")

        for (row in c(1:dim(metadata)[[1]])){
            if(metadata$`Plate Column`[[row]]==1){
                metadata[row,col_ind_start:col_ind_end] <- NA
            }
        }
    }

    ## For Optibuild, unstained columns need to have Parameter, Sample Type, Gating Method and Gating Argument
    ## All other fields for unstained columns starting after the Parameter column are optional
    else if(experiment_type == "OB"){
        col_ind_start <- which(colnames(metadata) == "Gating Control")
        col_ind_end <- which(colnames(metadata) == "Fluorochrome")

        for (row in c(1:dim(metadata)[[1]])){
            if(metadata$`Plate Column`[[row]]==1){
                metadata[row,col_ind_start:col_ind_end] <- NA
            }
        }
    }


    metadata <- metadata %>% mutate_all(na_if, "")
    return (metadata)
}

add_data_to_isotypes <- function(ab_df, iso_df, metadata, type){

    row_num <- switch(type, "test" = 1, "ref_a" = 2, "ref_b" = 3)
    ab_type_from_num <- switch(type, "test" = "Test Iso", "ref_a" = "Ref Iso 1", "ref_b" = "Ref Iso 2")

    iso_row <- 2

    ## Duplicate row of antibody data to then replace certain columns with isotype data
    df_ab_iso <- rbind.data.frame(ab_df[row_num,], ab_df[row_num,], stringsAsFactors = FALSE)

    ## Convert all factors to strings
    i <- sapply(df_ab_iso, is.factor)
    df_ab_iso[i] <- lapply(df_ab_iso[i], as.character)

    df_ab_iso <- as_tibble(df_ab_iso) %>%
        mutate("Gating.Control" = replace(Gating.Control, row_number() == iso_row, "NA")) %>%
        mutate("Concentration..mg.ml." = replace(Concentration..mg.ml., row_number() == iso_row, iso_df$Concentration..mg.ml.[row_num])) %>%
        mutate("Concentration..ug.test." = replace(Concentration..ug.test., row_number() == iso_row, iso_df$Concentration..ug.test.[row_num])) %>%
        mutate("Target.Species" = replace(Target.Species, row_number() == iso_row, ab_df$Host.Species[row_num])) %>%
        mutate("Specificity..CD." = replace(Specificity..CD., row_number() == iso_row, "NA")) %>%
        mutate("Specificity..non.CD." = replace(Specificity..non.CD.,
                                                row_number() == iso_row,
                                                paste(ab_df$Host.Species[row_num],
                                                      ab_df$Isotype..Heavy.Chain[row_num],
                                                      ab_df$Isotype..Light.Chain[row_num]))) %>%
        mutate("Clone" = replace(Clone, row_number() == iso_row, iso_df$Clone[row_num])) %>%
        mutate("Batch.Number" = replace(Batch.Number, row_number() == iso_row, iso_df$Batch.Number[row_num])) %>%
        mutate("Catalog.....Material.." = replace(Catalog.....Material.., row_number() == iso_row, iso_df$Catalog.....Material..[row_num])) %>%
        mutate("spec1_range"  = replace(spec1_range, row_number() == iso_row, "NA")) %>%
        mutate("spec2_range"  = replace(spec2_range, row_number() == iso_row, "NA")) %>%
        mutate("spec3_range"  = replace(spec3_range, row_number() == iso_row, "NA")) %>%
        mutate("spec1..Pos.or.Neg."  = replace(spec1..Pos.or.Neg., row_number() == iso_row, "NA")) %>%
        mutate("spec2..Pos.or.Neg."  = replace(spec2..Pos.or.Neg., row_number() == iso_row, "NA")) %>%
        mutate("spec3..Pos.or.Neg."  = replace(spec3..Pos.or.Neg., row_number() == iso_row, "NA")) %>%
        mutate("Ab.Type" = replace(Ab.Type, row_number() == iso_row, ab_type_from_num))

    return(df_ab_iso)
}

# create_metadata_lego_ui <- function(metadata, plate_id, prep_date, run_date, experiment_type, sample_type, sample_species, sample_strain, cst_lot_number){
create_metadata_lego_ui <- function(metadata, plate_id, prep_date, experiment_type, sample_type, sample_species, sample_strain){


    ## Write UI inputs to metadata
    metadata <- metadata %>%
        add_column("Ab/Iso Type" = rep(NA, 96)) %>%
        relocate(`Ab/Iso Type`, .before=`Gating Control`) %>%
        add_column("Concentration (mg/ml)" = rep(NA, 96)) %>%
        relocate(`Concentration (mg/ml)`, .before=`ug/test`)

    metadata$`Plate ID` <- rep(plate_id, 96)
    metadata$`Prep Date` <- rep(prep_date, 96)
    # metadata$`Stain Date` <- rep(stain_date, 96)
    metadata$`Run Date` <- rep(NA_character_, 96)
    metadata$`Experiment Type` <- rep(experiment_type, 96)
    metadata$`Stability Time point` <- rep(0, 96)
    metadata$`Sample Type` <- rep(sample_type, 96)
    metadata$`Sample Species` <- rep(sample_species, 96)
    metadata$`Sample Strain` <- rep(sample_strain, 96)
    metadata$`CS&T Beads Lot #` <- rep(NA_character_, 96)

    return(metadata)
}

create_metadata_table_lego <- function(ab_df, iso_df, reag_no, metadata,
                                       num_of_titrations, titr_order, starting_titr){

    row_num_reag <- switch(reag_no, "1" = 1:24, "2" = 25:48, "3" = 49:72, "4" = 73:96)


    test_df <- add_data_to_isotypes(ab_df, iso_df, metadata, "test")
    ref_a_df <- add_data_to_isotypes(ab_df, iso_df, metadata, "ref_a")
    ref_b_df <- add_data_to_isotypes(ab_df, iso_df, metadata, "ref_b")


    df_sliced_test_ab <- test_df[1,] %>% slice(rep(1:n(), each=8))
    df_sliced_test_iso <- test_df[2,] %>% slice(rep(1:n(), each=12))

    df_reagent <- rbind(df_sliced_test_ab, ref_a_df, ref_b_df, df_sliced_test_iso)


    ## Add titration/volume concentrations
    titration_list_test_ab <- get_lego_titrations(num_titr=num_of_titrations, order=titr_order, start_titr=starting_titr, "ab_test")
    titration_list_test_iso <- get_lego_titrations(num_titr=num_of_titrations, order=titr_order, start_titr=starting_titr, "iso_test")
    titration_list_all <- c(titration_list_test_ab, ref_a_df$Concentration..ug.test., ref_b_df$Concentration..ug.test., titration_list_test_iso)

    metadata <- metadata %>%
        mutate(`Ab/Iso Type` = replace(`Ab/Iso Type`, row_number() == row_num_reag, df_reagent$Ab.Type)) %>%
        mutate(`Gating Control` = replace(`Gating Control`, row_number() == row_num_reag, df_reagent$Gating.Control)) %>%
        mutate(`Target Species` = replace(`Target Species`, row_number() == row_num_reag, df_reagent$Target.Species)) %>%
        mutate(`Specificity (CD)` = replace(`Specificity (CD)`, row_number() == row_num_reag, df_reagent$Specificity..CD.)) %>%
        mutate(`Specificity (non-CD)` = replace(`Specificity (non-CD)`, row_number() == row_num_reag, df_reagent$Specificity..non.CD.)) %>%
        mutate(`Isotype/Host Species` = replace(`Isotype/Host Species`, row_number() == row_num_reag,
                                                paste(df_reagent$Host.Species,
                                                      df_reagent$Isotype..Heavy.Chain.,
                                                      df_reagent$Isotype..Light.Chain.))) %>%
        mutate(`Host Species` = replace(`Host Species`, row_number() == row_num_reag, df_reagent$Host.Species)) %>%
        mutate(`Isotype (Heavy Chain)` = replace(`Isotype (Heavy Chain)`, row_number() == row_num_reag, df_reagent$Isotype..Heavy.Chain.)) %>%
        mutate(`Isotype (Light Chain)` = replace(`Isotype (Light Chain)`, row_number() == row_num_reag, df_reagent$Isotype..Light.Chain.)) %>%
        mutate(`Clone` = replace(`Clone`, row_number() == row_num_reag, df_reagent$Clone)) %>%
        mutate(`Fluorochrome` = replace(`Fluorochrome`, row_number() == row_num_reag, df_reagent$Fluorochrome)) %>%
        mutate(`Batch Number` = replace(`Batch Number`, row_number() == row_num_reag, df_reagent$Batch.Number)) %>%
        mutate(spec1_name = replace(spec1_name, row_number() == row_num_reag, df_reagent$spec1_name)) %>%
        mutate(spec2_name = replace(spec2_name, row_number() == row_num_reag, df_reagent$spec2_name)) %>%
        mutate(spec3_name = replace(spec3_name, row_number() == row_num_reag, df_reagent$spec3_name)) %>%
        mutate(spec1_range = replace(spec1_range, row_number() == row_num_reag, df_reagent$spec1_range)) %>%
        mutate(spec2_range = replace(spec2_range, row_number() == row_num_reag, df_reagent$spec2_range)) %>%
        mutate(spec3_range = replace(spec3_range, row_number() == row_num_reag, df_reagent$spec3_range)) %>%
        mutate(`Positive / Negative_1` = replace(`Positive / Negative_1`, row_number() == row_num_reag, df_reagent$spec1..Pos.or.Neg.)) %>%
        mutate(`Positive / Negative_2` = replace(`Positive / Negative_2`, row_number() == row_num_reag, df_reagent$spec2..Pos.or.Neg.)) %>%
        mutate(`Positive / Negative_3` = replace(`Positive / Negative_3`, row_number() == row_num_reag, df_reagent$spec3..Pos.or.Neg.)) %>%
        mutate(`Optimal with units` = replace(`Optimal with units`, row_number() == row_num_reag,
                                                paste(df_reagent$Optimal,
                                                      df_reagent$Optimal.units))) %>%
        mutate(`gating_method` = replace(`gating_method`, row_number() == row_num_reag, df_reagent$Gating.Method)) %>%
        mutate(`gating_argument` = replace(`gating_argument`, row_number() == row_num_reag, df_reagent$Gating.Argument)) %>%
        mutate(`ug/test` = replace(`ug/test`, row_number() == row_num_reag, titration_list_all)) %>%
        mutate(`Concentration (mg/ml)` = replace(`Concentration (mg/ml)`, row_number() == row_num_reag, df_reagent$Concentration..mg.ml.))

    metadata$`Optimal with units` <- ifelse(sapply(tibble(rep(NA, 96)), function(i) {
        metadata$`Optimal with units` == "NA NA" | is.na(df_reagent$Optimal) | is.null(df_reagent$Optimal) | df_reagent$Optimal == ""
    }),
    "NA",
    paste(df_reagent$Optimal, df_reagent$Optimal.units))


    ## Make units column default to ug/test
    metadata$units <- rep("ug/test", dim(metadata)[1])

    ## Changing column 1 of plate to NA for all fields because unstained and not used
    ## For final metadata creation, all fields NA for any wash columns as well
    # if(experiment_type == "S"){
    #     col_ind_start <- which(colnames(metadata) == "Gating Control")
    #     col_ind_end <- which(colnames(metadata) == "Batch Number")
    #
    #     for (row in c(1:dim(metadata)[[1]])){
    #         if(metadata$`Plate Column`[[row]]==1){
    #             metadata[row,col_ind_start:col_ind_end] <- NA
    #         }
    #     }
    # }
    #
    # ## For Optibuild, unstained columns need to have Parameter, Sample Type, Gating Method and Gating Argument
    # ## All other fields for unstained columns starting after the Parameter column are optional
    # else if(experiment_type == "OB"){
    #     col_ind_start <- which(colnames(metadata) == "Gating Control")
    #     col_ind_end <- which(colnames(metadata) == "Fluorochrome")
    #
    #     for (row in c(1:dim(metadata)[[1]])){
    #         if(metadata$`Plate Column`[[row]]==1){
    #             metadata[row,col_ind_start:col_ind_end] <- NA
    #         }
    #     }
    # }


    metadata <- metadata %>% mutate_all(na_if, "")
    return (metadata)

}

apply_s_prefix_to_specs <- function(metadata){


    ## Configure spec ranges
    metadata$spec1_range <-
        ifelse(sapply(tibble(rep(NA, 96)), function(i) {
            is.na(metadata$spec1_range) |
                is.null(metadata$spec1_range) | metadata$spec1_range == "" | metadata$spec1_range == "NA"
        }),
        "NA",
        paste0('s', metadata$spec1_range))

    metadata$spec2_range <-
        ifelse(sapply(tibble(rep(NA, 96)), function(i) {
            is.na(metadata$spec2_range) |
                is.null(metadata$spec2_range) | metadata$spec2_range == "" | metadata$spec2_range == "NA"
        }),
        "NA",
        paste0('s', metadata$spec2_range))


    metadata$spec3_range <-
        ifelse(sapply(tibble(rep(NA, 96)), function(i) {
            is.na(metadata$spec3_range) |
                is.null(metadata$spec3_range) | metadata$spec3_range == "" | metadata$spec3_range == "NA"
        }),
        "NA",
        paste0('s', metadata$spec3_range))
    return(metadata)
}

get_lego_titrations <- function(num_titr, order, start_titr, type){

    titr_list <- c(start_titr)
    titr <- start_titr

    titration_options <- c(0.03, 0.06, 0.125, 0.250, 0.500, 1, 2)[1:num_titr]

    if(order == "high-to-low"){
        for(i in c(1:(num_titr - 1))){
            titr <- titr / 2
            titr_list <- append(titr_list, titr)
        }
    } else{
        for(i in c(1:(num_titr - 1))){
            titr <- titr * 2

            titr_list <- append(titr_list, titr)
        }
    }

    ## Find corresponding titration value (based on titration_options)
    range <- 0.001
    titr_list <- sort(titr_list)                            # need them sorted
    range <- range*1.000001                                 # avoid rounding issues
    nearest <- findInterval(titr_list, titr_list - range)   # index of nearest
    nearest <- c(-Inf, titr_list)[nearest + 1]              # value of nearest
    diff <- titration_options - nearest                     # compute errors
    snap <- diff <= range                                   # only snap near numbers
    titration_options[snap] <- nearest[snap]                # snap values to nearest

    if(order == "high-to-low"){
        titration_options <- rev(c(titration_options))
    } else{
        titration_options <- c(titration_options)
    }


    titration_list <- append(NA, titration_options)
    if(type == "ab_test"){
        wash_wells <- rep(NA, 8-length(titration_list))
    }
    else if(type == "iso_test"){
        wash_wells <- rep(NA, 12-length(titration_list))
    }
    titration_list <- append(titration_list, wash_wells)

    return(titration_list)

}

################################### MAP FCS FILENAMES TO WELL IDS ####################################################
# getAlpha <- function(filename) {
#   split_string = stringr::str_split(filename, pattern="_")
#   alphaNum = split_string[[1]][3]
#   nbr = as.numeric(substr(alphaNum,2,3))
#   if (nbr < 10) {
#     alphaNum = paste0(substr(alphaNum,1,1), "0", substr(alphaNum,2,3))
#   }
#   print(alphaNum)
#   return(alphaNum)
# }

# zeroPadpaste0 <- function(letter, number) {
#   if (strtoi(number) < 10) {
#     paste0(letter, "0", number)
#   } else {
#     paste0(letter, number)
#   }
# }

# letterMatrix <- sapply(LETTERS[1:8], function(x) rep(x,12), USE.NAMES = F)
# orderedWellID <- mapply(zeroPadpaste0, as.vector(letterMatrix), rep(seq(1,12),8), USE.NAMES = F)

map_fcs_filenames <- function(fcs_files, well_ID_file){
    print(fcs_files)
    ls_df <- tibble('files'=c(fcs_files))

    df <- read.csv(well_ID_file)
    df2 <- tibble('Filename'=NA,'Image Names'=NA,'wellIDs'=df$Well.ID)

    ##### MAP FCS FILENAMES TO DATAFRAME #####
    for(file in ls_df$files){
        for(well in df2$wellIDs){
            if(grepl(paste0(well,'_'),file,fixed=TRUE)){
                df2$Filename[df2$wellIDs == well] <- file
            }

        }
    }
    return(df2)

}

###### USE FLOWCORE LIBRARY TO EXTRACT KEYWORD METADATA####
find_param <- function(fdata) {
  df <- data.frame(flowCore::exprs(fdata)) %>%
      select(!contains(c("SC", "Time")))
  cnames <- colnames(df)
  df <- df %>% summarise(across(cnames, median))
  return(cnames[which(df == max(df) )])
}

##### LOOP THRU 8TH COLUMN TO EXTRACT PARAM #####
grab_meta_data <- function(file_path, check_max_vals_wellid) {

  mdat_ls <- list()

  for (wi in check_max_vals_wellid) {
    w = substr(wi, 14,15)
    fdat <-  flowCore::read.FCS(filename = file.path(file_path, wi), transformation="linearize")
    mdat_ls[[paste0("well_",w)]] <- find_param(fdat)
  }

  meta_data = flowCore::keyword(fdat)

  mdat_ls$'CYT' <- meta_data$`$CYT`
  mdat_ls$'CYTNUM' <- meta_data$CYTNUM
  mdat_ls$'CSTLOT' <- meta_data$`CST BEADS LOT ID`
  mdat_ls$'PLATENAME' <- meta_data$`PLATE NAME`
  mdat_ls$'DATE' <- meta_data$`$DATE`
  mdat_ls$'EXPTNAME' <- meta_data$`EXPERIMENT NAME`
  mdat_ls$'USERNAME' <- meta_data$`EXPORT USER NAME`

  return(mdat_ls)
}

######################### MERGE METADATA FILE OUTLINE WITH POST-CYTOMETER USER INPUTS ################################
# merge_metadata_for_OMIQ <- function(fcs_files, metadata, plate_id=NA, stain_date=NA, donor_id=NA, cytometer=NA, parameter=NA, notes=NA, experiment_type){

# merge_metadata_for_OMIQ <- function(fcs_files, metadata, plate_id=NA, donor_id=NA, cytometer=NA, parameter=NA, notes=NA){
# merge_metadata_for_OMIQ <- function(file_path, fcs_files, metadata, donor_id=NA, notes=NA){
merge_metadata_for_OMIQ <- function(fcs_files,
                                    metadata,
                                    parameter,
                                    cytometer,
                                    serial_num,
                                    run_date,
                                    donor_id=NA,
                                    notes=NA) {

    # check_max_vals_wellid <- fcs_files[which(grepl("[A-H]08.*fcs$",fcs_files))]
    # extracted_mdata <- grab_meta_data(file_path,check_max_vals_wellid)
    # parameter <- lapply(names(extracted_mdata), grep, pattern = "well", value=F)
    # param_index <- names(extracted_mdata)[which(sapply(parameter, FUN=function(X) 1 %in% X))]
    # parameter <- extracted_mdata[param_index]
    parameter <- unlist(parameter, use.names = F)
    parameter <- rep(parameter, each = 12)
    # parameter <- rep(gsub("[\\.]", "-", parameter), each = 12)
    # parameter <- as_tibble(sapply(parameter, function(i){ifelse(i == "NA-A", "NA", paste0(i,"-A"))}))
    print(parameter)

    metadata$`Run Date` <- rep(run_date, nrow(metadata))
    metadata$Filename <- fcs_files
    metadata$`Donor ID` <- rep(donor_id, nrow(metadata))
    # metadata$Cytometer <- rep(paste0(cytometer, ' (', cytometer_list$`Serial Number`[cytometer_list$Nickname == cytometer][[1]], ')'), nrow(metadata))
    # metadata$Cytometer <- rep(paste0(gsub("[\"]", "", extracted_mdata$CYT), ' (', extracted_mdata$CYTNUM, ')'), nrow(metadata))
    metadata$Cytometer <- rep(paste0(gsub("[\"]", "", cytometer), ' (', serial_num, ')'), nrow(metadata))
    metadata$Parameter <- parameter
    metadata$Notes <- ifelse(is.null(notes) | is.na(notes) | notes == "", rep(NA, 96), rep(notes, 96))

    # message(colnames(metadata))
    # if(experiment_type == "S"){
    if(metadata$`Experiment Type`[1] == "S"){
        col_ind_start <- which(colnames(metadata) == "Gating Control")
        col_ind_end <- which(colnames(metadata) == "Costain Parameter")

        for (row in c(1:dim(metadata)[[1]])){
            # if(metadata$`Plate Column`[[row]] > 1 && is.na(metadata$`ug/test`[[row]])){
            if(is.na(metadata$`ug/test`[[row]])){
                metadata[row,col_ind_start:col_ind_end] <- NA
            }
        }
    }
    ## For OptiBuild, unstained column needs Parameter, Sample Type, Gating Method and Argument
    else if(metadata$`Experiment Type`[1] == "OB"){
        col_ind_start <- which(colnames(metadata) == "Gating Control")
        col_ind_end <- which(colnames(metadata) == "Costain Parameter")

        ## for each row on the plate, if ug.test is NA, change to NA
        for (row in c(1:dim(metadata)[[1]])){
            # if(metadata$`Plate Column`[[row]] > 1 && is.na(metadata$`ug/test`[[row]])){
            if(is.na(metadata$`ug/test`[[row]]) & metadata$`Plate Column`[[row]] > 1){
                metadata[row,col_ind_start:col_ind_end] <- NA
            }
        }
    }
    ## LEGO TEMPLATE - will need to adjust
    else{
        col_ind_start <- which(colnames(metadata) == "Gating Control")
        col_ind_end <- which(colnames(metadata) == "Costain Parameter")

        for (row in c(1:dim(metadata)[[1]])){
            # if(metadata$`Plate Column`[[row]] > 1 && is.na(metadata$`ug/test`[[row]])){
            if(is.na(metadata$`ug/test`[[row]])){
                metadata[row,col_ind_start:col_ind_end] <- NA
            }
        }
    }


    metadata <- metadata[!is.na(metadata$Filename), ]
    return(metadata)


}

### AIRFLOW ping
GET_airflow <- function(VM, dag_run_id) {
    link <- paste0("http://", VM, ":8000/api/v1/dags/r_dag/dagRuns/", dag_run_id)
    res <- httr::GET(url = link,
                     config = authenticate("airflow", "airflow"),
                     httr::add_headers(`accept` = 'application/json'),
                     httr::content_type('application/json'))
    res <- content(res, "parsed")
    res
}

DELETE_airflow <- function(VM, dag_run_id) {
    link <- paste0("http://", VM, ":8000/api/v1/dags/r_dag/dagRuns/", dag_run_id)
    res <- httr::DELETE(url = link,
                     config = authenticate("airflow", "airflow"),
                     httr::add_headers(`accept` = 'application/json'),
                     httr::content_type('application/json'))
    res <- content(res, "parsed")
    res
}
