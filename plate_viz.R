# # library(platetools)
# library(ggplot2)
# library(plotly)
# library(highcharter)
# # library(viridis)
# # example dataframe
# df <- data.frame(vals = rnorm(96),
#                  well = num_to_well(1:96, plate = 96))
# 
# p <- raw_map(data = df$vals,
#         well = df$well,
#         plate = 96) +
#     ggtitle("Example 384-well plate") +
#     theme_dark() +
#     scale_fill_viridis()
# ggplotly(p)
# ggplotly(p) %>% scale_x_discrete(position = "top")
# 
# 
# Well_Name = c("A1","A2","A3","A4","A5","A6","A7","A8","A9","A10","A11","A12","B1","B2","B3","B4","B5","B6","B7","B8","B9","B10","B11","B12","C1","C2","C3","C4","C5","C6","C7","C8","C9","C10","C11","C12","D1","D2","D3","D4","D5","D6","D7","D8","D9","D10","D11","D12","E1","E2","E3","E4","E5","E6","E7","E8","E9","E10","E11","E12","F1","F2","F3","F4","F5","F6","F7","F8","F9","F10","F11","F12","G1","G2","G3","G4","G5","G6","G7","G8","G9","G10","G11","G12","H1","H2","H3","H4","H5","H6","H7","H8","H9","H10","H11","H12")
# 
# Sample_Name = c("Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Standard", "Standard","Standard", "Standard", "Standard", "Standard", "Standard", "Standard", "Standard", "Standard", "Standard", "Standard")
# 
# plateMap = data.frame(Well_Name,Sample_Name)
# plateMap
# plateMap <- mutate(plateMap,
#                    Row=as.numeric(match(toupper(substr(Well_Name, 1, 1)), LETTERS)),
#                    Column=as.numeric(substr(Well_Name, 2, 5)))
# plate= ggplot(data=plateMap, aes(x=Column, y=Row))
# plate
# ############
# plate= ggplot(data=plateMap, aes(x=Column, y=Row)) +
#     geom_point(data=expand.grid(seq(1, 12), seq(1, 8)), aes(x=Var1, y=Var2),
#                color="grey90", shape=21)
# plate
# ############
# 
# plate = ggplot(data=plateMap, aes(x=Column, y=Row)) +
#     geom_point(data=expand.grid(seq(1, 12), seq(1, 8)), aes(x=Var1, y=Var2),
#                color="grey90", shape=21) +
#     coord_fixed(ratio=(13/12)/(9/8), xlim=c(0.5, 12.5), ylim=c(0.5, 8.5)) +
#     scale_y_reverse(breaks=seq(1, 8), labels=LETTERS[1:8]) +
#     scale_x_continuous(breaks=seq(1, 12), position = "bottom")
# plate
# plate + geom_point(aes(colour = factor(Sample_Name)), size = 8)
# #     scale_x_discrete(position="top")
# 
# ############################
# Well_Name = c("A1","A2","A3","A4","A5","A6","A7","A8","A9","A10","A11","A12","B1","B2","B3","B4","B5","B6","B7","B8","B9","B10","B11","B12","C1","C2","C3","C4","C5","C6","C7","C8","C9","C10","C11","C12","D1","D2","D3","D4","D5","D6","D7","D8","D9","D10","D11","D12","E1","E2","E3","E4","E5","E6","E7","E8","E9","E10","E11","E12","F1","F2","F3","F4","F5","F6","F7","F8","F9","F10","F11","F12","G1","G2","G3","G4","G5","G6","G7","G8","G9","G10","G11","G12","H1","H2","H3","H4","H5","H6","H7","H8","H9","H10","H11","H12")
# 
# Sample_Name = c("Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Standard", "Standard","Standard", "Standard", "Standard", "Standard", "Standard", "Standard", "Standard", "Standard", "Standard", "Standard")
# 
# tib <- tibble('Row Letter'=rev(LETTERS[1:8]), 'Row Number'=as.numeric(c(1:8)))
# tib <- tib %>% slice(rep(1:n(), each=12))
# plateMap <- tibble(Well_Name, Sample_Name)
# plateMap <- mutate(plateMap,
#                    "Row Letter"=tib$`Row Letter`,
#                    "Row Number"=tib$`Row Number`,
#                    Column=as.numeric(substr(Well_Name, 2, 5)))
# plateMap
# plate = ggplot(data=plateMap, aes(x=Column, y=`Row Number`)) +
#     geom_point(data=expand.grid(seq(1, 12), seq(1, 8)), aes(x=Var1, y=Var2),
#                color="grey90", shape=21) +
#     coord_fixed(ratio=(13/12)/(9/8), xlim=c(0.5, 12.5), ylim=c(0.5, 8.5)) +
#     scale_y_reverse(breaks=seq(1, 8), labels=LETTERS[1:8]) +
#     scale_x_continuous(breaks=seq(1, 12))
# plate
# plate + geom_point(aes(colour = factor(Sample_Name)), size = 8)
# 
# plate

################################################################################################
plate_vis <- function(){
    Well_Name = c("A1","A2","A3","A4","A5","A6","A7","A8","A9","A10","A11","A12","B1","B2","B3","B4","B5","B6","B7","B8","B9","B10","B11","B12","C1","C2","C3","C4","C5","C6","C7","C8","C9","C10","C11","C12","D1","D2","D3","D4","D5","D6","D7","D8","D9","D10","D11","D12","E1","E2","E3","E4","E5","E6","E7","E8","E9","E10","E11","E12","F1","F2","F3","F4","F5","F6","F7","F8","F9","F10","F11","F12","G1","G2","G3","G4","G5","G6","G7","G8","G9","G10","G11","G12","H1","H2","H3","H4","H5","H6","H7","H8","H9","H10","H11","H12")
    
    Sample_Name = c("Unstained","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Wash","Wash","Wash",
                    "Unstained","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Wash","Wash","Wash",
                    "Unstained","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Wash","Wash","Wash",
                    "Unstained","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Wash","Wash","Wash",
                    "Unstained","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Wash","Wash","Wash",
                    "Unstained","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Wash","Wash","Wash",
                    "Unstained","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Wash","Wash","Wash",
                    "Unstained","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Wash","Wash","Wash")
    
    
    Map2 = data.frame(Well_Name,Sample_Name)
    
    plateMap2 <-mutate(Map2,
                       Row=as.numeric(match(toupper(substr(Well_Name, 1, 1)), LETTERS)),
                       Column=as.numeric(substr(Well_Name, 2, 5)))
    
    
    hchart(plateMap2,"bubble", hcaes(x=Column, y=rev(Row), group = Sample_Name), maxSize = "10%") %>%
        hc_yAxis(min=1, max=8) %>%
        ## There needs to be two A's here because the first "A". I could not figure out how to expand the grid as in ggplot2 with getting a error.
        hc_yAxis(categories = rev(c("A", "B", "C", "D", "E", "F", "G", "H", "H"))) %>%
        hc_xAxis(opposite=TRUE) %>%
        hc_title(text="96 - well Plate Template")
    
    
}
Well_Name = c("A1","A2","A3","A4","A5","A6","A7","A8","A9","A10","A11","A12","B1","B2","B3","B4","B5","B6","B7","B8","B9","B10","B11","B12","C1","C2","C3","C4","C5","C6","C7","C8","C9","C10","C11","C12","D1","D2","D3","D4","D5","D6","D7","D8","D9","D10","D11","D12","E1","E2","E3","E4","E5","E6","E7","E8","E9","E10","E11","E12","F1","F2","F3","F4","F5","F6","F7","F8","F9","F10","F11","F12","G1","G2","G3","G4","G5","G6","G7","G8","G9","G10","G11","G12","H1","H2","H3","H4","H5","H6","H7","H8","H9","H10","H11","H12")

Sample_Name = c("Unstained","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Wash","Wash","Wash",
                "Unstained","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Wash","Wash","Wash",
                "Unstained","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Wash","Wash","Wash",
                "Unstained","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Wash","Wash","Wash",
                "Unstained","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Wash","Wash","Wash",
                "Unstained","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Wash","Wash","Wash",
                "Unstained","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Wash","Wash","Wash",
                "Unstained","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Sample","Wash","Wash","Wash")


Map2 = data.frame(Well_Name,Sample_Name)

plateMap2 <-mutate(Map2,
                   Row=as.numeric(match(toupper(substr(Well_Name, 1, 1)), LETTERS)),
                   Column=as.numeric(substr(Well_Name, 2, 5)))


p <- hchart(plateMap2,"bubble", hcaes(x=Column, y=rev(Row), group = Sample_Name), maxSize = "10%") %>%
    hc_yAxis(min=1, max=8) %>%
    ## There needs to be two A's here because the first "A". I could not figure out how to expand the grid as in ggplot2 with getting a error.
    hc_yAxis(categories = rev(c("A", "B", "C", "D", "E", "F", "G", "H", "H"))) %>%
    hc_xAxis(opposite=TRUE) %>%
    hc_title(text="96 - well Plate Template")

