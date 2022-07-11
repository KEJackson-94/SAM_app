################################################################################
# 1.2 Load relevant packages & data frames & set proper working directory
################################################################################

library(tidyr)
library(ggplot2)
library(gridExtra)
library(scales)
library(shiny)
library(plotly)

#setwd("C:\\SAM_app2\\SAM_app2\\") # depending on where you unzip file
#getwd()

# I added path argument for github, might not work when pushed to shiny.io
# On a similar note, I moved csv files to this subdirectory '~/data//' (originally, in setwd())
files <- list.files(path = "~/data",pattern = '\\.csv$', full.names = FALSE) 

all_data <- do.call(rbind, lapply(files, function(x) 
  transform(read.csv(x), Indicator = basename(x))))

all_data$Indicator <-gsub('.{4}$', '', all_data$Indicator) 
all_data$Indicator <- gsub('^.{4}', '', all_data$Indicator)
all_data <- all_data[which(all_data$ISO !="[]"),]

Country_lst <- unique(all_data$Country_Name)
ISO_lst <- unique(all_data$ISO)
SAM_lst <- unique(all_data$Indicator)

#### indicators by dimension for reference

#econ_indctrs <- c("Food_Loss", "Trade_Openness", "Government_Support", "Price_Volatility", "Finance_Access", "Labor_Productivity")
#soc_indctrs <- c("Crop_Diversity", "Food_Affordability", "Under_nourishment", "Rural_Poverty", "Gender_Gap", "Land_Right")
#env_indctrs <- c('Soil_Erosion', 'P_Surplus', 'N_Surplus', 'Land_Use_Change', 'Greenhouse_Gas','Water_Consumption')

#### Here are thresholds for reference

#Water Consumption (SUSI): g-1, r-2
#Nitrogen Surplus (Nsur): g-52, r-69
#Phosphorous Surplus (Psur): g-3.5, r-6.9
#Land Cover Change (LCC): g-0, r-0.0053
#Greenhouse Gas Emissions (GHG): g-0.86, r-1.08
#Soil Erosion (SER): g-1, r-5

#Labor Productivity (AGDP): g-7946, r-460
#Finance Access (A2F): g-100, r-25
#Price Volatility (PVOL): g-0.10, r-0.23
#Government Support (AEXP): g-2405, r-25
#Trade Openness (TROP): g-71, r-17
#Food Loss (FLP): g-2.2, r-6.6

#Crop Diversity (H index): g-48, r-22
#Food Affordability (RSE): g-100, r-30
#Under-nourishment (UDN): g-0, r-7.5
#Gender Gap Score (GGG): g-80, r-70
#Land Rights (LRS): g-2, r-3
#Rural Poverty Ratio (RPV): g-2, r-13

################################################################################

ui <- fluidPage(
  titlePanel("Welcome to SAM, the Sustainable Agriculture Matrix!"),
  # Copy the line below to make a select box 
  selectInput("select", label = h6("Which Country's Nitrogen Input/Output Data Would You Like to View?"), 
              choices = Country_lst, 
              selected = 1),
  selectInput("select2", label = h6("Which SAM Indicator Would You Like to View?"), 
              choices = SAM_lst, 
              selected = 1),
  fluidRow(column(9,plotlyOutput("myPlot")))
)

################################################################################
server <- function(input, output) {
  output$"myPlot" <- 
    renderPlotly({
      Indctr <- input$'select2'
      Cntry <- ISO_lst[which(Country_lst==input$'select')[1]]
      if (input$'select2' == "Food_Loss"){ #### Econ Indicators
        indicator_name <- 'Food Loss (FLP)'
        y_label <- 'FLP (%)'
        green_bndry <- 2.2
        red_bndry <- 6.6
      }
      else if (input$'select2' == "Trade_Openness"){
        indicator_name <- 'Trade Openness (TROP)'
        y_label <- 'TROP (%)'
        green_bndry <- 71
        red_bndry <- 17
      }
      else if (input$'select2' == "Government_Support"){
        indicator_name <- ' Government Support (AEXP)'
        y_label <- 'AEXP (2011 US$ PPP)'
        green_bndry <- 2405
        red_bndry <- 25
      }
      else if (input$'select2' == "Price_Volatility"){
        indicator_name <- 'Price Volatility (PVOL)'
        y_label <- 'PVOL (unitless)'
        green_bndry <- 0.10
        red_bndry <- 0.23
      }
      else if (input$'select2' == "Finance_Access"){
        indicator_name <- 'Finance Access (A2F)'
        y_label <- 'A2F (score)'
        green_bndry <- 100
        red_bndry <- 25
      }
      else if (input$'select2' == "Labor_Productivity"){
        indicator_name <- 'Labor Productivity (AGDP)'
        y_label <- 'AGDP (2011 US$ PPP)'
        green_bndry <- 7946
        red_bndry <- 460
      }
      else if (input$'select2' == 'Soil_Erosion'){ #### Env Indicators
        indicator_name <- 'Soil Erosion (SER)'
        y_label <- 'SER (ton/ha)'
        green_bndry <- 1
        red_bndry <- 5
      }
      else if (input$'select2' == 'Greenhouse_Gas'){
        indicator_name <- 'Greenhouse Gas Emissions (GHG)'
        y_label <- 'GHG (CO2eq/ha)'
        green_bndry <- 0.86
        red_bndry <- 1.08
      }
      else if (input$'select2' == 'Land_Use_Change'){
        indicator_name <- 'Land Cover Change (LCC)'
        y_label <- 'LCC (ha-deforested/ha-Cropland/yr)'
        green_bndry <- 0.00
        red_bndry <- 0.0053
      }
      else if (input$'select2' == 'P_Surplus'){
        indicator_name <- 'Phosphorous Surplus (Psur)'
        y_label <- 'Psur (kg P/ha/yr)'
        green_bndry <- 3.5
        red_bndry <- 6.9
      }
      else if (input$'select2' == "N_Surplus"){
        indicator_name <- 'Nitrogen Surplus (Nsur)'
        y_label <- 'Nsur (kg N/ha/yr)'
        green_bndry <- 52
        red_bndry <- 69
      }
      else if (input$'select2' == 'Water_Consumption'){
        indicator_name <- 'Water Consumption (SUSI)'
        y_label <- 'SUSI (annual km^3 irrigation/ km^3 sustainable water consum.)'
        green_bndry <- 1
        red_bndry <- 2
      }
      else if (input$'select2' == "Crop_Diversity"){ #### Soc. Indicators
        indicator_name <- 'Crop Diversity (Hindex)'
        y_label <- 'Hindex (count)'
        green_bndry <- 48
        red_bndry <- 22
      }
      else if (input$'select2' == "Food_Affordability"){
        indicator_name <- 'Food Affordability (RSE)'
        y_label <- 'RSE (%)'
        green_bndry <- 100
        red_bndry <- 30
      }
      else if (input$'select2' == "Under_nourishment"){
        indicator_name <- 'Under-nourishment (UDN)'
        y_label <- 'UDN (%)'
        green_bndry <- 0
        red_bndry <- 7.5
      }
      else if (input$'select2' == "Rural_Poverty"){
        indicator_name <- 'Rural Poverty (RPV)'
        y_label <- 'RPV (%)'
        green_bndry <- 2
        red_bndry <- 13
      }
      else if (input$'select2' == "Gender_Gap"){
        indicator_name <- 'Gender Gap Score (GGG)'
        y_label <- 'GGG (score)'
        green_bndry <- 80
        red_bndry <- 70
      }
      else {
        indicator_name <- 'Land Rights (LRS)'
        y_label <- 'LRS (score)'
        green_bndry <- 2
        red_bndry <- 3
      }
      
      #### Use this for individual plots
      df <- all_data[which(all_data$ISO == Cntry),]
      df_tidyr <- df %>% gather(key = 'Year', value = 'Raw', X1961:X2016)
      df_tidyr$Year <- as.integer(gsub('^.{1}', '', df_tidyr$Year))
      df_tidyr <- df_tidyr[which(df_tidyr$Indicator==Indctr),]
      country_name <- df_tidyr$Country_Name[1]
      indicator_name <- df_tidyr$Indicator[1]
      
      df_tidyr <- na.omit(df_tidyr)
      
      ggplot(data = df_tidyr, aes(x= df_tidyr$Year, y=df_tidyr$Raw, color=df_tidyr$Indicator)) +
        geom_line(size=2, color = 'darkgrey')+
        geom_point(size=2, color = 'black') +
        xlim(min(df_tidyr$Year), max(df_tidyr$Year)) + 
        xlab('year') +
        ylab(y_label) + ### MAKE SURE THIS IS RIGHT
               ggtitle(paste0(indicator_name,', ',input$'select')) + 
               theme_classic() +
               theme(legend.position="none", 
                     axis.text.x = element_text(size = 9),
                     axis.text.y = element_text(size = 9),
                     axis.title.x = element_text(size = 9),
                     axis.title.y = element_text(size = 9),
                     plot.title=element_text(size=12)) +
               facet_wrap(~Indicator, scales = "free") + 
               geom_hline(yintercept = green_bndry, color="green", linetype="dashed", size= 2) + 
               geom_hline(yintercept = red_bndry, color="red", linetype="dashed", size =2)
    })
}

################################################################################

shinyApp(ui = ui, server = server)