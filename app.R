library(magrittr)
library(data.table)
library(sf)
library(shiny)
library(tmap)

main_path <- "2022_moth"

# species list
species_list <- 
  readRDS("data/species_list.rds")

# species info
species_info <- 
  readRDS("data/species_list_20230525.rds")

family_list <- 
  species_info$Family_ch %>% 
  unique

# Define UI ----
ui <- fluidPage(
  tags$style("background-color: whitesmoke;"),
  # set the page title
  titlePanel(h1("蛾類物種分布地圖",
                style = "grid-area: header;
                         padding: 15px 0px;
                         text-align: center;
                         font-size: 36px;
                         font-family: sans-serif;
                         color: whitesmoke;
                         background-color: rgb(49, 49, 49);
                         border-radius: 15px;
                         margin: 5px 0px 20px 0px;")),
  
  # sidebar select species
  fluidRow(
    column(4,
           h2("物種分布地圖"),
           br(),
           selectInput('family_selected',
                       '選擇科別',
                       family_list),
           # dynamic select of alphabet of scientific name based on family selected
           uiOutput("dynamicAlphabet"),
           # dynamic select of species based on family selected
           uiOutput("dynamicSpecies"),
           HTML("此圖為選擇的物種以 Maxent 預設的閥值產生的 11 種分布範圍。<br/>可透過滑鼠右鍵選擇新視窗開啟圖片來觀看原始大小的圖檔。<br/>"),
           uiOutput('image')
    ),
    column(8,
           # interactive map
           h2("互動式地圖"),
           p("透過互動式地圖區塊觀看物種分布範圍。點選圖層選單按鈕以選擇想要關閉顯示的圖層"),
           tmapOutput(outputId = "tmapMap", height = 600)
           
    )
  )
  
)

# Define server logic ----
server <- function(input, output, session) {
  
  # create alphabet list based on family select
  output$dynamicAlphabet <- renderUI({
    if (is.null(input$family_selected))
      return()
    
    family_choose <- input$family_selected
    
    # Define the choices for the dynamic selectInput based on the selected fruit
    alphabet_of_family <- 
      species_info[Family_ch == family_choose, scientific_name] %>% 
      substr(1, 1) %>% 
      sort() %>% 
      unique
    
    # Update the dynamic selectInput based on the choices
    # updateSelectInput(session, "dynamic_species", label = "選擇物種", choices = species_of_family)
    selectInput("dynamic_alphabet", label = "選擇學名開頭字母", choices = alphabet_of_family)
  })
  
  # create species list based on family select
  output$dynamicSpecies <- renderUI({
    if (is.null(input$dynamic_alphabet))
      return()
    
    family_choose <- input$family_selected
    alphabet_choose <- input$dynamic_alphabet
    
    # Define the choices for the dynamic selectInput based on the selected fruit
    species_of_family <- species_info[Family_ch == family_choose & scientific_name %like% alphabet_choose, 
                                      scientific_name] %>% sort()
    
    # Update the dynamic selectInput based on the choices
    # updateSelectInput(session, "dynamic_species", label = "選擇物種", choices = species_of_family)
    selectInput("dynamic_species", label = "選擇物種", choices = species_of_family)
  })
    
  
  # import species data
  sp_data <- reactive({
    req(input$dynamic_species)
    fread(sprintf("data/binary_table_minmax/%s.csv",
                  input$dynamic_species))
  })
  
  occurrence <- reactive({
    req(input$dynamic_species)
    fread(sprintf("data/species_data/%s.csv", 
                  input$dynamic_species)) %>% 
      sf::st_as_sf(coords = c("X", "Y"), crs = 3826)
  })
  
  # set url of image
  sp_url <- reactive({
    sprintf("%s.png", input$dynamic_species)
  })
  
  output$image <- renderUI({
    tags$img(src = sp_url(), width = "100%")
  })
  
  min_range <- reactive({
    sp_data() %>%
      dplyr::filter(variable == "max") %>%
      dplyr::select(x, y, value) %>%
      raster::rasterFromXYZ(crs = sp::CRS('+init=EPSG:3826'))
  })  
  
  max_range <- reactive({
    sp_data() %>%
      dplyr::filter(variable == "min") %>%
      dplyr::select(x, y, value) %>%
      raster::rasterFromXYZ(crs = sp::CRS('+init=EPSG:3826'))
  })
  
  output$tmapMap <- renderTmap({
      tm_shape(max_range(), name = "最大分布範圍") +
        tm_raster(title = "最大分布範圍", col = "value", palette = "red", alpha = 1/4) +
        tm_shape(min_range(), name = "最小分布範圍") +
        tm_raster(title = "最小分布範圍", col = "value", palette = "red", alpha = 2/4) +
        tm_shape(occurrence(), name = "物種分布點位") +
        tm_dots(palette = "black") +
        tm_view(set.view = c(121, 23.5, 8))
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)

