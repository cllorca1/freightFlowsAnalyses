pacman::p_load(dplyr, ggplot2, readr, tidyr, reshape, shiny, shinydashboard, plotly, processx, leaflet, stread, tmap, rgdal)

this_folder = "C:/code/freightFlowsR/simulation_results_analysis/dashboard/"

source(paste(this_folder, "read_data_fun.R", sep =""))

upper_folder = "c:/models/freightFlows/"

scenario_folders = c("muc_scenario_zero_c",
            "muc_scenario_3km",
            "muc_scenario_1km",
            "muc_scenario_paketbox")

scenarios = c("Base (urban)", "a (urban)", "b (urban)","c (urban)" )

colors_three = c("#407dd8","#36a332")




ui = dashboardPage(
  dashboardHeader(title = "RadLast"),
  dashboardSidebar(
    checkboxGroupInput(inputId = "selected_scenarios" , 
                       label = "Scenarios",
                       choices = scenarios, selected = scenarios[1], width = 325)
  ),
  dashboardBody(
    fluidRow(
      tabBox(
        tabPanel(
          title = "Tours",
          plotlyOutput("tours", width = "100%")
        ),
        tabPanel(
          title = "Weight",
          plotlyOutput("weight", width = "100%")
        ),
        tabPanel(
          title = "Distance/Weight",
          plotlyOutput("distance_per_kg", width = "100%")
        ),
        tabPanel(
          title = "Operating time",
          plotlyOutput("operating_time", width = "100%")
        ),
        tabPanel(
          title = "Map",
          leafletOutput("map", width = "100%", height = 800)
        )
      )
    )
  )
)


server = function(input, output){

  dataInput = reactive({
    summary = read_model_results(upper_folder, scenarios, scenario_folders, input$selected_scenarios)
    summary
  })
  
  output$tours = renderPlotly({
    summary = dataInput()
    
    p = ggplot(summary, aes(y=n, x=scenario, fill = vehicle)) +
      scale_fill_manual(values = colors_three) + 
      geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) +
      ylab("Number of tours") +
      xlab("Scenario (area)") +
      theme(text=element_text(size=14)) 
    ggplotly(p, height = 800)
    
  })
  
  output$weight = renderPlotly({
    summary = dataInput()
    
    p = ggplot(summary, aes(y=weight_tn, x=scenario, fill = vehicle)) +
      scale_fill_manual(values = colors_three) + 
      geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) +
      ylab("Sum of parcel weight (tn)")  + 
      xlab("Scenario (area)") +
      ylim(0,80) + 
      theme(text=element_text(size=14))
    ggplotly(p, height = 800)
    
  })
  
  
  output$distance_per_kg = renderPlotly({
    summary = dataInput()
    
    p = ggplot(summary, aes(y=distance/weight_tn/1e3, x=scenario, fill = vehicle)) +
      scale_fill_manual(values = colors_three) + 
      geom_bar(stat = "identity", position =  "stack") +
      ylab("Distance to deliver 1kg (m/kg)") + 
      xlab("Scenario (area)") +
      theme(text=element_text(size=14))
    ggplotly(p, height = 800)
    
  })
  
  output$operating_time = renderPlotly({
    summary = dataInput()
    
    p = ggplot(summary, aes(y=operatingTime/3600, x=scenario, fill = vehicle)) +
      scale_fill_manual(values = colors_three) + 
      geom_bar(stat = "identity", position = "stack") +
      ylab("Sum of operating time (h)") + 
      xlab("Scenario (area)")+
      theme(text=element_text(size=14))
    ggplotly(p, height = 800)
    
  })
  
  output$map  = renderLeaflet({
    shp_muc = st_read(paste(upper_folder, "input/shp/zones_31468_jobs.shp", sep = ""))
    p =  tm_basemap(leaflet::providers$CartoDB) +
      tm_shape(shp_muc, name = "zones_munich") + tm_borders()
    tmap_leaflet(p)

    
  })
  
  
  
  
  
  
  
}



shinyApp(ui, server)