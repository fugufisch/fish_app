
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
source('load_data.R')

shinyUI(fluidPage(

  # Application titel
  titlePanel("Pike tracked positions"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("limits",
                     label = "Choose timerange:",
                     start = mintime,
                     end   = maxtime,
                     min   = mintime,
                     max  = maxtime),

      radioButtons( "select_data", "Select Data:",
                    c("original", "interpolated", "both")),

      checkboxGroupInput( "select_plots", "Select which plots to compute:",
                          c("Fish traces" = "Fish traces",
                            "Fish traces interactive (experimental)" = "Fish traces interactive",
                            "Fish densities (slow)" = "Fish densities",
                            "Data density" = "Data density" ),
                           selected =  c("Fish traces","Data density") ),

      lapply(1:length(groups), function(i) {
        ids_group = as.character( unique( filter( data, group==groups[i] )$ID ) )
        names(ids_group) <- sapply(ids_group, FUN=substring, first=1, last=3)
        checkboxGroupInput( paste0("select_ids_", i), groups[i], ids_group, inline=TRUE )
      })
      #checkboxGroupInput( 'select_ids', 'Select fish',
      #                  all_ids, inline=TRUE)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      #submitButton("Update Plots", icon("refresh")),
      #plotOutput("distPlot"),
      conditionalPanel(
        "input.select_plots.indexOf('Fish traces') != -1",
        plotOutput("distPlot")),
      conditionalPanel(
        "input.select_plots.indexOf('Fish traces interactive') != -1",
        ggvisOutput("plot_traces_interactive")),
      conditionalPanel(
        "input.select_plots.indexOf('Fish densities') != -1",
        plotOutput("fishDensityPlot")),
      conditionalPanel(
        "input.select_plots.indexOf('Data density') != -1>",
        plotOutput("dataDensityPlot"))
    )
  )
))
