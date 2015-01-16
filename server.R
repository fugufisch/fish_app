
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
source('load_data.R')

shinyServer(function(input, output) {

  data_f <- reactive({
    all_ids <- c()
    for (i in 1:length(groups)) { all_ids <- c( all_ids, input[[ paste0("select_ids_",i) ]])}

    data_f <- data %>%
      filter(ID %in% all_ids) %>%
      #filter( as.Date(datetime) < max(input$limits) & as.Date(datetime) > min(input$limits))
      filter( datetime < as.POSIXct(max(input$limits)) & datetime > as.POSIXct(min(input$limits)) ) %>%
      arrange(ID, datetime, origin)

    if (input$select_data %in% c("original"))
      data_f <- data_f %>% filter( origin=="original")

    if (input$select_data %in% c("interpolated"))
      data_f <- data_f %>% filter( origin=="interpolated")
    return(data_f)
  })

  data_f_ggvis <- reactive({
    data_f_ggvis <- data_f()
    if ( nrow(data_f_ggvis)>1000 )
      data_f_ggvis <- data_f_ggvis %>% sample_n(1000)
    return(data_f_ggvis)
  })

  tooltip <- function(data){
    if (is.null(data)) return(NULL)
    if (is.null(data$ID)) return(NULL)
    paste0("Time: ", data$time)
  }

  data_f_plot <- reactive({
  #data_f %>%
    data_f_ggvis %>%
    ggvis( ~x, ~y, fill= ~ID, key := ~time, shape = ~origin) %>%
    layer_points()   %>%
    add_tooltip(tooltip, "hover")
  })

  data_f_plot %>% bind_shiny("plot_traces_interactive")

  #output$dynamic_ui <- renderUI({
      #sliderInput("time",
      #         "Choose the time:",
      #          min = as.numeric( min(input$limits) ),
      #          max = as.numeric( max(input$limits) ),
      #          value = as.numeric( min(input$limits) ) )
  #})

  output$distPlot <- renderPlot({
    #point_data <- data_f() %>% group_by(ID) %>%
    #  filter( as.numeric(datetime) > input$time) %>%
    #  filter( as.numeric(datetime) < input$time + 10 )
      #filter( (as.numeric(datetime) > input$time)) #%>%
      #filter(datetime <= min(datetime) + 10000)

    if ("Fish traces" %in% input$select_plots) {

      p <- ggplot( data_f(), aes(x,y,colour=ID) )
      p <- p + geom_path( mapping=aes(x,y), data=lake, colour="black" ) +
             theme( axis.title.x = element_blank(),
                    axis.title.y = element_blank() ) +
             ggtitle("Fish traces")
      if(dim(data_f())[1] != 0) {
        #p + geom_point(data=point_data  ) +
        p +  geom_path(alpha=.6, aes(linetype = origin))
      }
      else {
        p
      }
    }
  })

  output$fishDensityPlot <- renderPlot({
    if ("Fish densities" %in% input$select_plots) {
      if(dim(data_f())[1] != 0) {
        p <- ggplot( data_f(), aes(x,y) ) +
              xlim(range(lake$x)) +
              ylim(range(lake$y)) +
              theme(axis.ticks=element_blank(),
              axis.text.y=element_blank(),
              axis.text.x=element_blank() ) +
              coord_fixed()

        p + stat_density2d(geom="tile", aes(fill = ..density..),  contour = FALSE, n = 100) +
            geom_path(mapping=aes(x,y),
              data=lake,
              colour="white" ) +
            ggtitle("Heat-Map of fish-density") +
            facet_grid( . ~ origin)
      }
    }
  })


  output$dataDensityPlot <- renderPlot({

    if ("Data density" %in% input$select_plots) {
      if (dim(data_f())[1] !=0) {
        p <- ggplot( data_f(), aes(datetime,fill=ID))
        p + geom_density( alpha=.3) + aes(y = ..count.., linetype = origin) +
          theme( axis.title.x = element_blank(),
                 axis.title.y = element_blank() ) +
          ggtitle("Measurement density")
      }
    }
  })

})



















#   observe({
#     t_lim = input$limits
#     updateSliderInput(session, "time",
#                       min = min(t_lim),
#                       max = max(t_lim))
#   })
