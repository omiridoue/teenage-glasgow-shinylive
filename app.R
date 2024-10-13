# We need the following libraries installed and loaded
pacman::p_load(
  shiny,
  tidyverse,
  igraph,
  crosstalk,
  glue,
  plotly,
  sigmajs, 
  reshape, 
  Matrix,
  colourvalues,
  htmltools,
  DT, 
  stringr,
  htmlTable,
  shinythemes,
  shinyWidgets,
  shinyhelper,
  magrittr
)

require(shinylive)
#This file hides away the data loading, cleaning and processing through igraph
source("sigma_functions.R")

#Shiny apps are broken in two components one for user interface, ui, and one for the server.
#A few comments on the user interface, there are several structures you can go with, 
#this one has a sidepanel and mainpage. You would nest these components in the 'fluidPage'
#Shiny can take code that would normally pair well with HTML pages called css, the purpose
#for this is to change font, background color etc. and is customisable to your preference.
#the style option can on its own specify the height for ex of the section you're working on
#110vh would stand for vertical height, with 100 referring to the height of a page (anything
#past 100 'overflows' and can be viewed by scrolling. Within the fluidpage you can specify
#what are called fluidRows these store other aspects of the app along horizontal strips, here
#we use radioButtons, pickerInput and sliderInput. The first argument in any of these refers to the name you use to refer to the filter, the second argument is what is actually displayed as a name and the remaining arguments normally refer to the choices if discrete, the value which is a default 'pre-populated' selection, or the max, min for continuous filters. The following code hardcodes these but it's best to have these take after actual variable values. Another feature is the helper functions these offer popup messages with any message you would like to offer to the user, they can be customised in many ways - even embed a full markdown file with graph/images, text or display interactively information about variables observed.
ui <- fluidPage(
  tags$head(tags$style(
    HTML('
                   #sidebar {
                      background-color: #dec4de;
                  }
          
                  body, label, input, button, select { 
                    font-family: "Arial";
                  }')
  )),
  sidebarPanel(style = "max-height: 110vh",
    h4("Filters"),
    tags$style(HTML("hr {border-top: 1px solid #000000;}
                .well {height: 800px;}",
                ".well {background-color:#e5eaf5;}")),
    fluidRow(
      radioButtons("waves", "School Survey Wave", choices = list(1,2,3))%>% 
        helper(type = "inline",
               title = "How do Friendships Change with Time?",
               content = c("Try refreshing the page before switching waves, sometimes the filters might get stuck! Thanks for being patient :)"),
               buttonLabel = "Got it!",
               easyClose = T,
               fade = TRUE,
               size = "s")
    ),
    fluidRow(
      pickerInput(
        "sex_variable", 
        "Participant Sex", 
        choices = unique(vertices1$sex_variable),
        options = list(`actions-box` = TRUE), 
        multiple = T
      )
    ),
    fluidRow(
      pickerInput(
        "parent_smoking", 
        "Group Participants by Parents' Smoking Habits", 
        choices = unique(vertices1$parent_smoking),
        options = list(`actions-box` = TRUE), 
        multiple = T
      )
    ),
    fluidRow(
      sliderInput(
        "filter", 
        "Number of Friends (Indegree)", 
        value = 0, 
        min = -1, 
        max = 6,
        step = 1
      )
    )%>% 
      helper(type = "inline",
             title = "Interpreting Popularity",
             content = c("The -1 value is there for lack of better code :(, the filter takes values greater than the ones you select so you might interpret -1 like a setting to view the isolates/missing values of the network!"),
             buttonLabel = "Got it!",
             easyClose = T,
             fade = TRUE,
             size = "s"),
    actionButton("reset", "Clear Selection"),
    hr(),
    h4("Whole Network Descriptors"),
    fluidRow(
      DTOutput('dtNodes')
    )
  ),
  mainPanel(
    h3("Teenage Glasgow Friendship Network"),
    #fluidRow(textOutput("wave_text")),
    #This is probably the most important part it connects to the server side to 
    #display the sigmajs output interactively as you filter through it! The interface
    #is really cool for this reason, it returns a rendered version of the sigmajsOutput
    fluidRow(sigmajsOutput("sg", width = "100%", height = "550px")%>% 
               helper(type = "inline",
                      title = "Network Plot",
                      content = c("Try clicking on the graph background to wake up the plot. Then click on different nodes to highlight their groups of peers!"),
                      buttonLabel = "Got it!",
                      easyClose = TRUE,
                      fade = TRUE,
                      size = "s")),
#    fluidRow(actionButton("download", "Save SVG"), actionButton("png", "Save PNG"))%>% 
#      helper(type = "inline",
#             title = "Save your Graphs!",
#             content = c("You can save your Graph Visual. Bear in mind this works like
#                         taking a screenshot of the plot area so do zoom out and check
#                         all your nodes are in frame!"),
#             buttonLabel = "Got it!",
#             easyClose = T,
#             fade = TRUE,
#             size = "s"),
    hr(),
    h4("References:"),
    h5("West and Sweeting (1995), Michell and Amos (1997), Pearson and Michell (2000),
                         Pearson and West (2003)"),
    h5("The interactive graphs in this document were built using sigma.js for R. The ideas draw on the R Glasgow Users Group from Nov 2023, special thanks to Dylan Lewis and Erik Igelström at the University of Glasgow Social Public Health Unit for showcasing their work and for all the helpful comments and advice from attendees! Most of the code draws on the extensive sigmajs-shiny documentation by John Coene. Data curation in igraph was carried forward from a previous project as part of the NCRM Social Network Analysis course delivered by Dr. Michael Heaney. Generative Artificial Intelligence was used to code aspects of the server side for this shiny document. Thank you to Dr. Emily Long, Dr. Mark McCann and Dr. Srebrenka Letina for their valuable comments and feedback on temporal graph visualisations. Any error, omission or issue is my own, please drop me a line at 2333157O@student.gla.ac.uk would appreciate your feedback!"),
    #htmlOutput("picture"),
    #fluidRow(div(style ="height:100px; padding:0px;"), DTOutput("dtNodes"))
  )
)

server <- function(input, output, session){
  
  observe_helpers(withMathJax = TRUE)
  
  output$picture <-
    renderText({
      c(
        '<img src="',
        "https://drive.google.com/file/d/1Bku_NjECpf6e3dDhrVO49wGWj_B43T4D/view?usp=sharing",
        '">'
      )
    })
  
  output$wave_text <- renderText({ input$waves })
  
  observeEvent(input$reset,{
    updateSliderInput(session, "filter", value = 0)
    updatePickerInput(session, inputId = "parent_smoking", selected = c("Non-Smoking", "Smoking"))
    updatePickerInput(session, inputId = "sex_variable", selected = c("Girl", "Boy"))
    
  })
  
  g <- reactive({
    if (input$waves < 2) {
      g1  # Assuming sd1 is the appropriate data object
    } else if(input$waves == 2) {
      g2
    } else {
      g3  # Assuming sd2 is the appropriate data object
    }
  })
  
  sd <- reactive({
    if (input$waves < 2) {
      sd1  # Assuming sd1 is the appropriate data object
    } else if(input$waves == 2) {
      sd2
    } else {
      sd3  # Assuming sd2 is the appropriate data object
    }
  })
  
  results <- reactive({
    if (input$waves < 2) {
      result1  # Assuming sd1 is the appropriate data object
    } else if(input$waves == 2) {
      result2
    } else {
      result3  # Assuming sd2 is the appropriate data object
    }
  })
  
  output$table <- renderDataTable(results())
  
  layout <- reactive({
    if (input$waves < 2) {
      layout1  # Assuming sd1 is the appropriate data object
    } else if(input$waves == 2) {
      layout2
    } else {
      layout3  # Assuming sd2 is the appropriate data object
    }
  })
  
  output$styledtable <- reactive({
    if (input$waves < 2) {
      styled_table1  # Assuming sd1 is the appropriate data object
    } else if(input$waves == 2) {
      styled_table2
    } else {
      styled_table3  # Assuming sd2 is the appropriate data object
    }
  })
  
  output$sg <- renderSigmajs({
    sigmajs() %>%
      sg_from_igraph(g(), sd = sd(), layout()) %>% 
      sg_neighbours() %>% 
      sg_settings(drawLabels = TRUE, 
                  hoverFontStyle = "bold", 
                  labelColor = "node", 
                  labelSizeRatio = 3,
                  defaultLabelHoverColor = "node",
                  mouseWheelEnabled = TRUE, 
                  drawEdgeLabels = FALSE,
                  labelThreshold= 100) %>% 
      sg_drag_nodes() %>% 
      sg_layout() %>%
      sg_noverlap()
  })
  
  output$dtNodes <- renderDataTable({
    DT::datatable(results(), options = my.options
    )
  }, server=FALSE)
  # Get filtered node IDs
  
  observeEvent(input$waves, {
    sigmajsProxy("sg") %>% 
      sg_clear_p()
  })
  
  observeEvent(input$filter, {
    sigmajsProxy("sg") %>% 
      sg_filter_undo_p("sz") %>% # we undo the filter before applying it
      sg_filter_gt_p(input$filter, "indegree", name = "sz") %>% 
      sg_noverlap_p()
  })
  
  observeEvent(input$sex_variable, {
    sigmajsProxy("sg") %>% 
      sg_filter_undo_p("sex_variable") %>% 
      sg_filter_not_eq_p(input$sex_variable, "sex_variable", name = "sex_variable") %>% 
      sg_noverlap_p()
  })
  
  observeEvent(input$parent_smoking, {
    sigmajsProxy("sg") %>% 
      sg_filter_undo_p("parent_smoking") %>% 
      sg_filter_not_eq_p(input$parent_smoking, "parent_smoking", name = "parent_smoking") %>% 
      sg_noverlap_p()
  })
  
  observeEvent(input$download, {
    sg_export_svg_p(sigmajsProxy("sg"), labels=TRUE)
  })
  observeEvent(input$png, {
    sg_export_img_p(sigmajsProxy("sg"))
  })
  
}

shinyApp(ui, server) # run
