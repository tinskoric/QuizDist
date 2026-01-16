# Tin Skoric

# Libraries and Stuff

library(bslib)
library(shiny)
library(shinylive)
library(munsell)
library(showtext)
library(hpfilter)
library(rlang)
library(dplyr)
library(EnvStats)
library(htmltools)
library(WriteXLS)
library(readxl)
library(leaflet)
library(sf)
library(janitor)
library(tidyverse)
library(tidygeocoder)
library(tidyr)
library(tigris)
library(ggtext)
library(usmap)
library(ggthemes)
library(ggrepel)
library(usmapdata)
library(kableExtra)
library(ggiraph)
library(stringi)
library(gganimate)
library(psych)
library(lubridate)
library(RColorBrewer)
library(roll)
library(scales)
library(summarytools)
library(tseries)
library(TTR)
library(zoo)
library(grid)
library(gridExtra)
library(patchwork)
library(ggpubr)
library(socviz)
library(gapminder)

font_add_google("Space Grotesk", family = "Space Grotesk")
showtext_auto()

################################################################################

# Color Palette

canned_colors_list <- function(){
  return(
    list(
      purples = list(
        softpurple = "#4c48a3", # ntc: black
        darkpurple = "#3a355f",
        oceanpurple = "#1a124e"
      ),
      compliments = list(
        danger = "#a62452",
        warn = "#a67752",
        info = "#2624d1",
        success = "#266452"
      )
    )
  )
}

canned_colors_get <- function(group, color){
  gc <- c()
  if(missing(group) == TRUE){
    group <- names(canned_colors_list())
  }
  g <- as.vector(group)
  if(missing(color) == TRUE){
    for (i in 1:length(g)){
      for (j in 1:length(unlist(canned_colors_list()[g[[i]]]))){
        gc <- append(gc, canned_colors_list()[[g[i]]][[j]])
      }
    }
  } else {
    c <- as.vector(color)
    for (i in 1:length(g)){
      for (j in 1:length(c)){
        if(TRUE %in% as.vector(names(canned_colors_list()[[g[i]]]) %in% c[j])){
          gc <- append(gc, canned_colors_list()[[g[i]]][[c[j]]])
        }
      }
    } 
  }
  if(is.null(gc)){
    return(FALSE)
  } else {
    return(gc)
  }
}

################################################################################

# ggplot theme

theme_can <- function(
    base_size = 16, # base_size is rel(1)
    base_family = "Space Grotesk",
    base_line_size = base_size / 24,
    base_rect_size = base_size / 24
) {
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      panel.border = element_rect(color = "#3a355f", fill = NA),
      panel.background = element_rect(color = NA, fill = "transparent"),
      plot.background = element_rect(color = NA, fill = "transparent"),
      panel.grid.major = element_line(color = "#4c48a3", linewidth = 0.25, linetype = "dashed", lineend = "round", linejoin = "mitre"),
      panel.grid.minor = element_blank(),
      legend.background = element_rect(color = NA, fill = "transparent"),
      legend.box.background = element_rect(color = "#3a355f", fill = "transparent"),
      legend.key = element_rect(color = NA, fill = "transparent"),
      axis.ticks = element_blank(),
      axis.line = element_line(color = "#4c48a3", linewidth = 0.75),
      axis.text.x = element_text(family = "Space Grotesk", size = 12, color = "#4c48a3", vjust = -0.25),
      axis.text.y = element_text(family = "Space Grotesk", size = 12, color = "#4c48a3", hjust = -0.25),
      axis.title.x = element_text(family = "Space Grotesk", size = 16, color = "#4c48a3", vjust = 0),
      axis.title.y = element_text(family = "Space Grotesk", size = 16, color = "#4c48a3", angle = 90, vjust = 2.5),
      plot.title = element_textbox_simple(
        family = "Space Grotesk", face = "bold", size = 18, 
        color = "#4c48a3", lineheight = 1,
        padding = margin(2, 2, 2, 2),
        margin = margin(0, 0, 0, 0),
        fill = "gray90"
      ),
      plot.subtitle = element_textbox_simple(
        family = "Space Grotesk", face = "bold", size = 12, 
        color = "#3a355f", lineheight = 1,
        padding = margin(2, 2, 2, 2),
        margin = margin(0, 0, 5, 0),
        fill = "antiquewhite"
      ),
      element_markdown(family = "Space Grotesk", size = 12, color = "#3a355f")
    )
}

################################################################################

quizDropInteractive <- function(n, dropCount, mean, stdev, minimum, maximum, nLoop){
  if(dropCount < n){
    nDrop <- n - dropCount
    trial_i <- c()
    for (i in 1:nLoop) {
      trial_i <- append(trial_i, mean(sort(rnormTrunc(n = n, mean = mean, sd = stdev, min = minimum, max = maximum), decreasing = TRUE)[1:nDrop]))
    }
    return(trial_i)
  } else {
    print("uh oh")
  }
}

ui <- fluidPage(
  shinyjs::useShinyjs(),
  title = NULL,
  theme = bslib::bs_theme(
    version = 5, bootswatch = "flatly",
    primary = canned_colors_get(,"softpurple")
  ),
  fluidRow(
    column(
      width = 2,
      sliderInput(
        "quiz_input", "Number of Total Quizzes",
        min = 1, max = 100, value = 20, step = 1
      ),
      uiOutput("quizDrop_input"),
      sliderInput(
        "mean_input", "Mean",
        min = 1, max = 100, value = 80, step = 1
      ),
      sliderInput(
        "sd_input", "Standard Deviation (using a fraction of the mean)",
        min = 0.05, max = 0.2, value = 0.1, step = 0.05
      ),
      sliderInput(
        "min_input", "Minimum Quiz Result",
        min = 0, max = 99, value = 0, step = 1
      ),
      uiOutput("max_input"),
      sliderInput(
        "loop_input", "Number of Trials",
        min = 30, max = 200, value = 100, step = 1
      )
    ),
    column(
      width = 10,
      plotOutput("quizPlot"),
      plotOutput("quizPlot2")
    )
  )
)

server <- (function(input, output, session) {
  quizCount <- reactive({input$quiz_input})
  quizMean <- reactive({input$mean_input})
  quizSD <- reactive({input$sd_input})
  quizMin <- reactive({input$min_input})
  output$quizDrop_input <- renderUI ({
    dropped_quizzes <- quizCount() - 1
    shiny::req(dropped_quizzes)
    sliderInput(
      inputId = "quizDrop_selection",
      label = "Number to Drop",
      min = 0, max = dropped_quizzes,
      value = dropped_quizzes%/%2, step = 1
    )
  })
  output$max_input <- renderUI ({
    min_quiz <- quizMin()
    shiny::req(min_quiz)
    sliderInput(
      inputId = "max_selection",
      label = "Maximum Quiz Result",
      min = min_quiz, max = 100,
      value = 100, step = 1
    )
  })
  quizDropSelect <- reactive({input$quizDrop_selection})
  quizMax <- reactive({input$max_selection})
  quizLoop <- reactive({input$loop_input})
  
  quizData <- reactive({
    trial_drop_i <- c()
    trial_no_drop_i <- c()
    n <- quizCount()
    mean <- quizMean()
    sd <- quizSD()
    min <- quizMin()
    max <- quizMax()
    loop <- quizLoop()
    for (i in 0:(n-1)) {
      trial_drop_i <- append(trial_drop_i, round(mean(quizDropInteractive(n, i, mean, mean*sd, min, max, loop)), 2))
      trial_no_drop_i <- append(trial_no_drop_i, round(mean(quizDropInteractive(n, 0, mean, mean*sd, min, max, loop)), 2))
    }
    drop_test <- tibble(
      "Number Dropped" = 0:(n-1), "Dropped (Result)" = trial_drop_i, "None Dropped (Result)" = trial_no_drop_i, Difference = trial_drop_i - trial_no_drop_i,
      "Incremental Difference" = trial_drop_i - lag(trial_drop_i, 1)
    )
    drop_test$`Drop Margin` <- (drop_test$Difference)/(drop_test$`Number Dropped`)
    drop_test$`Incremental Margin` <- (drop_test$`Incremental Difference`)/(drop_test$`Number Dropped`)
    drop_test <- drop_test %>% filter(`Number Dropped` > 0)
  })
  
  output$quizPlot <- renderPlot({
    ggplot() +
      geom_line(
        data = quizData(),
        mapping = aes(x = `Number Dropped`, y = `Incremental Margin`), color = canned_colors_get(,"softpurple"),
        linewidth = 2, lineend = "round"
      ) +
      geom_point(
        data = quizData(),
        mapping = aes(x = `Number Dropped`, y = `Incremental Margin`), shape = 21, size = 2, color = canned_colors_get(,"softpurple"), fill = canned_colors_get(,"oceanpurple")
      ) +
      geom_point(
        data = quizData() %>% filter(`Number Dropped` == quizDropSelect()),
        mapping = aes(x = `Number Dropped`, y = `Incremental Margin`), shape = 21, size = 2, color = canned_colors_get(,"danger"), fill = canned_colors_get(,"danger")
      ) +
      theme_can() +
      scale_y_continuous(
        "Incremental Margin",
        expand = c(0, 0)
      )
  })
  
  output$quizPlot2 <- renderPlot({
    ggplot() +
      geom_line(
        data = quizData(),
        mapping = aes(x = `Number Dropped`, y = `Drop Margin`), color = canned_colors_get(,"softpurple"),
        linewidth = 2, lineend = "round"
      ) +
      geom_point(
        data = quizData(),
        mapping = aes(x = `Number Dropped`, y = `Drop Margin`), shape = 21, size = 2, color = canned_colors_get(,"softpurple"), fill = canned_colors_get(,"oceanpurple")
      ) +
      geom_point(
        data = quizData() %>% filter(`Number Dropped` == quizDropSelect()),
        mapping = aes(x = `Number Dropped`, y = `Drop Margin`), shape = 21, size = 3, color = canned_colors_get(,"danger"), fill = canned_colors_get(,"danger")
      ) +
      theme_can() +
      scale_y_continuous(
        "Drop Margin",
        expand = c(0, 0)
      )
  })
})

# Create Shiny app ----
shinyApp(ui = ui, server = server)