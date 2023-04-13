library(shiny)
source("global.R")


# Define UI ------

shinyUI(fluidPage(
  title = 'TITLE',
  useShinyjs(),
  
  tags$head(
    title = "Flipside Data Science",
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css?family=Questrial"),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css?family=Inter")
  ),
  withTags({
    header(class="top-banner",
           section(
             a(class="fs-logo", href="https://www.flipsidecrypto.com", img(src = "FLIPSIDE-BRAND-1-WHITE.png", width = "75%")),
             section(class="socials",
                     a(class="twitter", target = "_blank", href="https://twitter.com/flipsidecrypto", "Twitter"),
                     a(class="linkedin", target = "_blank", href="https://www.linkedin.com/company/flipside-crypto", "LinkedIn"),
                     a(class="discord", target = "_blank", href="https://flipsidecrypto.com/discord", "Discord"),
                     a(href="https://next.flipsidecrypto.xyz/", target = "_blank", "Explore our data!")
             )
           )
    )
  }),
  hr(class = "break-line"),
  
  # APP LABEL HERE -----------------------------------  
  
  withTags({
    section(class='hero',
            p("HERO IMAGE HERE"),
            h1(
              class='header', 
              'TAGLINE TITLE', 
            ),
            p('TAGLINE'),
    )
  }),
  
  # APP START HERE -----------------------------------  
  
  ## EXAMPLE INPUTS DIV ----
  div( # re-using chart classes to make smoother outlining
    class = 'chart-container',
    div(
      class = 'chart-block',
      fluidRow(
        column(3, 
               selectInput(inputId = 'selectinput',
                           label = "Select Input",
                           choices = c("a","b","c"),
                           selected = NULL, multiple = TRUE)
               ),
        column(3, 
               radioButtons(inputId = 'selectinput',
                            label = "Select Input",
                            choices = c("a","b","c"),
                            selected = NULL)
               ),
        column(3, 
               textAreaInput(inputId = "textarea",label = "Text Area", value = "area input")
               ),
        column(3, 
               textInput(inputId = "text",label = "Text", value = "text input")
               )
      ),
      fluidRow(
        column(3, 
               dateInput(inputId = 'selectdate',
                         label = "Select Date",
                         value = Sys.Date(),
                         min = Sys.Date()- 100,
                         max = Sys.Date() + 100)
        ),
        column(3, 
               dateRangeInput(inputId = "daterange",
                              label = "Date Range",
                              start = Sys.Date()-1,
                              end = Sys.Date()+1,
                              min = Sys.Date()- 100,
                              max = Sys.Date() + 100)
        ),
        column(3, 
               numericInput(inputId = "numericinput",
                            label = "Number",
                            value = 1,
                            min = -10,
                            max = 10,
                            step = 1)
        ),
        column(3, 
               checkboxGroupInput(inputId = 'checkboxgroup',
                                  label = "Checkbox Group",
                                  choices = c(1,2,3),
                                  selected = 1,
                                  choiceNames = c("One","Two","Three"))
        )
      )
      
    )),
  
  
  ## EXAMPLE PLOTLY Scatterplot CHART ---- 
  
  div(
    class = 'chart-container',
    div(
      class = 'chart-block',
      div(class = 'chart-title', span('SCATTERPLOT EXAMPLE')),
      div(
        class = 'chart',
        plotlyOutput('exscatter')
      )
    )
  ),
  
  # EXAMPLE PLOTLY Barplot CHART ----
  
  div(
    class = 'chart-container',
    div(
      class = 'chart-block',
      div(class = 'chart-title', span('BARPLOT EXAMPLE')),
      div(
        class = 'chart',
        plotlyOutput('exbar')
      )
    )
  ),
  
  # EXAMPLE FORM DIV

  
  # EXAMPLE REACTABLE TABLE DIV ----
   # re-using chart classes to make smoother 
  div(
    class = 'chart-container',
    div(
      class = 'chart-block',
      div(class = 'chart-title', span('Reactable Table EXAMPLE')),
              reactableOutput("myreactable")
      )
    )
  
  # EXAMPLE WALLET CONNECT DIV ----
 
  
  
) # end FluidPage
) # end shinyUI