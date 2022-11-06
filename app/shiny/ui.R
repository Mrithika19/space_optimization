
navbarPage(
  title = "Packing Optimizer",
  id = "main",
  selected = "Instances",
  theme = shinytheme("yeti"),
  collapsible = TRUE,
  
  # Include shinyDashboard css
  useShinydashboard(),
  # Include shinyalerts (popups)
  useShinyalert(),
  
  tabPanel(
    title = "Instances",
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(
            width = 12,
            align = "center",
            h4("Choose Problem Instance"),
            hr()
          )
        ),
        pickerInput(
          inputId = "author",
          label = "Search instances by author",
          choices = list(
            Personal = personal
          ),
          options = list(
            size = 7
          )
        ),
        selectInput(
          inputId = "instance",
          label = "Choose problem instance to analyse",
          choices = NULL,
          selectize = FALSE,
          size = 10
        ),
        br(),
        fileInput(
          inputId = "file_input",
          label = "Upload",
          multiple = FALSE, 
          placeholder = "Problem Instance",
          accept = c(".csv")
        ),
        textInput(
          inputId = "save_as",
          label = "Save instance as",
          placeholder = "Name of Instance"
        ),
        fluidRow(
          column(
            width = 12,
            align = "center",
            helpText("Note: File name must end with '.csv'", style = "color: #00a3cb"),
            actionButton(
              inputId = "save",
              label = "Save",
              icon = icon("save")
            ),
            actionButton(
              inputId = "delete",
              label = "Delete",
              icon =icon("trash-alt")
            )
          )
        )
      ),
      mainPanel(
        wellPanel(
          fluidRow(
            column(
              width = 12, 
              align = "center",
              h4("Summary of Problem Instance"),
              hr()
            )
          ),
          br(),
          fluidRow(
            column(
              width = 6,
              DT::dataTableOutput("instance_summary")
            ),
            column(
              width = 6,
              pickerInput(
                inputId = "sort_data_vis",
                label = "Sort instance for visualisation",
                choices = c(
                  "Default Order",
                  "Decreasing Width", 
                  "Decreasing Height", 
                  "Decreasing Area",
                  "Increasing Width",
                  "Increasing Height",
                  "Increasing Area"
                ),
                options = list(
                  size = 7
                )
              )
            )
          )
        ),
        fluidRow(
          shinydashboard::valueBox(
            value = textOutput("strip_width"),
            subtitle = "Strip Width",
            icon = icon("arrows-alt-h")
          ),
          shinydashboard::valueBox(
            value = textOutput("optimal"),
            subtitle = "Optimal Height",
            icon = icon("thumbs-up")
          ),
          shinydashboard::valueBox(
            value = textOutput("n_items"),
            subtitle = "Number of Items",
            icon = shiny::icon("clipboard-list")
          )
        ),
        uiOutput(
          outputId = "display_instance"
        )
      )
    )
  ),
  tabPanel(
    title = "Algorithms",
    sidebarLayout(
      sidebarPanel(
        h4("Packing Algorithms"),
        radioGroupButtons(
          inputId = "heuristic",
          label = "Choose heuristic algorithm",
          choices = c("Constructive", "Improved Best-Fit"),
          direction = "horizontal",
          justified = TRUE
        ),
        pickerInput(
          inputId = "sort_data",
          label = "Sort instance (heuristic only)",
          choices = c(
            "Default Order" = NA,
            "Decreasing Width" = "dec.W", 
            "Decreasing Height" = "dec.H", 
            "Decreasing Area" = "dec.A",
            "Increasing Width" = "inc.W",
            "Increasing Height" = "inc.H",
            "Increasing Area" = "inc.A"
          ),
          options = list(
            size = 7
          )
        ),
        br(),
        p("Metaheuristics"),
        radioGroupButtons(
          inputId = "meta_heuristic",
          choices = c("Genetic Algorithm")
        ),
        br(),
        p("Maximum Algorithm Run Time"),
        fluidRow(
          column(
            width = 3,
            numericInput(
              inputId = "max_time_h", 
              label = "Hours", 
              value = 0, 
              min = 0, 
              step = 1,
              width = "100%"
            )
          ),
          column(
            width = 1,
            p(":")
          ),
          column(
            width = 3,
            numericInput(
              inputId = "max_time_m", 
              label = "Minutes", 
              value = 0, 
              min = 0, 
              max = 59,
              step = 1,
              width = "100%"
            )
          ),
          column(
            width = 1,
            p(":")
          ),
          column(
            width = 3,
            numericInput(
              inputId = "max_time_s", 
              label = "Seconds", 
              value = 0, 
              min = 0, 
              max = 59,
              step = 1,
              width = "100%"
            )
          ),
          fluidRow(
            column(
              width = 12,
              helpText("Warning: Metaheuristics may take a long time to run, we advise setting a time limit.", style = "color: #f14c19")
            )
          )
        )
      ),
      mainPanel(
        wellPanel(
          tabsetPanel(
            id = "algo_params",
            tabPanel(
              title = "Overview",
              wellPanel(
                fluidRow(
                  h4("Input Summary")
                ),
                br(),
                fluidRow(
                  column(
                    width = 6,
                    align = "center",
                    p("Algorithm"),
                    verbatimTextOutput("selected_algorithm")
                  ),
                  column(
                    width = 6,
                    align = "center",
                    p("Instance"),
                    verbatimTextOutput("selected_instance")
                  )
                )
              )
            ),
            tabPanel(
              title = "GA Parameters",
              wellPanel(
                fluidRow(
                  h4("Hybrid Genetic Algorithm Parameters")
                ),
                br(),
                fluidRow(
                  column(
                    width = 3,
                    p("General"),
                    hr(),
                    numericInput(
                      inputId = "ga_pop_size",
                      label = "Population size",
                      value = 30,
                      min = 10,
                      max = 200,
                      step = 10,
                      width = "50%"
                    ),
                    numericInput(
                      inputId = "ga_n_gen",
                      label = "Number of generations",
                      value = 50,
                      min = 10,
                      max = 1000,
                      step = 10,
                      width = "50%"
                    )
                  ),
                  column(
                    width = 3,
                    p("Selection"),
                    hr(),
                    radioButtons(
                      inputId = "ga_selection_type", 
                      label = "Type", 
                      choiceNames = c("Roulette Wheel", "Tournament"),
                      choiceValues = c("roulette", "tournament")
                    ),
                    uiOutput("tournament_size")
                  ),
                  column(
                    width = 3,
                    p("Crossover"),
                    hr(),
                    radioButtons(
                      inputId = "ga_crossover_type",
                      label = "Type",
                      choiceNames = c("Partially Matched", "Order"),
                      choiceValues = c("pmx", "ox")
                    ),
                    sliderInput(
                      inputId = "ga_crossover_prob",
                      label = "Crossover probability",
                      min = 0,
                      max = 1,
                      value = 0.7,
                      step = 0.01,
                      round = FALSE
                    )
                  ),
                  column(
                    width = 3,
                    p("Mutation"),
                    hr(),
                    sliderInput(
                      inputId = "ga_mutation_prob",
                      label = "Mutation probability",
                      min = 0,
                      max = 0.1,
                      value = 0.01,
                      step = 0.001
                    )
                  )
                )
              )
            )
          ),
          hr(),
          fluidRow(
            column(
              width = 12,
              align = "center",
              actionButton(
                inputId = "run_algorithm",
                label = "Run Algorithm",
                style = "color: #fff; background-color: #f14c19"
              )
            )
          )
        ),
        wellPanel(
          tabsetPanel(
            id = "run_panels",
            tabPanel(
              title = "Information",
              wellPanel(
                br(),
                fluidRow(
                  column(
                    width = 4,
                    shinydashboard::valueBox(
                      value = textOutput("run_time"),
                      subtitle = "Run Time",
                      icon = shiny::icon("hourglass-half"),
                      width = NULL
                    )
                  ),
                  column(
                    width = 4,
                    shinydashboard::valueBox(
                      value = textOutput("packing_height"),
                      subtitle = "Packed Height",
                      icon = shiny::icon("arrows-alt-v"),
                      width = NULL
                    )
                  ),
                  column(
                    width = 4,
                    shinydashboard::valueBox(
                      value = textOutput("optimal_height_run"),
                      subtitle = "Optimal Height",
                      icon = shiny::icon("thumbs-up"),
                      width = NULL
                    )
                  )
                ),
                hr(),
                fluidRow(
                  shinycssloaders::withSpinner(
                    plotlyOutput("run_plot"),
                    hide.ui = FALSE
                  )
                )
              )
            ),
            tabPanel(
              title = "Additional",
              wellPanel(
                fluidRow(
                  column(
                    width = 12,
                    align = "center",
                    h5("Layout"),
                    hr(),
                    DT::dataTableOutput(
                      outputId = "layout_info"
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  ),
  tabPanel(
    title = "Compare",
    wellPanel(
      fluidRow(
        column(
          width = 12,
          align = "center", 
            p("Compare Algorithms")
          )
        )
      ),
      uiOutput(
        outputId = "compare_inputs"
      ),
      hr(),
      fluidRow(
        column(
          width = 4,
          p("Maximum Run Time Per Algorithm"),
          fluidRow(
            column(
              width = 3,
              numericInput(
                inputId = "compare_max_time_h", 
                label = "Hours", 
                value = 0, 
                min = 0, 
                step = 1,
                width = "100%"
              )
            ),
            column(
              width = 1,
              p(":")
            ),
            column(
              width = 3,
              numericInput(
                inputId = "compare_max_time_m", 
                label = "Minutes", 
                value = 0, 
                min = 0, 
                max = 59,
                step = 1,
                width = "100%"
              )
            ),
            column(
              width = 1,
              p(":")
            ),
            column(
              width = 3,
              numericInput(
                inputId = "compare_max_time_s", 
                label = "Seconds", 
                value = 0, 
                min = 0, 
                max = 59,
                step = 1,
                width = "100%"
              )
            )
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          align = "center",
          actionButton(
            inputId = "run_compare",
            label = "Compare",
            style = "color: #fff; background-color: #f14c19"
          )
        )
      ),
    wellPanel(
      tabsetPanel(
        id = "compare_output",
        tabPanel(
          title = "Information",
          uiOutput(
            outputId = "compare_outputs"
          )
        ),
        tabPanel(
          title = "Plots",
          uiOutput(
            outputId = "compare_plots"
          )
        )
      )
    )
  )
)