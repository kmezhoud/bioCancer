#######################################
# Shiny interface for data tabs
#######################################

## show error message from filter dialog
output$ui_filter_error <- renderUI({
  if (is_empty(r_info[["filter_error"]])) return()
  helpText(r_info[["filter_error"]])
})

## data ui and tabs
## state is not available in global environment
## neither are the state_... functions
output$ui_data <- renderUI({
  tagList(
    includeCSS(file.path(getOption("radiant.path.data"),"app/www/style.css")),
    sidebarLayout(
      sidebarPanel(
        wellPanel(
          uiOutput("ui_datasets"),
          conditionalPanel("input.tabs_data != 'Datasets'",
                           checkboxInput("show_filter", "Filter data", value = state_init("show_filter", FALSE)),
                           conditionalPanel("input.show_filter == true",
                                            returnTextAreaInput("data_filter",
                                                                label = "",
                                                                value = state_init("data_filter"),
                                                                placeholder = "Provide a filter (e.g., Genes == 'ATM') and press return"
                                            ),
                                            uiOutput("ui_filter_error")
                           )
          )
        ),
        conditionalPanel("input.tabs_data == 'Datasets'",
                         ## needed to modify radiant helps
                         help_modal("Datasets","manage_help",author = "Karim Mezhoud",
                                    inclMD(file.path(getOption("radiant.path.bioCancer"),"app/tools/help/manage.md"))),

                         uiOutput("ui_Manage")),
        conditionalPanel("input.tabs_data == 'View'",
                         ## needed to modify radiant helps
                         help_modal("View","view_help",author = "Karim Mezhoud",
                                    inclMD(file.path(getOption("radiant.path.bioCancer"),"app/tools/help/view.md"))),

                         uiOutput("ui_View")),
        conditionalPanel("input.tabs_data == 'Plots'",
                         ## needed to modify radiant helps
                         help_modal("Plots","visualize_help",author = "Karim Mezhoud",
                                    inclMD(file.path(getOption("radiant.path.bioCancer"),"app/tools/help/Plots.md"))),

                         uiOutput("ui_Visualize")),
        conditionalPanel("input.tabs_data == 'Regroup'",
                         ## needed to modify radiant helps
        help_modal("Regroup","pivotr_help",author = "Karim Mezhoud",
                   inclMD(file.path(getOption("radiant.path.bioCancer"),"app/tools/help/Regroup.Rmd"))),

                         uiOutput("ui_Pivotr")),
        conditionalPanel("input.tabs_data == 'Statistics'",
                         ## needed to modify radiant helps
                         help_modal("Statistics","explore_help",author = "Karim Mezhoud",
                                    inclMD(file.path(getOption("radiant.path.bioCancer"),"app/tools/help/Statistics.md"))),

                         uiOutput("ui_Explore"))
        #conditionalPanel("input.tabs_data == 'Transform'", uiOutput("ui_Transform")),
        #conditionalPanel("input.tabs_data == 'Combine'", uiOutput("ui_Combine"))
      ),
      mainPanel(
        ## add logo to the page
        #img(src='imgs/icon.png', align = "right"),
        tabsetPanel(id = "tabs_data",
                    tabPanel("Datasets",
                             conditionalPanel("input.dman_preview == 'preview'", h2("Data preview"), htmlOutput("man_example")),
                             conditionalPanel("input.dman_preview == 'str'", h2("Data structure"), verbatimTextOutput("man_str")),
                             conditionalPanel("input.dman_preview == 'summary'", h2("Data summary"), verbatimTextOutput("man_summary")),
                             conditionalPanel(condition = "input.man_show_log == true",
                                              h2("Data load and save commands"),
                                              uiOutput("ui_man_log")
                             ),
                             conditionalPanel("input.man_add_descr == false", uiOutput("man_descr_html")),
                             conditionalPanel("input.man_add_descr == true", uiOutput("man_descr_md"))
                    ),
                    tabPanel("View",
                             download_link("dl_view_tab"),
                             DT::dataTableOutput("dataviewer")
                    ),
                    tabPanel("Plots",
                             download_link("dlp_visualize"),
                             plotOutput("visualize", width = "100%", height = "100%")
                    ),
                    tabPanel("Regroup",
                             conditionalPanel("input.pvt_tab == true",
                                              download_link("dl_pivot_tab"),
                                              DT::dataTableOutput("pivotr")
                             ),
                             conditionalPanel("input.pvt_chi2 == true", htmlOutput("pivotr_chi2")),
                             conditionalPanel("input.pvt_plot == true", br(), br(),
                                              download_link("dlp_pivot"),
                                              plotOutput("plot_pivot", width = "100%", height = "100%")
                             )
                    ),
                    tabPanel("Statistics",
                             download_link("dl_explore_tab"),
                             DT::dataTableOutput("explore")
                    )
                    # tabPanel("Transform",
                    #          htmlOutput("transform_data"),
                    #          verbatimTextOutput("transform_summary"),
                    #          uiOutput("ui_tr_log")
                    # ),
                    # tabPanel("Combine",
                    #          htmlOutput("cmb_data1"),
                    #          htmlOutput("cmb_data2"),
                    #          htmlOutput("cmb_possible"),
                    #          htmlOutput("cmb_data")
                    # )
        )
      )
    )
)
})
