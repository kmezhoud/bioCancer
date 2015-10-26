
help_menu <-
  tagList(
    navbarMenu(title = "", id = "Help", icon = icon("question-circle"),
               tabPanel("Help", uiOutput("help_modeling"), icon = icon("question")),
               #tabPanel("Videos", uiOutput("help_videos"), icon = icon("film")),
               tabPanel("About", uiOutput("help_about"), icon = icon("info")),
               #tabPanel(tags$a("", href = "http://kmezhoud.github.io/CancerPortal/", target = "_blank",
               #                list(icon("globe"), "bioCancer docs"))),
               tabPanel(tags$a("", href = "https://github.com/kmezhoud/CancerPortal/issues", target = "_blank",
                               list(icon("github"), "Report issue")))
    ),
    js_head
  )
## ui for quant class - radiant
source("inst/quant/quant_ui.R")

## ui for marketing
modeling_ui <-
  tagList(
     navbarMenu("Maps",
             tabPanel("(Dis)similarity", uiOutput("mds")),
             tabPanel("Attributes", uiOutput("pmap"))

  ),
  navbarMenu("Factor",
             tabPanel("Pre-factor", uiOutput("pre_factor")),
             tabPanel("Factor", uiOutput("full_factor"))
  ),

  navbarMenu("Cluster",
             tabPanel("Hierarchical", uiOutput("hier_clus")),
             tabPanel("K-means", uiOutput("kmeans_clus"))
  ),

  navbarMenu("Conjoint",
             tabPanel("Create profiles", uiOutput("conjoint_profiles")),
             tabPanel("Conjoint", uiOutput("conjoint"))
  )
)




## ui for base radiant
shinyUI(
  do.call(navbarPage, c("bioCancer", nav_ui,quant_ui,modeling_ui,shared_ui, help_menu))
)

