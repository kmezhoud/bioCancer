output$ui_visPhysic <- renderUI({
  h4("Dynamic Layouts:")
  enableHierarchi <- c('TRUE', 'FALSE')
  PhysicLayout <-c('barnesHut', 'repulsion', 'hierarchicalRepulsion', 'forceAtlas2Based')
  MethodHierarchi <- c("hubsize", "directed")
  Hierarchi_Att <- c("LR","RL","UD","DU")

  tagList(
    checkboxInput("enableHierarchiId",label="Hierarchi", value = FALSE),
    #selectizeInput("enableHierarchiId", label = " Enable Hierarchi", choices = enableHierarchi,
     #              selected= 'FALSE', multiple= FALSE),
    selectizeInput("MethodHierarchiId",label="Layout Method", choices = MethodHierarchi,
                   selected= "hubsize", multiple = FALSE),
    selectizeInput("Hierarchi_AttId", label = "Direction", choices = Hierarchi_Att,
                   selected= 'LR', multiple = FALSE),

    selectizeInput("PhysicLayoutId",label="Physics Options", choices = PhysicLayout,
                   selected= 'hierarchicalRepulsion', multiple = FALSE)


)
})



# output$ui_FIsFilterNetwork <- renderUI({
#   FIs_Att <- c("activat","inhibit","predicted","catalyze","reaction","phosphoryl","regulat","express",
#                "complex","regulat","binding","compound","input","dissociation","indirect","ubiquitinated" )
#
#
#   selectizeInput("FIs_AttNetworkId", label = "FIs Attributes:", choices = FIs_Att,
#                  selected = state_multiple("FIs_AttNetworkId",FIs_Att, "activat"), multiple = TRUE)
#
# })
#
# output$ui_UseLinkerNetwork <- renderUI({
#   UseLinkers <- c("FALSE", "TRUE")
#
#
#   selectizeInput("UseLinkerNetId", label = "Use Linkers:", choices = UseLinkers,
#                  selected = "FALSE", multiple = FALSE)
#
# })
#
# output$ui_NodeAttri_NetworkFI <- renderUI({
#   NetworkEnrich <- c("None","Freq. Interaction", "GeneSet", "FreqInt./GeneSet")
#   selectizeInput("NodeAttri_NetworkID", label= "From ReactomeFI:", choices= NetworkEnrich,
#                  selected= "None", multiple=FALSE)
# })


# output$ui_ReacLayout <- renderUI({
#   Layouts <- c("dot","neato","twopi","circo")
#
#
#   selectizeInput("ReacLayoutId", label = "Layouts:", choices = Layouts,
#                  selected = "dot", multiple = FALSE)
#
# })

#
# output$ui_NodeAttri_ReactomeFI <- renderUI({
#   ReactomeEnrich <- c("None","Freq. Interaction", "GeneSet", "FreqInt./GeneSet")
#   selectizeInput("NodeAttri_ReactomeID", label= "From ReactomeFI:", choices= ReactomeEnrich,
#                  selected= "None", multiple=FALSE)
# })
#
# output$ui_AnnoGeneSet_ReactomeFI <- renderUI({
#   type <- c("None","Pathway", "BP","CC","MF")
#   selectizeInput("TypeGeneSetID", label="Type of enrichment:", choices=type,
#                  selected ="None", multiple=FALSE
#   )
# })
#
# output$ui_GeneSetFDR <- renderUI({
#   #if(is.null(r_data$MinGeneSetFDR)){
#   sliderInput("GeneSetFDRID", "FDR of enrichment", 0.025, min =0.0005,
#               max=0.05, step=0.0005 )
#   #}else{
#   #sliderInput("GeneSetFDRID", "FDR", 0.005, min =r_data$MinGeneSetFDR,
#   #            max=0.05, step=0.0005 )
#   #}
# })
#
# output$ui_NodeAttri_Classifier <- renderUI({
#   ClassEnrich <- c("None","mRNA","Studies", "mRNA/Studies")
#   selectizeInput("NodeAttri_ClassifierID", label= "From Classifier:", choices= ClassEnrich,
#                  selected= "None", multiple=FALSE)
# })
#
# output$ui_NodeAttri_ProfData <- renderUI({
#
#   Dimension <- c("None","CNA","Met_HM27", "Met_HM450","Mutation" )
#   selectizeInput("NodeAttri_ProfDataID", label= "Select Profiles Data:", choices= Dimension,
#                  selected= "None", multiple=TRUE)
# })
# output$ui_Freq_MutSlider <- renderUI({
#   #div(style="height: 27px;",
#   sliderInput("FreqMutSliderID", "Mutation Percentage", 25, min = 1,
#               max = 100, step = 1)
#   #)
#
# })
#
# output$ui_MetSliderHM450 <- renderUI({
#   sliderInput("ThresholdMetHM450ID", "Silencing gene rate HM450", 0.8, min =0,
#               max=1, step=0.05 )
# })
#
# output$ui_MetSliderHM27 <- renderUI({
#   sliderInput("ThresholdMetHM27ID", "Silencing gene rate HM27", 0.8, min =0,
#               max=1, step=0.05 )
# })

#
# output$ui_Network <- renderUI({
#   updateSelectizeInput(session, 'StudiesIDNetwork', choices = Studies[,1], selected = c("brca_tcga","gbm_tcga","lihc_tcga","lusc_tcga"))
#
#   tagList(
# #     conditionalPanel(condition = "input.NetworkRunId==true",
# #                      actionButton("ReacGeneListId", "load Reactome Genes")
# #     ),
#     h4("Edges Attributes:"),
#     wellPanel(
#       uiOutput("ui_FIsFilterNetwork"),
#       uiOutput("ui_UseLinkerNetwork")
#       #uiOutput("ui_Hierarchi")
#     ),
#     h4("Nodes Attributes"),
#     wellPanel(
#       uiOutput("ui_NodeAttri_NetworkFI")
# #       conditionalPanel("input.NodeAttri_ReactomeID =='GeneSet' ||
# #                        input.NodeAttri_ReactomeID =='FreqInt./GeneSet'",
# #                        uiOutput("ui_AnnoGeneSet_ReactomeFI"),
# #                        uiOutput("ui_GeneSetFDR")
# #       )
#     ),
# #     div(class="row",
# #         div(class="col-xs-4",
# #             checkboxInput("NetworkRunId", "Run" ,value = FALSE)),
# #         div(class="col-xs-4",
# #             checkboxInput("ReacLegendId", "Legend", value=FALSE))
# #     ),
#     help_modal_km('Reactome','ReactomeHelp',inclMD(file.path(r_path,"base/tools/help/Reactome.md")))
#   )
#
#
# })


