## ui for base radiant
shinyUI(
  do.call(navbarPage, c("CancerPortal", nav_ui, shared_ui))
)

