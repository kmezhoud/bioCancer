
## ui for data menu in radiant
do.call(navbarPage,
        c("bioCancer",
          #img(src="imgs/icon.png", style="float:right; padding-right:25px"),
          getOption("cBioPortal.nav_ui"),
          getOption("Enrichment.nav_ui"),
          getOption("radiant.nav_ui"),
          #getOption("Tools.nav_ui"),
          getOption("radiant.shared_ui"),
          help_menu("help_bioCancer_ui")
        )
)
