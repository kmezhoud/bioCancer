# # if base is available in inst/ use it, else use installed radiant (i.e., for shinyapps.io)
# bp <- if (file.exists("../base"))  ".." else system.file(package = "bioCancer")
# if (bp == "") stop("bioCancer app not found")
#
# # sourcing from radiant base, note that path is set in base/global.R
# # source(file.path(bp, "base/global.R"), encoding = r_encoding, local = TRUE)
# source(file.path(bp, "base/global.R"), encoding = "UTF-8", local = TRUE)
# rm(bp)

if (file.exists("../base") && file.exists("../quant")){
  r_path <- ".."
  source("../base/global.R", encoding = "UTF-8", local = TRUE)
} else{
  r_path <- "inst"

  source("inst/base/global.R", encoding = "UTF-8", local = TRUE)
}

for (file in list.files(paste0(system.file(package ="bioCancer"),"/Rbis"),
                        pattern="\\.(r|R)$",
                        full.names = TRUE)) {

  source(file, encoding = r_encoding, local = TRUE)
}
addResourcePath("figures_quant", file.path(r_path,"quant/tools/help/figures/"))
