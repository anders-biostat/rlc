library(Rd2md)

file.remove("api.md")
writeLines(c("---",
             "usePrism: true",
             "title: 'R/LinkedCharts API'",
             "hideTOC: 'true'",
             "api: true",
             "---"), "api.md")

for(file in list.files("man")){
    Rd2markdown(paste0("man/", file), "api.md", append = TRUE)
}

