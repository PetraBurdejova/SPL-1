# https://github.com/lborke/yamldebugger
library(devtools)
devtools::install_github("lborke/yamldebugger")

# load the package every time you want to use 'yamldebugger'
library(yamldebugger)

workdir = getwd()

d_init = yaml.debugger.init(workdir, show_keywords = TRUE)

qnames = yaml.debugger.get.qnames(d_init$RootPath)

d_results = yaml.debugger.run(qnames, d_init)

OverView = yaml.debugger.summary(qnames, d_results, summaryType = "mini")
