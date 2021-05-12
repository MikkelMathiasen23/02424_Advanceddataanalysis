# Libraries
suppressMessages(suppressWarnings({
  library(tidyverse)
  library(car)
  library(ggfortify)
  library(latex2exp)
  library(gridExtra)
  library(lubridate)
  library(RColorBrewer)
  library(splines)
  #library(ctsmr)
}))

# Knitr options
knitr::opts_chunk$set(echo = FALSE) 
knitr::opts_chunk$set(fig.align='center')
knitr::opts_chunk$set(fig.width=9.5)
knitr::opts_chunk$set(fig.height=5.5)
knitr::opts_chunk$set(fig.asp=NULL)
knitr::opts_chunk$set(fig.pos='H')
knitr::opts_chunk$set(out.width='90%')
knitr::opts_chunk$set(warning=FALSE)
knitr::opts_chunk$set(message=FALSE)
knitr::opts_chunk$set(results = "hide")
knitr::opts_chunk$set(cache = TRUE)

# ggplot theme
#theme_set(theme_minimal())
