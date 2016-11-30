# Course Title: Mastering R Programming
# Author: Selva Prabhakaran
# URL: https://www.packtpub.com/big-data-and-business-intelligence/mastering-r-programming-video
# Contact: selva86@gmail.com
# Website: www.r-statistics.co


library(spellcheckr)
spellcheckr::correct("objet")

# install https://www.latex-project.org/get/
# R CMD Rd2pdf spellcheckr

detach("package:spellcheckr")
devtools::install_github("selva86/spellcheckr")
library(spellcheckr)
spellcheckr::correct("objet")
