# Course Title: Mastering R Programming
# Author: Selva Prabhakaran
# URL: https://www.packtpub.com/big-data-and-business-intelligence/mastering-r-programming-video
# Contact: selva86@gmail.com
# Website: www.r-statistics.co


mymean <- function(x, y, z) {
  .Deprecated("mean")
  (x + y + z)/3
}
mymean(1, 2, 3)

mymean <- function(x, y, z) {
  .Defunct("mean")
  (x + y + z)/3
}
mymean(1, 2, 3)

# "No repository set, so cyclic dependency check skipped."

options(repos = c(CRAN="http://cran.r-project.org"))

R.home()

# R CMD check --as-cran

# "No visible binding for global variable"

# http://stackoverflow.com/questions/8096313/no-visible-binding-for-global-variable-note-in-r-cmd-check

# http://stackoverflow.com/questions/23475309/in-r-is-it-possible-to-suppress-note-no-visible-binding-for-global-variable

# http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when

# http://r.789695.n4.nabble.com/R-CMD-check-tells-me-no-visible-binding-for-global-variable-what-does-it-mean-td1837236.html#a1995245


pkg.env <- new.env()  # create local env

pkg.env$cur.val <- 0  # assign variable to the environment

inc <- function(by=1) {
  pkg.env$cur.val <- pkg.env$cur.val + by  # access the variable from pkg.env and increment
  pkg.env$cur.val
}

inc()
inc(2)



# http://stackoverflow.com/questions/12598242/global-variables-in-packages-in-r/#12605694

