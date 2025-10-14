## ----comment=''---------------------------------------------------------------
file <- system.file("config.R", package = "ronfig")
cat(readChar(file, file.info(file)$size))


## -----------------------------------------------------------------------------
library(ronfig)
str(load_config(file))
str(load_config(file, "debug"))
str(load_config(file, "forecast"))


## -----------------------------------------------------------------------------
f <- tempfile()
cat("default <- list(a=mean(1:10))", file = f)
with(load_config(f), a)


## -----------------------------------------------------------------------------
crate <- carrier::crate(function(x) mean(x))
with(load_config(f, crates = list(mean = crate)), a)
unlink(f)

