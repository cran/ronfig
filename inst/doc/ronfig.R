litedown::reactor(print = NA)

file <- system.file("config.R", package = "ronfig")
cat(readChar(file, file.info(file)$size))

library(ronfig)
str(load_config(file))
str(load_config(file, "debug"))
str(load_config(file, "forecast"))

