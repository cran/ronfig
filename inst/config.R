# This is our default configuration which must be a named list
# All other changes are layered on top of this.
default <- list(
    date = as.Date("2025-09-02"),
    N = 1000,
    alpha = 0.3,
    gamma = 0.2,
    beta = 0.7,
    max_delay = 30,
    years = 2006:2025
)

# You may need to debug some results and wish to set one parameter to 0 and only
# look at a reduced number of years, e.g.
debug <- list(
    years = 2024:2025,
    alpha = 0
)

# Or you may wish to consider an extended range of years
forecast <- list(
    years = 2006:2030
)
