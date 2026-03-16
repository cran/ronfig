# ronfig 0.0.9

- `load_config()` and `list_config()` gain an entry `allow_null` to control
  whether NULL entries are permitted. If `FALSE` then a NULL entry will trigger
  an error. This new argument defaults to `TRUE` for backwards compatibility.

- Minor change to documentation example.

- Vignette tweaks and change of underlying vignette engine back to
  [litedown](https://cran.r-project.org/package=litedown).

# ronfig 0.0.8

- `load_config()` (and consequently `list_config()`) gain an argument, `strict`.
  If `TRUE` (default and backwards compatible) then only the documented 
  subset of base R functions can be used within the configuration file.
  If `FALSE`, the entire base namespace is made available (in particular, `::`,
  which allows other calling of other packages).

# ronfig 0.0.7

- Fix bug when calling `load_config()` with `config = NULL`.

# ronfig 0.0.6

- Added `modifyList()` to the functions available within the configuration file.

- Added the ability to pull specific configuration values out of the returned
  object when `as_is = TRUE`.

- Added `list_config()`. This returns the names of all possible configurations
  within a file.

# ronfig 0.0.5

- Vignette tweaks and change of underlying vignette engine to
  [quarto](https://cran.r-project.org/package=quarto).

# ronfig 0.0.4

- Added `file.path()` to the functions available within the configuration file.

- Improved documentation about injection functions in to the evaluation
  environment.

# ronfig 0.0.3

- Added the following to the functions to be usable in the configuration file:
    - getters: `$`, `[` and `[[`
    - setters: `$<-`, `[<-` and `[[<-`
    
- Removed `length()` and `seq_along()` from functions available in the configuration
  file.
    
- Added the ability to injection functions in to the environment where the
  configuration file will be available. This is done by allowing the user to
  supply a list of crated functions (utilising `carrier::crate()`).

# ronfig 0.0.2

- CRAN (re)submission.

# ronfig 0.0.1

- Initial CRAN submission.
