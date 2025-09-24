# ronfig 0.0.3

- Added the following to the functions to be usable in the configuration file:
    - getters: $, [ and [[
    - setters: $<-, [<- and [[<-
    
- Removed length() and seq_along() from functions available in the configuration
  file.
    
- Added the ability to injection functions in to the environment where the
  configuration file will be available. This is done by allowing the user to
  supply a list of crated functions (utilising `carrier::crate()`).

# ronfig 0.0.2

- CRAN (re)submission.

# ronfig 0.0.1

- Initial CRAN submission.
