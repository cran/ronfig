# load_config basic error handling works

    Code
      load_config(letters)
    Condition
      Error in `load_config()`:
      ! `filename` must be a string.

---

    Code
      load_config(filename, as_is = letters)
    Condition
      Error in `load_config()`:
      ! `as_is` must be a boolean.

---

    Code
      load_config(filename, config = "new", as_is = TRUE)
    Condition
      Error in `load_config()`:
      ! `config` can only be given when `as_is` is FALSE.

---

    Code
      load_config(filename, letters)
    Condition
      Error in `load_config()`:
      ! `config` must be a string.

---

    Code
      load_config(filename, default = letters)
    Condition
      Error in `load_config()`:
      ! `default` must be a string.

---

    Code
      load_config(filename, default = "bob")
    Condition
      Error in `load_config()`:
      ! Cannot find `default` entry ("bob") in the rconfig file.

# check the error handling of parsed file with load_config

    Code
      load_config(filename)
    Condition
      Error in `load_config()`:
      ! Unable to load configuration file: could not find function "library"
      i Only the following functions are available to use in rconfig files:
      * <-, =, +, -, *, :
      * as.Date, as.Date.character
      * array, matrix
      * list, data.frame
      * c, cc
      * [, [.data.frame, [.Date, [.POSIXct
      * $, [[, [[.data.frame, [[.Date, [[.POSIXct
      * $<-, $<-.data.frame
      * [<-, [<-.data.frame, [<-.Date, [<-.POSIXct
      * seq, seq.default, seq.int, seq.Date
      * sequence, sequence.default, seq_len
      * Sys.Date, Sys.time

---

    Code
      load_config(filename)
    Condition
      Error in `load_config()`:
      ! `default` entry must be a named list.

---

    Code
      load_config(filename)
    Condition
      Error in `load_config()`:
      ! `default` entry ("default") must be a non-empty and uniquely-named list.

---

    Code
      load_config(filename, config = "george")
    Condition
      Error in `load_config()`:
      ! Cannot find `config` entry ("george") in the rconfig file.

---

    Code
      load_config(filename, config = "george")
    Condition
      Error in `load_config()`:
      ! `config` entry ("george") must be a named list.

---

    Code
      load_config(filename, config = "george")
    Condition
      Error in `load_config()`:
      ! `config` entry ("george") must be a non-empty and uniquely-named list.

# check the error handling when cannot parse file

    Code
      load_config(filename)
    Condition
      Error in `load_config()`:
      ! Unable to load configuration file
      Caused by error in `parse()`:
      ! 1:3: unexpected symbol
      1: 11skfdj
            ^

# load_config crate error handling works

    Code
      load_config(filename, crates = 1)
    Condition
      Error in `load_config()`:
      ! `crates` must be a named list of `carrier::crate()`.

---

    Code
      load_config(filename, crates = list(1))
    Condition
      Error in `load_config()`:
      ! `crates` must be a uniquely-named list of `carrier::crate()`.

---

    Code
      load_config(filename, crates = list(a = 1, 2))
    Condition
      Error in `load_config()`:
      ! `crates` must be a uniquely-named list of `carrier::crate()`.

---

    Code
      load_config(filename, crates = list(a = 1, a = 2))
    Condition
      Error in `load_config()`:
      ! `crates` must be a uniquely-named list of `carrier::crate()`.

---

    Code
      load_config(filename, crates = list(a = 1, b = 2))
    Condition
      Error in `load_config()`:
      ! `crates` must be a uniquely-named list of `carrier::crate()`.
      x Entry a has class numeric.

---

    Code
      load_config(filename, crates = list(list = carrier::crate(function(x) mean(x))))
    Condition
      Error in `load_config()`:
      ! "list" cannot be used as a crated function name.

