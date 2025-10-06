test_that("load_config works as expected when as_is = FALSE", {
    tryCatch({
        filename <- tempfile()

        # try and exercise all of the allowed inputs
        # TODO: the + 0 to ensure underlying type is double in the mapequal comparison
        #       needs looking at. Ideally testthat would ignore type for Dates.
        text <- r"(
            basic <- list(
                a = 1L,
                b = 2.0,
                c = c("one", "two", "three"),
                d = as.Date("2020-01-01")[1] + 0:9,
                e = seq_len(10),
                f = list(g = 10, h = seq_len(10)),
                p = file.path('bob', 'george')

            )

            new <- list(b=3.0, f = list(h = matrix(1:9, 3), i = cc(1, 2, 3)), m = seq(from = basic$d[1], to = basic$d[3], by = "day") + 0)
        )"
        cat(text, file = filename)

        # when as_is = FALSE (default)
        conf <- load_config(filename, "new", default = "basic")
        expected <- list(
            a = 1L,
            b = 3.0,
            c = c("one", "two", "three"),
            d = as.Date("2020-01-01") + 0:9,
            e = 1:10,
            f = list(g = 10, h = matrix(1:9, 3), i = c("1", "2", "3")),
            m = as.Date("2020-01-01") + 0:2,
            p = file.path('bob', 'george')
        )
        expect_mapequal(conf, expected)

    }, finally = unlink(filename))
})

test_that("load_config works as expected when as_is = TRUE", {
    tryCatch({
        filename <- tempfile()

        # try and exercise all of the allowed inputs
        text <- r"(
            basic <- list(
                a = 1L,
                b = 2.0,
                c = c("one", "two", "three"),
                d = as.Date("2020-01-01") + 0:9,
                e = 1:10,
                f = list(g = 10, h = seq_len(10))
            )

            new <- list(b=3.0, f = list(h = matrix(1:9, 3), i = c("1", "2", "3")))
        )"
        cat(text, file = filename)

        # when as_is = TRUE
        conf <- load_config(filename, as_is = TRUE)
        env <- new.env()
        eval(str2expression(text), envir = env)
        expected <- as.list(env)
        expect_mapequal(conf, expected)


    }, finally = unlink(filename))
})

test_that("load_config handles environments correctly", {
    tryCatch({
        filename <- tempfile()

        # config file referencing a variable in caller environment errors
        b <- 1
        text <- "default <- list(a = b)"
        cat(text, file = filename)
        expect_error(load_config(filename))

        # config file referencing a variable in own environment works
        text <- "b <- 1; default <- list(a = b)"
        cat(text, file = filename)
        conf <- load_config(filename)
        env <- new.env()
        eval(str2expression(text), env)
        expect_identical(conf, env$default)

    }, finally = unlink(filename))
})

test_that("load_config works with accessors and setterd", {
    tryCatch({
        filename <- tempfile()

        # try and exercise all of the allowed inputs
        text <- r"(
            basic <- list(
                a = 1L,
                b = 2.0,
                c = c("one", "two", "three"),
                d = as.Date("2020-01-01") + 0:9,
                e = seq_len(10),
                f = list(g = 10, h = seq_len(10))
            )

            basic$a <- basic[["b"]] + basic$a # 2.0 + 1L

            new <- list(b=basic$a + basic[["b"]], f = list(h = matrix(1:9, 3), i = cc(1, 2, 3)))
        )"
        cat(text, file = filename)

        # when as_is = FALSE (default)
        conf <- load_config(filename, "new", default = "basic")
        expected <- list(
            a = 3,
            b = 5,
            c = c("one", "two", "three"),
            d = as.Date("2020-01-01") + 0:9,
            e = 1:10,
            f = list(g = 10, h = matrix(1:9, 3), i = c("1", "2", "3"))
        )
        expect_mapequal(conf, expected)

    }, finally = unlink(filename))
})

test_that("load_config basic error handling works", {
    tryCatch({
        filename <- tempfile()

        # create a basic config file
        text <- "default <- list(a = 1)"
        cat(text, file = filename)

        # first we check that we can parse the file as expected
        conf <- load_config(filename)
        env <- new.env()
        eval(str2expression(text), env)
        expect_identical(conf, env$default)

        # Now we check the errors
        expect_snapshot(error = TRUE, load_config(letters))
        expect_snapshot(error = TRUE, load_config(filename, as_is = letters))
        expect_snapshot(error = TRUE, load_config(filename, config = "new", as_is = TRUE))
        expect_snapshot(error = TRUE, load_config(filename, letters))
        expect_snapshot(error = TRUE, load_config(filename, default = letters))
        expect_snapshot(error = TRUE, load_config(filename, default = "bob"))

    }, finally = unlink(filename))
})

test_that("check the error handling of parsed file with load_config", {
    tryCatch({
        filename <- tempfile()

        # create config file with a call to `library` should trigger an error
        text <- "library(data.table);default <- list(a = 1)"
        cat(text, file = filename)
        expect_snapshot(error = TRUE, load_config(filename))

        # config where default is not a list should trigger an error
        text <- "default <- c(a = 1)"
        cat(text, file = filename)
        expect_snapshot(error = TRUE, load_config(filename))

        # config where default is not a named list should trigger an error
        text <- "default <- list(1)"
        cat(text, file = filename)
        expect_snapshot(error = TRUE, load_config(filename))

        # config where 'config' entry is not present
        text <- "default <- list(a = 1)"
        cat(text, file = filename)
        expect_snapshot(error = TRUE, load_config(filename, config = "george"))

        # config where config is not a list should trigger an error
        text <- "default <- list(a = 1); george <- c(a = 1)"
        cat(text, file = filename)
        expect_snapshot(error = TRUE, load_config(filename, config = "george"))

        # config where config is not a named list should trigger an error
        text <- "default <- list(a = 1); george <- list(1)"
        cat(text, file = filename)
        expect_snapshot(error = TRUE, load_config(filename, config = "george"))

    }, finally = unlink(filename))
})

test_that("check the error handling when cannot parse file", {
    tryCatch({
        filename <- tempfile()

        # create config file with a call to `library` should trigger an error
        text <- "11skfdj ;j jf  jjnfjf jjf~///"
        cat(text, file = filename)
        expect_snapshot(error = TRUE, load_config(filename))

    }, finally = unlink(filename))
})

test_that("load_config works with crated functions", {
    tryCatch({
        filename <- tempfile()

        # create a basic config file
        text <- "default <- list(a = 1, b = bob(1:4))"
        cat(text, file = filename)

        # first we check that we can parse the file as expected
        conf <- load_config(filename, crates = list(bob = carrier::crate(\(x) mean(x))))
        expect_identical(conf, list(a=1, b = mean(1:4)))

    }, finally = unlink(filename))
})


test_that("load_config crate error handling works", {
    tryCatch({
        filename <- tempfile()

        # create a basic config file
        text <- "default <- list(a = 1)"
        cat(text, file = filename)

        # Now we check the errors
        expect_snapshot(error = TRUE, load_config(filename, crates = 1))
        expect_snapshot(error = TRUE, load_config(filename, crates = list(1)))
        expect_snapshot(error = TRUE, load_config(filename, crates = list(a = 1, 2)))
        expect_snapshot(error = TRUE, load_config(filename, crates = list(a = 1, a = 2)))
        expect_snapshot(error = TRUE, load_config(filename, crates = list(a = 1, b = 2)))

        # Check name restrictions
        text <- "default <- list(a = 1, b = list(1:4))"
        cat(text, file = filename)
        expect_snapshot(
            error = TRUE,
            load_config(filename, crates = list(list = carrier::crate(\(x) mean(x))))
        )

    }, finally = unlink(filename))
})


