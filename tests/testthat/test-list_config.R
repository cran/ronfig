test_that("list_config works as expected", {
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

            tmp <- list(123)

            bob <- 2

            new <- list(b=3.0, f = list(h = matrix(1:9, 3), i = cc(1, 2, 3)), m = seq(from = basic$d[1], to = basic$d[3], by = "day") + 0, tmp = tmp[[1]])
        )"
        cat(text, file = filename)

        expect_identical(sort(list_config(filename)), sort(c("basic", "new")))

    }, finally = unlink(filename))
})
