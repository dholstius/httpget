test_that('cache exists', {
	expect_true(file.exists(cacheDir()))
})

key <- paste(LETTERS[runif(32, 0, 25)], collapse='')
object <- list(foo=runif(10), bar=rnorm(10))

test_that('cache is writable', {
	cachePut(key, object)
	expect_true(cacheContains(key))
})

test_that('cache is readable', {
	expect_equal(object, cacheGet(key))
})

test_that('cache is deletable', {
	cacheDelete(key)
	expect_true(is.null(cacheGet(key)))
	expect_false(cacheContains(key))
})