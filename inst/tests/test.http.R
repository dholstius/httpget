url <- 'http://www.berkeley.edu'

test_that('request can be made', {
	response <- httpGet(url)
	expect_false(is.null(response))
})

test_that('response is cached', {
	key <- cacheKey(url)
	cacheDelete(key)
	expect_false(cacheContains(key))
	response <- httpGet(url)
	expect_equal(attr(response, 'cacheKey'), cacheKey(url))
	expect_true(cacheContains(key))
})
