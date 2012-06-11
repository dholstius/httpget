cacheDir <- function() {
	path <- file.path(getwd(), '.httpget')
	if (!file.exists(path))	{
		message("Creating cache directory: ", path)
		dir.create(path)
	}
	return(path)
}

cacheFile <- function(key) {
	file.path(cacheDir(), paste(key, '.rds', sep=''))
}

cachePut <- function(key, value) {
	saveRDS(value, file=cacheFile(key))
}

cacheGet <- function(key) {
	filename <- cacheFile(key)
	if (file.exists(filename)) 
		return(readRDS(filename))
	return(NULL)
}

cacheDelete <- function(key) {
	file.remove(cacheFile(key))
}