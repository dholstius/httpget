cacheKey <- function(url) {
	stopifnot(is.character(url))
	digest(url, serialize=FALSE)
}

cacheKeys <- function() {
	filenames <- list.files(cacheDir(), full.names=FALSE)
	return(sub('\\.rds$', '', filenames))
}

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

cachePurge <- function() {
	filenames <- as.list(list.files(cacheDir(), full.names=TRUE))
	if (length(filenames) < 1) {
		message("Cache is already empty.")
	} else {
		prompt <- sprintf("Really delete contents of %s? %d files will be deleted [y/N] ", cacheDir(), length(filenames))
		really <- readline(prompt)
		if (really == 'y') {
			success <- do.call('file.remove', filenames)
		}
		return(invisible(success))
	}
}