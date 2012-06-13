cacheKey <- function(url) {
	stopifnot(is.character(url))
	digest(url, serialize=FALSE)
}

cacheContains <- function(key) {
	file.exists(cacheFile(key))
}

cacheDir <- function(key) {
	file.path(getwd(), '.httpget')
}

cacheFile <- function(key) {
	file.path(cacheDir(), substring(key, 1, 3), substring(key, 4, 6), paste(key, '.rds', sep=''))
}

cachePut <- function(key, value, quiet=TRUE) {
	file <- cacheFile(key)
	subdir <- dirname(file) 
	if (!file.exists(subdir))	{
		if (!quiet) message("Creating cache directory: ", subdir)
		dir.create(subdir, recursive=TRUE)
	}
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
	filenames <- as.list(list.files(cacheDir(), full.names=TRUE, recursive=TRUE))
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