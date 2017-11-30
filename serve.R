#you may need to
install.packages('servr')
library(servr)

#Change this to the directory your "index.html" is in
rootDir <- paste0(
	'c:/users/', 
	Sys.info()[[7]], 
	'/Documents/GitHub/streamvis-lecture/'
)

httd(rootDir);

