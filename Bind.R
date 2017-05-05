#You may need to install these libraries:
#install.packages(c('httr', 'rvest', 'xml2', 'dplyr', 'data.table', 'jsonlite', 'tidytext'))

###ONLY LINE YOU'LL REALLY NEED TO CHANGE: Set to directory of streaming vis
outDir <- paste0('c:/users/niacj1/Documents/LectureINST728Q/data/')

try(dir.create(outDir, recursive = TRUE))

#Call the libraries you'll need
library(httr);
library(xml2);
library(rvest);
library(dplyr);
library(data.table);
library(jsonlite);
library(tidytext);

clUrl <- 'https://washingtondc.craigslist.org';

srcUrl <- paste0(clUrl, '/search/sss');

#Turn off SSL verification; unfortunately, this is sometimes a necessary step.
httr::set_config(httr::config( ssl_verifypeer = 0L ));

#Start an initial session
ses <- rvest::html_session(srcUrl);

#Anchors categorizing by commodity grouping from sidebar
catA <- ses %>% 
	html_nodes('#searchform > div.search-options-container > div > div.searchgroup.categories > ul.othercats') %>%
		html_nodes('a')

		
catA %>% html_attr('class')
#Get 3-digit codes for commodities, and descriptive names
catCodeA <- gsub('/search/', '', catA %>% html_attr('href'), fixed=T)
catNameA <- catA %>% html_text() %>% as.list()


#Dropdown list of categories
catB <- ses %>% 
	html_nodes('#subcatAbb') %>%
		html_nodes('option')


#Get 3-digit codes for commodities, and descriptive names
catCodeB <- catB %>% html_attr('value')
catNameB <- catB %>% html_text() %>% as.list()

catCode <- c(catCodeA, catCodeB)
catName <- c(catNameA, catNameB)

#assign codes as list names
names(catName) <- catCode


#Can now refer to it using $ or [[]] operators
catName$vga
catName[['vga']]

#Note: single brackets return the object, double brackets return the value
catName['vga']
identical(catName[['vga']], catName['vga'])
identical(catName[['vga']], catName$vga)

#Get the links:
hrefs <- ses %>% 
	rvest::html_nodes('#sortable-results > ul') %>% 
		rvest::html_nodes('a') %>% 
			#the name of the html attribute we want is "href"; this returns values as strings
			rvest::html_attr('href');

#Subset to just the item IDs you want: Unique, drop the '#' values using !grepl (NOT where '#' is found).
ids <- unique(hrefs[!grepl('#', hrefs, fixed = T)]);

#Paste the html "ids" together with the CL url:
links1 <- paste0(clUrl, ids);

#Create tibble to update later
tib <- tibble::tibble();

#Choose a sentiment lexicon ('afinn', 'bing', or 'nrc')
lex <- tidytext::get_sentiments(lexicon = 'nrc');

#FUNCTION TO UPDATE TIBBLE

updateT <- function(links){

	#Use lapply to apply the method from previous code to *all* links
	lapply(links, function(lnk){
	#Test: lnk <- links[1]
	#We assign it to "pg1" so that we only need to make a request from the CL servers once.
		pg <- httr::GET(lnk) %>% 
			xml2::read_html();
		
		#Let's get all of the "meta" HTML elements:
		metas <- pg %>% 
			rvest::html_nodes('meta');
		
		#Of course, these are just meta tags; they may or may not always contain all information in the posting. It does contain the title, but so does the page itself:
		title <- pg %>% 
			rvest::html_nodes('title') %>% 
				rvest::html_text();
		
		#Price, if available
		price <- pg %>% 
			rvest::html_nodes('span.price') %>%
				rvest::html_text();
		
		#Strip dollar signs
		nprice <- unique(gsub('$', '', price, fixed = T))
		
		#Strip commas, class as numeric
		nprice <- as.numeric(gsub(',', '', nprice, fixed = T))
		
		if(length(price) == 0){nprice <- 0}
		
		#Time user made the post
		utime <- pg %>% 
			rvest::html_nodes('.timeago') %>%
				rvest::html_attr('datetime');
		
		#Reassigning to earliest value:
		utime <- min(utime);
		
		#Store as time object
		dtime <- as.POSIXct(strptime(utime, '%Y-%m-%dT%H:%M:%S%z'))
		
		#User description
		udesc <- pg %>% 
			rvest::html_nodes('#postingbody') %>%
					html_text();
		
		#Drop the QR code bit, if any 
		udesc <- gsub('QR Code Link to This Post', '', udesc, fixed = T)
		
		genus <- substr(gsub('https://washingtondc.craigslist.org/', '', lnk, fixed = T), 1, 3)
		species <- substr(gsub('https://washingtondc.craigslist.org/', '', lnk, fixed = T), 5, 7)
	
		ccat <- pg %>% 
			rvest::html_nodes('body > section > header > nav > ul > li.crumb.category') %>%
				html_text()
		
		seller <- 'other';
		
		clncat <- gsub(' - by owner', '', ccat, fixed = T)
		if (nchar(clncat) < nchar(ccat)){
			seller <- 'owner';
		}
		if( nchar(gsub(' - by dealer', '', ccat, fixed = T)) < nchar(ccat) ) {
			clncat <- gsub(' - by dealer', '', ccat, fixed = T);
			seller <- 'dealer';	
		}
		
		#regex subb space
		icategory <-  catName[grepl(gsub('[[:space:]]', '', clncat), gsub('[[:space:]]', '', catName))][1]
		
		if(is.null(icategory)){
			catName[[species]] <- gsub('[[:space:]]', '', clncat)
			icategory <-  catName[grepl(gsub('[[:space:]]', '', clncat), gsub('[[:space:]]', '', catName))][1]
		}
		print(icategory[[1]])
		
		grpcategory <- icategory[[1]]
		
		if(is.null(icategory[[1]])){
			grpcategory <- ''
		}
		
		try({
			posting <- tibble(
				title = title, 
				url = lnk, 
				time = dtime,
				price = nprice,
				supercat = genus,
				subcat = species,
				grpcode = names(icategory),
				group = grpcategory,
				desc = udesc
			);
		
			#Then, split text into tokens: each individual word is a token.
			tibPost <- posting %>% 
				tidytext::unnest_tokens(word, desc) %>% 
					dplyr::select(title, everything());
			
			#Stop words: Common words like "and" "the" and so on
			data('stop_words')
			cleanPost <- tibPost %>%
			  anti_join(stop_words);
			
			#Get sentiments
			sent <- cleanPost  %>%
			  inner_join(lex);
			
			tib <<- rbind(tib, sent);
	 	})
	})
}


#Reshape and run regression model (log price = alpha + beta*(log matrix of sentiments))
runReg <- function(){
	grpPr <- data.table::as.data.table(tib %>% group_by(title, group) %>% summarise(avg_price = mean(price)))[avg_price > 0]
	#grpPr[avg_price > 0, avg_price := log(avg_price)]
	
	DT <- data.table::as.data.table(tib)[, .(.N, avgp = mean(price)), by = list(sentiment, title)][avgp > 0]
	dDT <- data.table::dcast(DT, title ~ sentiment, value.var = "N")
	mDT <- data.table::melt(dDT, id.vars = c('title'))
	mDT[is.na(value), value := 0]
	mDT[, value := as.numeric(value)]
	grpSn <- data.table::dcast(mDT, title ~ variable, value.var = "value")
	
	data.table::setkey(grpPr, key = title)
	data.table::setkey(grpSn, key = title)
	
	y <- grpPr[grpSn]

	y[avg_price    > 0, avg_price    := as.numeric(log(avg_price	 ))]
	y[anger        > 0, anger        := as.numeric(log(anger       ))]
	y[anticipation > 0, anticipation := as.numeric(log(anticipation))]
	y[disgust      > 0, disgust      := as.numeric(log(disgust     ))]
	y[fear         > 0, fear         := as.numeric(log(fear        ))]
	y[joy          > 0, joy          := as.numeric(log(joy         ))]
	y[negative     > 0, negative     := as.numeric(log(negative    ))]
	y[positive     > 0, positive     := as.numeric(log(positive    ))]
	y[sadness      > 0, sadness      := as.numeric(log(sadness     ))]
	y[surprise     > 0, surprise     := as.numeric(log(surprise    ))]
	y[trust        > 0, trust        := as.numeric(log(trust       ))]
	

lapply(y[,unique(group)], function(grp){	 
	
#	zrs <- lapply(y[,unique(group)], function(grp){
#		mdlstr <- 'mdl <- avg_price ~ '
#		subY <- y[group == grp]
#		if (!all(subY$anger 				== 0)){mdlstr <- paste0(mdlstr, '+anger 				')} 
#		if (!all(subY$anticipation 	== 0)){mdlstr <- paste0(mdlstr, '+anticipation  ')} 
#		if (!all(subY$disgust 			== 0)){mdlstr <- paste0(mdlstr, '+disgust 			')} 
#		if (!all(subY$fear 					== 0)){mdlstr <- paste0(mdlstr, '+fear 				  ')} 
#		if (!all(subY$joy 					== 0)){mdlstr <- paste0(mdlstr, '+joy 					')} 
#		if (!all(subY$negative 			== 0)){mdlstr <- paste0(mdlstr, '+negative 		  ')} 
#		if (!all(subY$positive 			== 0)){mdlstr <- paste0(mdlstr, '+positive 		  ')} 
#		if (!all(subY$sadness 			== 0)){mdlstr <- paste0(mdlstr, '+sadness 			')} 
#		if (!all(subY$surprise 			== 0)){mdlstr <- paste0(mdlstr, '+surprise 		  ')} 
#		if (!all(subY$trust 				== 0)){mdlstr <- paste0(mdlstr, '+trust 				')} 
#		
#		mdlstr <- gsub('~ +', '~ ', mdlstr, fixed = TRUE)
#		print(all(subY$anger 				== 0))
#		print(subY)
#		print(mdlstr)
#
#		eval(parse(text = mdlstr))
#	
#		zr <- lm(mdl, subY)
#		names(zr$coeff)[1] <- 'Intercept'
#
#	})
	regDT <- as.data.table(t(summary(zr)$coefficients))
	
	regDT[, rowname := attributes(t(summary(zr)$coefficients))$dimnames[[1]]]
	setnames(regDT, old = '(Intercept)', new = 'Intercept')
	setnames(regDT, old = names(regDT), new = paste0('reg.', names(regDT)))
	regDT[reg.rowname == 'Estimate', joiner := 1]
	y[, joiner := 1]
	
	zMrg <- merge(x = y, y = regDT, by = 'joiner', allow.cartesian = T)
	
	zMrg[, estimate := 
		reg.Intercept + 
		negative*reg.negative + 
		positive*reg.positive + 
		anger*reg.anger + 
		anticipation*reg.anticipation + 
		disgust*reg.disgust + 
		fear*reg.fear + 
		joy*reg.joy + 
		sadness*reg.sadness + 
		surprise*reg.surprise + 
		trust*reg.trust
	]

# zMrg[, err := estimate - avg_price]
#	zMrg[, .(sse = sum(err^2)), by = group]
	
#	out <- zMrg[, .(
#		title, 
#		group, 
#		avg_price 		= ifelse(avg_price 		 == 0, 0, exp(1)^avg_price		), 
#		estimate 			= ifelse(estimate 		 == 0, 0, exp(1)^estimate			), 
#		negative 			= ifelse(negative 		 == 0, 0, exp(1)^negative			), 
#		positive 			= ifelse(positive 		 == 0, 0, exp(1)^positive			),
#		anger 				= ifelse(anger 			   == 0, 0, exp(1)^anger				),
#		anticipation 	= ifelse(anticipation  == 0, 0, exp(1)^anticipation	),
#		disgust 			= ifelse(disgust 		   == 0, 0, exp(1)^disgust			),
#		fear 					= ifelse(fear 				 == 0, 0, exp(1)^fear					),
#		joy 					= ifelse(joy 				   == 0, 0, exp(1)^joy					),
#		sadness 			= ifelse(sadness 		   == 0, 0, exp(1)^sadness			),
#		surprise 			= ifelse(surprise 		 == 0, 0, exp(1)^surprise			),
#		trust 				= ifelse(trust 			   == 0, 0, exp(1)^trust        )
#	)]

	out <- zMrg[, .(
		title, 
		group, 
		avg_price 		= exp(1)^avg_price		, 
		estimate 			= exp(1)^estimate			, 
		negative 			= exp(1)^negative			, 
		positive 			= exp(1)^positive			,
		anger 				= exp(1)^anger				,
		anticipation 	= exp(1)^anticipation	,
		disgust 			= exp(1)^disgust			,
		fear 					= exp(1)^fear					,
		joy 					= exp(1)^joy					,
		sadness 			= exp(1)^sadness			,
		surprise 			= exp(1)^surprise			,
		trust 				= exp(1)^trust       
	)]


#used to be using -1s
#
#	out <- out[, .(
#		title, 
#		group, 
#		avg_price 		= ifelse(avg_price 		 < 0, 0, avg_price		), 
#		estimate 			= ifelse(estimate 		 < 0, 0, estimate			), 
#		negative 			= ifelse(negative 		 < 0, 0, negative			), 
#		positive 			= ifelse(positive 		 < 0, 0, positive			),
#		anger 				= ifelse(anger 			   < 0, 0, anger				),
#		anticipation 	= ifelse(anticipation  < 0, 0, anticipation	),
#		disgust 			= ifelse(disgust 		   < 0, 0, disgust			),
#		fear 					= ifelse(fear 				 < 0, 0, fear					),
#		joy 					= ifelse(joy 				   < 0, 0, joy					),
#		sadness 			= ifelse(sadness 		   < 0, 0, sadness			),
#		surprise 			= ifelse(surprise 		 < 0, 0, surprise			),
#		trust 				= ifelse(trust 			   < 0, 0, trust        )
#	)]
#

	out[, err := estimate - avg_price]    
#	out[, .(sse = sum(err^2)), by = group]
#ggplot(out, aes_string(x = 'avg_price', y = 'estimate')) + 
#ggplot(zMrg, aes_string(x = 'estimate', y = 'err')) + 
#  geom_point() +
#  stat_smooth(method = "lm", col = "red") 

zr$data <- zMrg[, .(
		title, 
		group, 
		avg_price 	, 
		estimate 		, 
		negative 		, 
		positive 		,
		anger 			,
		anticipation,
		disgust 		,
		fear 				,
		joy 				,
		sadness 		,
		surprise 		,
		trust 			
	)]
	
	return(zr);
}

#Function to get by-group details; using data.table is easiest approach here.
getGrpDet <- function(){

	titlePr <- data.table::as.data.table(
		tib %>% 
			group_by(group, title) %>% 
				summarise(
					avg_price = mean(price) 
				)
	)

	grpPr <- data.table::as.data.table(
		tib %>% 
			group_by(group) %>% 
				summarise(
					avg_price = mean(price), 
					std_dev = sd(price)
				)
	)
	
	N <- length(unique(tib$title))
	DT <- data.table::as.data.table(tib)[, .(.N, avgp = mean(price)), by = list(sentiment, title, group)]
	dDT <- data.table::dcast(DT, title + group ~ sentiment, value.var = "N")
	mDT <- data.table::melt(dDT, id.vars = c('title', 'group'))
	mDT[is.na(value), value := 0]
	titleSn <- data.table::dcast(mDT, group + title ~ variable, value.var = "value")
	
	grpSn <- titleSn[, .(
		anger 			 = mean(anger 			), 
		anticipation = mean(anticipation),
		disgust 		 = mean(disgust 		), 
		fear 				 = mean(fear 				), 
		joy 				 = mean(joy 				), 
		negative 		 = mean(negative 		), 
		positive 		 = mean(positive 		), 
		sadness 		 = mean(sadness 		), 
		surprise 		 = mean(surprise 		), 
		trust        = mean(trust       )
	),
	by = list(group)]
	
	data.table::setkey(grpPr, key = group)
	data.table::setkey(grpSn, key = group)
	
	y <- grpPr[grpSn]
	
	y[, total := anger + anticipation + disgust + fear + joy + negative + positive + sadness + surprise + trust]
	
#	z <- y[, .(
#		group, 
#		avg_price, 
#		std_dev, 
#		negative			=	negative		 / total, 
#		positive			=	positive		 / total, 
#		anger					=	anger				 / total, 
#		anticipation	=	anticipation / total, 
#		disgust				=	disgust			 / total, 
#		fear					=	fear				 / total, 
#		joy						=	joy					 / total, 
#		sadness				=	sadness			 / total, 
#		surprise			=	surprise		 / total, 
#		trust					=	trust				 / total
#	)]
	

	zr <- runReg();
	
	regDT <- as.data.table(t(summary(zr)$coefficients))
	
	regDT[, rowname := attributes(t(summary(zr)$coefficients))$dimnames[[1]]]
	setnames(regDT, old = '(Intercept)', new = 'Intercept')
	setnames(regDT, old = names(regDT), new = paste0('reg.', names(regDT)))
	regDT[reg.rowname == 'Estimate', joiner := 1]
	y[, joiner := 1]
	
	zMrg <- merge(x = y, y = regDT, by = 'joiner', allow.cartesian = T)
	
	zMrg[, estimate := 
		negative*reg.negative + 
		positive*reg.positive + 
		anger*reg.anger + 
		anticipation*reg.anticipation + 
		disgust*reg.disgust + 
		fear*reg.fear + 
		joy*reg.joy + 
		sadness*reg.sadness + 
		surprise*reg.surprise + 
		trust*reg.trust
	]
	
	zMrg[, err := estimate - avg_price]

	z <- zMrg[,.(
		group, 
	)]
	
	return(z);
}


updateT(unique(links1));
#Write lines to json files in our outdir
		writeLines(jsonlite::toJSON(tib %>% count(sentiment, sort=FALSE) ), paste0(outDir, 'SentimentAgg',format(Sys.time(), '%Y%m%d%H%M%S'),'.json'));
		#writeLines(jsonlite::toJSON(tib %>% count(sentiment, group, sort=FALSE) ), paste0(outDir, 'SentimentGrp',format(Sys.time(), '%Y%m%d%H%M%S'),'.json'));
		#writeLines(jsonlite::toJSON(tib %>% group_by(group) %>% summarise(avg_price = mean(price), std_dev = sd(price)), paste0(outDir, 'PriceGrp',format(Sys.time(), '%Y%m%d%H%M%S'),'.json'));
		writeLines(jsonlite::toJSON(getGrpDet()), paste0(outDir, 'GrpDetail',format(Sys.time(), '%Y%m%d%H%M%S'),'.json'));
		writeLines(jsonlite::toJSON(runReg()$coeff), paste0(outDir, 'Regression',format(Sys.time(), '%Y%m%d%H%M%S'),'.json'));
		writeLines(jsonlite::toJSON(tib), paste0(outDir, 'TibBk',format(Sys.time(), '%Y%m%d%H%M%S'),'.json'));


upCnt <- 9;
s <- 0;

#collector - some URLs don't work with this demo scraper, so we ignore them;
junk <- unique(links1);

recur <- function(){
	#Start another session, override old
	if(s == 0 | s > 1250){
		ses <<- rvest::html_session(srcUrl);
	} else {
		print(paste0(srcUrl, '?s=', s))
		ses <<- rvest::html_session(paste0(srcUrl, '?s=', s));
	}
	#Get the links:
	hrefs <- ses %>% 
		rvest::html_nodes('#sortable-results > ul') %>% 
			rvest::html_nodes('a') %>% 
				#the name of the html attribute we want is "href"; this returns values as strings
				rvest::html_attr('href');
	
	#Subset to just the item IDs you want: Unique, drop the '#' values using !grepl (NOT where '#' is found).
	ids <- unique(hrefs[!grepl('#', hrefs, fixed = T)]);
	
	#Paste the html "ids" together with the CL url:
	newlinks <- paste0(clUrl, ids);
	
	#get links already in tibble
	oldlinks <- tolower(tib$url);
	
	uplinks <- c();
	#override links var with links not already in tibble
	uniqlinks <- unique(c(uplinks, newlinks[!(tolower(newlinks) %in% oldlinks)]));	

	#Remove links in the junk drawer;
	uplinks <- uniqlinks[!(tolower(uniqlinks) %in% tolower(junk))];
	
	junk <<- c(junk, uplinks)
	
	if(length(uplinks)>0){
		try(updateT(uplinks));

		writeLines(jsonlite::toJSON(tib %>% count(sentiment, sort=FALSE) ), paste0(outDir, 'SentimentAgg',format(Sys.time(), '%Y%m%d%H%M%S'),'.json'));
		writeLines(jsonlite::toJSON(getGrpDet()), paste0(outDir, 'GrpDetail',format(Sys.time(), '%Y%m%d%H%M%S'),'.json'));
		writeLines(jsonlite::toJSON(runReg()$coeff), paste0(outDir, 'Regression',format(Sys.time(), '%Y%m%d%H%M%S'),'.json'));
		writeLines(jsonlite::toJSON(tib), paste0(outDir, 'TibBk',format(Sys.time(), '%Y%m%d%H%M%S'),'.json'));

		filelist <- list.files(outDir)
		metalist <- list(
			'sagg' = filelist[grepl('SentimentAgg', filelist, fixed=T)],
#			'sgrp' = filelist[grepl('SentimentGrp', filelist, fixed=T)],
#			'pgrp' = filelist[grepl('PriceGrp', filelist, fixed=T)],
			'grdt' = filelist[grepl('GrpDetail', filelist, fixed=T)],
			'regr' = filelist[grepl('Regression', filelist, fixed=T)],
			'tibb' = filelist[grepl('TibBk', filelist, fixed=T)]
		)
		
		tryCatch(
			{
				writeLines(jsonlite::toJSON(metalist), paste0(outDir, 'FileList.json'))
			}, 
			error = function(e){
				writeLines(jsonlite::toJSON(metalist), paste0(outDir, 'FileList.json'))
			},
			finally = {
				upCnt <<- upCnt + 1;
				s <<- 0;
		}
	)
	} else {
		bumps <- s;
		s <<- bumps + 120;
	}
	
	#Backup the whole tibble if it's been more than twenty attempts
	if (upCnt > 10){
			writeLines(jsonlite::toJSON(tib), paste0(outDir, 'TibBk',format(Sys.time(), '%Y%m%d%H%M%S'),'.json'));
			regErr <- as.data.table(runReg()$qr$qr)[, .(`(Intercept)`+ anger+ anticipation      + disgust        + fear+ joy    +  negative    +  positive     +  sadness    + surprise+trust)]
			
			upCnt <<- 0;
	}
	
	Sys.sleep(10);
	recur();
}

recur();

