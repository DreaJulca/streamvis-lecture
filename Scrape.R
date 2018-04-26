#Install relevant packages (you only need to do this once, not on each run of the script).
#install.packages(c('rvest', 'ggvis', 'tidytext', 'jsonlite', 'dplyr'));

#Call the libraries you'll need
library(httr);
library(rvest);
#library(ggvis);
library(dplyr);
#library(data.table);
library(tidytext);

clUrl <- 'https://washingtondc.craigslist.org';

srcUrl <- paste0(clUrl, '/search/sss');

#Turn off SSL verification; unfortunately, this is sometimes a necessary step.
httr::set_config(httr::config( ssl_verifypeer = 0L ));

#Start a session
ses <- rvest::html_session(srcUrl);

#Look at F12 dev tools; which HTML elements are relevant to you? 
#Copy the selector, then use "%>%" operator to apply function "html_nodes" to the session, returning the list of sales items.
ses %>% 
	rvest::html_nodes('#sortable-results > ul');

#Look at the anchor ('a') nodes
ses %>% 
	rvest::html_nodes('#sortable-results > ul') %>% 
		rvest::html_nodes('a');

#Notice that there is a class of links called "Restore" that's just '#'; we want to skip these.
#The rest appear to be valid, although each link is duplicated; this is because CL attaches it to image and title separately.
#So, get the links:
hrefs <- ses %>% 
	rvest::html_nodes('#sortable-results > ul') %>% 
		rvest::html_nodes('a') %>% 
			#the name of the html attribute we want is "href"; this returns values as strings
			rvest::html_attr('href');

hrefs;

#Subset to just the item IDs you want: Unique, drop the '#' values using !grepl (NOT where '#' is found).
ids <- unique(hrefs[!grepl('#', hrefs, fixed = T)]);

#Side note here:
#just to clarify, 
grepl('#', hrefs, fixed = T);
!grepl('#', hrefs, fixed = T);

#What if we don't use '!'?
hrefs[grepl('#', hrefs, fixed = T)];


#Back to the scraping: Paste the html "ids" together with the CL url:
#links <- paste0(clUrl, ids);
links <- ids;

#Now you have a bunch of links to sales pages:
links;

#Let's make a GET request to read the information from the first page (using httr) just to see what happens.
#We assign it to "pg1" so that we only need to make a request from the CL servers once.
pg1 <- httr::GET(links[1]) %>% 
	xml2::read_html();
pg1;

#Let's get all of the "meta" HTML elements:
metas <- pg1 %>% 
	rvest::html_nodes('meta');
metas;

#Of course, these are just meta tags; they may or may not always contain all information in the posting. It does contain the title, but so does the page itself:
title <- pg1 %>% 
	rvest::html_nodes('title') %>% 
		rvest::html_text();

#"postinginfo" classes
pinfo <- pg1 %>% 
	rvest::html_nodes('.postinginfo');
pinfo;

#Price, if available
price <- pg1 %>% 
	rvest::html_nodes('span.price') %>%
		rvest::html_text();

#Strip dollar signs
nprice <- gsub('$', '', price, fixed = T)

#Strip commas, class as numeric
nprice <- as.numeric(gsub(',', '', nprice, fixed = T))


#Time user made the post
utime <- pg1 %>% 
	rvest::html_nodes('.timeago') %>%
		rvest::html_attr('datetime');

#Take a look at this one:
utime;

#Interesting; the "timeago" DOM class is duplicated in the page -- this can happen when using "class" selectors,
#which start with '.' (not so much "id" selectors, which start with '#')
#So we can deal with it by reassigning to only unique values:
utime <- unique(utime);

#Store as time object
dtime <- as.POSIXct(strptime(utime, '%Y-%m-%dT%H:%M:%S%z'))
#format(dtime, '%Y-%m-%dT%H:%M:%S%z')

#User description
udesc <- pg1 %>% 
	rvest::html_nodes('#postingbody') %>%
			html_text();

udesc;
writeLines(udesc);

#Drop the QR code bit, if any 
udesc <- gsub('QR Code Link to This Post', '', udesc, fixed = T)

#Cool. Let's use the tidytext library to get the sentiment from the user description.
#First, choose a sentiment lexicon ('afinn', 'bing', or 'nrc')

lex <- tidytext::get_sentiments(lexicon = 'nrc');

#Look at the unique sentiments we have in our lexicon. We'll use these in our "tibble"
unique(lex$sentiment)


posting <- tibble(
	title = title, 
	time = dtime,
	price = nprice,
	desc = udesc #,
	#url = lnk
)


#Then, split text into tokens: each individual word is a token.
tibPost <- posting %>% 
	tidytext::unnest_tokens(word, desc) %>% 
		dplyr::select(title, everything())


#Stop words: Common words like "and" "the" and so on
data('stop_words')
cleanPost <- tibPost %>%
  anti_join(stop_words)

#Get sentiments
sent <- cleanPost  %>%
  inner_join(lex) 

sent %>% 
	count(sentiment, sort = TRUE) 

posting %>% mutate(sentiments = emotCnt)



