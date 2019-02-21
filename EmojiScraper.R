# Function to fetch an Emoji dictionary from www.emojipedia.org 

ScrapeEmojis <- function(){
  
  # installing packages
  if("rvest" %in% installed.packages() != TRUE) {
    install.packages("rvest")
  }
  if("pryr" %in% installed.packages() != TRUE) {
    install.packages("pryr")
  }
  if("XML" %in% installed.packages() != TRUE) {
    install.packages("XML")
  }
  
  # attaching packages
  library(rvest)
  library(pryr)
  library(XML)
  
  # define standard Emoji pages
  pages <- c("https://emojipedia.org/people/",
             "https://emojipedia.org/nature/",
             "https://emojipedia.org/food-drink/",
             "https://emojipedia.org/activity/",
             "https://emojipedia.org/travel-places/",
             "https://emojipedia.org/objects/",
             "https://emojipedia.org/symbols/",
             "https://emojipedia.org/flags/")
  
  # define pages with skintone Emojis
  skinpages <- c("https://emojipedia.org/emoji-modifier-fitzpatrick-type-1-2/",
                 "https://emojipedia.org/emoji-modifier-fitzpatrick-type-3/",
                 "https://emojipedia.org/emoji-modifier-fitzpatrick-type-4/",
                 "https://emojipedia.org/emoji-modifier-fitzpatrick-type-5/",
                 "https://emojipedia.org/emoji-modifier-fitzpatrick-type-6/")
  
  # function to scrape and parse XML tables
  scraper <- function(url,Xpath){
    
    # exception control
    if(url =="https://emojipedia.org/emoji-modifier-fitzpatrick-type-1-2/"){Xpath <- "/html/body/div[3]/div[1]/article/section[1]/ul[2]"}
    
    #### importing data
    Emojis <- read_html(url)
    EmojiList<- html_nodes(Emojis, xpath = Xpath)[[1]]
    EmojiNode <- xmlTreeParse(EmojiList)
    EmojiList <- xmlToList(EmojiNode)
    
    # Deleting unnecessary attributes
    EmojiList$.attrs <- NULL
    
    #### Parsing Data
    Emojis <- EmojiList[seq(1,length(EmojiList),3)]
    Emojis <- sapply(Emojis, `[[`, 1) 
    length(Emojis)
    
    EmojiNames <- EmojiList[seq(2,length(EmojiList),3)]
    EmojiNames <- unlist(EmojiNames)
    length(EmojiNames)
    
    # adding byte column
    Bytes <- pryr::bytes(Emojis)
    
    DF <- data.frame(Emojis,EmojiNames,Bytes)
    return(DF)
    
  }
  
  # Initiating list objects
  Emojis <- list()
  SkinEmojis <- list()
  
  # Scraping Emojis
  Emojis <- lapply(pages,scraper, Xpath = "/html/body/div[3]/div[1]/ul")
  
  # Scraping Skin-tone Emojis
  SkinEmojis <- lapply(skinpages[],scraper, Xpath = "/html/body/div[3]/div[1]/article/section[1]/ul")
  
  # Pasting lists together
  Emojis <- c(Emojis,SkinEmojis)
  
  # collapsing list of lists into dataframe
  Emojis <- do.call(rbind, Emojis)
  
  # remove duplicates
  Emojis <- Emojis[duplicated(Emojis) == FALSE,]
  
  # fixing rownames
  rownames(Emojis) <- 1:dim(Emojis)[1]
  
  # return dictionary
  return(Emojis)
}
