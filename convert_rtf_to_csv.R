library(striprtf)
library(stringr)

files <- c("Factiva-20211001-1439.rtf", "Factiva-20211001-1445.rtf", "Factiva-20211001-1442.rtf", "Factiva-20211001-1421.rtf")



#this identify the end of articles
regex_identifier = regex("(Document [a-zA-Z0-9]+)")

#this identify the word count
regex_word = regex(".*[0-9]{1,3} (mots|words).*")

#this identify the line
regex_date = regex("[0-9]{1,2} (January|February|March|April|May|June|July|August|September|October|November|December) [0-9]{4}")

#this identify where the article starts (NOTE THESE VARY DEPENDING ON SOURCE AND COUNTRY)
regex_copy = regex("(Copyright [0-9]{4}+)")
regex_copy2 = regex("(© [0-9]{4}+)")

#These are the vector we want to turn into dataframe  
titles = c()
words = c()
dates = c()
works = c()
text = ""
#is_title this tell you if the title has been collected
is_title = 0
#this tells you when the code got to the article
is_article = 0 

for (file in files){
  file <- read_rtf(file)
  for (line in file){
    #this remove empty line and remove the header before the title. 
    if (str_trim(line) == "" | str_trim(line) %in% c("World",
                                                     "General News",
                                                     "Business",
                                                     "Sport",
                                                     "Opinion") ){
    }
    #this collect the title and the code above ensure there no headings
    else if (is_title == 0){
      titles <- append(titles, line)
      is_title = 1
    }
    #this use the copy right state as a way to tell the article starts
    else if (str_detect(line, regex_copy) | str_detect(line, regex_copy2) ){
      is_article = 1
      print("boom")
    }
    #this collects the total number fo words for the artcle
    else if (str_detect(line, regex_word)){
      words <- append(words, line)
    }
    #this collects the date the article was written
    else if (str_detect(line, regex_date)){
      dates <- append(dates, line)
    }
    #this tell you when the article ends using the Document number
    else if(str_detect(line, regex_identifier)){
      works <- append(works, text)
      text = ""
      is_title = 0
      is_article = 0
      
    }
    #this copies the aricle
    else if(is_article == 1) {
      text <- paste(text, line, sep=" ")
    }
  }
}

#turn the vector into a dataframe 
dataframe <- data.frame(dates, words, titles, works)

#this save the files as a csv file. 
write.csv(dataframe, "corpus.csv", row.names = FALSE)

