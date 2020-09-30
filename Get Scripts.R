# This script downloads the Riverdale scripts from springfieldspringfield.co.uk/ as a txt for analysis.

# If the pacman package is not installed on the machine, install it and load it
if (!require("pacman")){
  
  install.packages("pacman")
  
  library(pacman)
} # End if

# Use the pacman package to load the needed packages. Do not update them.
pacman::p_load(dplyr, plyr, rvest,babynames, tidytext,tidyr,ggplot2, install = TRUE, update = FALSE)

##### FUNCTIONS #####

#' Clean strings by removing white space and puncuation
# ' 
#' @param string the string to be cleaned
#' @return a lowercase string vector with each word sep. into in each index
#' @example Clean_String("In God we  trust.   All OthERS MUst bRing DATA." )
#functions from here http://www.mjdenny.com/Text_Processing_In_R.html
Clean_String <- function(string){
  
  # Make the string lowercase
  temp <- tolower(string)
  
  # Ensure there is only one space of white space between each word.
  temp <- stringr::str_replace_all(temp,"[\\s]+", " ")
  
  # Split the vector so each word is at its own index. 
  #For example "in god we trust" "in" is at index 1, "god" is at index 2. 
  temp <- stringr::str_split(temp, " ")[[1]]
  
  # Find if there are extra white spaces in any of the indexes of the vector.
  indexes <- which(temp == "") 
  
  #If there is white space, remove those indexes of the array
  if(length(indexes) > 0){
    temp <- temp[-indexes]
  } #End if
  
  # Return a vector of strings
  return(temp)
  
} #End of Clean_String function

Clean_Text_Block <- function(text){
  # Get rid of blank lines
  indexes <- which(text == "")
  if (length(indexes) > 0) {
    text <- text[-indexes]
  }
  # See if we are left with any valid text:
  if (length(text) == 0) {
    cat("There was no text in this document! \n")
    to_return <- list(num_tokens = 0, 
                      unique_tokens = 0, 
                      text = "")
  } else {
    # If there is valid text, process it.
    # Loop through the lines in the text and combine them:
    clean_text <- NULL
    for (i in 1:length(text)) {
      # add them to a vector 
      clean_text <- c(clean_text, Clean_String(text[i]))
    }
    # Calculate the number of tokens and unique tokens and return them in a 
    # named list object.
    num_tok <- length(clean_text)
    num_uniq <- length(unique(clean_text))
    to_return <- list(num_tokens = num_tok, 
                      unique_tokens = num_uniq, 
                      text = clean_text)
  }
  
  return(to_return)
}






scriptsURLBase <- "https://www.springfieldspringfield.co.uk/view_episode_scripts.php?tv-show=riverdale-2017&episode="




rdScriptHome <- "https://www.springfieldspringfield.co.uk/episode_scripts.php?tv-show=riverdale-2017"


base <- rdScriptHome %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="content_container"]/div[3]/div[2]/div[3]')
  

baseLinks <- rdScriptHome %>%
  read_html() %>%
  html_nodes("a") %>% html_attr("href")

baseLinks <- as.data.frame(baseLinks)

colnames(baseLinks) <- "Links"

baseLinks <- baseLinks %>%
  filter(grepl("view_episode_scripts", Links))

baseLinks <- baseLinks %>% mutate(SeasonEpisode = gsub("view_episode_scripts.php?tv-show=riverdale-2017&episode=", '', Links, fixed = TRUE))

baseLinks$Season <- paste("Season", substring(baseLinks$SeasonEpisode, 2,3))

text <- rdScriptHome %>%
  read_html() %>%
  html_nodes("a") %>% html_text() 

text <- as.data.frame(text)

text <- text %>% filter(grepl("Chapter", text))

text <- as.vector(text$text)
baseLinks$Episode <- text 



baseLinks$FullLink <- paste0( scriptsURLBase,baseLinks$Links, sep ='')

scriptTokens <- data.frame(words = character(),wordNumber = numeric(), Episode = character(),EpisodeNum = numeric(), Season = character())

for (i in 1:nrow(baseLinks)){
  print(i)
   episode <- baseLinks[i, "Episode"]
   season <- baseLinks[i, "Season"]
   episodeClean <- gsub(" ", "", episode, fixed = TRUE)
   print(episode)
  script <- baseLinks[i, "FullLink"]%>%
    read_html() %>%
    html_nodes(xpath='//*[@id="content_container"]/div[2]/div[2]/div[2]/div[1]') %>% html_text()

  script <- gsub("[\r\n\t]", "", script)

  script <- gsub('\"', '', script, fixed =  TRUE)
  
  script <-  as.data.frame(script)
  
  token <- script %>%
    unnest_tokens(word, script, token = stringr::str_split, pattern = " ")
  
  token <- token %>%
    unnest_tokens(word, word)
  
  token$wordNumber <-1:nrow(token)
  
  token$Episode <- episode
  
  token$EpisodeNum <- i
  
  token$Season <- season
  
  scriptTokens <- rbind(scriptTokens, token)
  write.table(script, paste0(episodeClean,"_Script.txt",sep = ''))

}