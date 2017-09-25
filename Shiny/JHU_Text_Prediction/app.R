#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(stringr)
library(aws.s3)

## Load data

load(url("https://s3.amazonaws.com/cojamalo/data/trie_large.RData"))


## Functions

find_word <- function(this, choice_rank = 1, word_str="", first_run=TRUE){
    if(this[[1]]){ # If current node is_word, return completed word
        return(word_str)
    }
    else if (choice_rank+1 > length(this)) { # If the number of possible next letters for current node is less than the desired choice
        return(0)    
    }
    else if (first_run) {
        return(find_word(this[[choice_rank+1]], word_str=paste0(word_str, names(this[choice_rank+1])), first_run = FALSE)) 
    }
    else{
        first_run = FALSE
        return(find_word(this[[2]], word_str=paste0(word_str, names(this[2])), , first_run = FALSE))   
    }
}


get_child = function(input) {
    result = tryCatch({
        trie[[strsplit(input, "")[[1]]]]
    }, warning = function(w) {
        #warning-handler-code
    }, error = function(e) {
        return(NULL)
    }, finally = {
        #cleanup-code
    }) # END tryCatch
    return(result) 
}


letter_drop = function(input) {
    #print("letter drop output:")
    
    input = substr(input, 1, nchar(input)-1)  
    
    #print(input)
    if(input == "" | length(input) < 1) {
        return(0)    
    }
    current = get_child(input)
    found = !is.null(current)
    if(found) {
        return(list(trie=current, word=input))    
    }
    else {
        return(letter_drop(input))    
    }
}


pred = function(input, choice_no=1) {
    input_trie = get_child(input)
    success_get = !is.null(input_trie)
    #print(paste("initial word match?", success_get))
    if(!success_get) {
        drop_list = letter_drop(input) 
        success_drop = !anyNA(drop_list, recursive=FALSE)
        #print(paste("dropped word match?", success_drop))
        if(!success_drop) {
            return(NULL) # What to return if no matches after initial and letter drop   
        }
        input_trie = drop_list[[1]]
        input = drop_list[2]
    }
    #print(paste("final entry is:",input))
    result = find_word(input_trie,choice_no)
    if(result==0) {
        return(0)    
    }
    else {
        return(paste0(input,result)) 
    }
}

suggest_3 = function(input) {
    one = pred(input, 1)
    two = pred(input, 2)
    three = pred(input, 3) 
    
    if(one == two) {
        one_trie = get_child(input)
        # check length
        if(length(one_trie) >= 4) {
            two = pred(paste0(input,names(one_trie[3])), 1)
            three = pred(paste0(input,names(one_trie[4])), 2) 
        }
        else if(length(one_trie) == 3) {
            two = pred(paste0(input,names(one_trie[3])), 1)
            three = pred(paste0(input," "), 2) 
        }
        else if(length(one_trie) == 2) {
            two = pred(paste0(input," "), 1)
            three = pred(paste0(input," "), 2)   
            
        }
        else {
            two = three = 0 
        }
        
    }
    
    
    if(one != 0 & two == 0 & length(two) > 1) {
        #print("dropping for two")
        drop_list = letter_drop(input)
        success_drop = !anyNA(drop_list, recursive=FALSE)
        #print(paste("dropped word match?", success_drop))
        if(!success_drop) {
            two = 0 # What to return if no matches after initial and letter drop
        }
        else {
            #print("dropped word to predict:")
            #print(drop_list[[2]])
            two = pred(drop_list[[2]],1)
        }
        
    }
    if(one != 0 & three == 0 & length(three) > 1) {
        #print("dropping for three")
        drop_list = letter_drop(input)
        success_drop = !anyNA(drop_list, recursive=FALSE)
        #print(paste("dropped word match?", success_drop))
        if(!success_drop) {
            three = 0 # What to return if no matches after initial and letter drop
        }
        else {
            #print("dropped word to predict:")
            #print(drop_list[[2]])
            three = pred(drop_list[[2]],2)
        }
        
    }
    
    if(one==two) {
        two = 0    
    }
    if(three == one | three == two) {
        three = 0    
    }
    
    return(list(one,two,three))
    
}


clean_vector_source = function(vector_source) {
    clean_output = vector_source %>%
        str_replace_all('[^\\sA-Za-z\'\\-]', '') %>%
        str_replace_all('(?<!\\b[A-Za-z]{0,9})([-])(?![A-Za-z]+\\b)', "")
    clean_output = clean_output[!str_detect(clean_output, "[^\x20-\x7E]")]
    return(clean_output)
}

final_3 = function(input) {
    
    input = clean_vector_source(input)
    
    num_space = str_count(input, " ")
    
    if(num_space == 0 | (num_space == 1 & substr(input, nchar(input), nchar(input)) == " ")) {
        final_list = suggest_3(input) 
    }
    else if(num_space >= 1) {
        ## Check if space is last
        ending_space = (substr(input, nchar(input), nchar(input)) == " ")
        
        if(ending_space) {
            input = substr(input, 1, nchar(input)-1)        
        }
        last_word = str_extract(input, "\\s(\\w+)$") %>% str_replace(" ", "")
        last_two = str_extract(input, '\\S+\\s+\\S+$')
        
        if(ending_space) {
            last_word = paste0(last_word," ")
            last_two = paste0(last_two," ") 
        }
        first_opts = suggest_3(last_two)
        second_opts = suggest_3(last_word)
        
        final_list = list()
        for(elem in first_opts) {
            if(elem != 0 & !(elem %in% final_list)) {
                final_list = c(final_list, elem)    
            }
            if(length(final_list) == 3) {
                break    
            }
        }
        
        if(length(final_list) < 3) {
            for(elem in second_opts) {
                if(elem != 0 & !(elem %in% final_list)) {
                    final_list = c(final_list, elem)    
                }
                if(length(final_list) == 3) {
                    break    
                }
            }
        }
    }
    final_list_clean = list()
    for(elem in final_list) {
        if(elem == 0) {
            final_list_clean = c(final_list_clean, "")        
        }
        else{
            final_list_clean = c(final_list_clean, elem) 
        }
    }
    
    list_len = length(final_list_clean)
    if(list_len < 3) {
        for(i in 1:(3-list_len)) {
            final_list_clean = c(final_list_clean, "")
        }  
    }
    
    
    return(final_list_clean)
}


###

# Define UI for application that draws a histogram
ui <- fluidPage(
   
     # Application title
   titlePanel("Text Prediction Web App"),
   
   hr(),
   
   # Row of suggestions
   fluidRow(
       column(1),
       column(3, 
              verbatimTextOutput("value1")
       ),
       column(3, 
              verbatimTextOutput("value2")
       ),
       column(3, 
              verbatimTextOutput("value3")
       ),
       column(1)
   ),
   ## Input box
   fluidRow(
       column(1),
       column(10, 
              textInput("caption", "", "", placeholder="Enter text here!"),
              tags$head(tags$style(type="text/css", ".shiny-input-container:not(.shiny-input-container-inline) {width: 100%;}"))
        ),
       column(1)
   ),
   ## What the app is using to predict
   #textOutput("input_text"),
   
   
   # Directions
   fluidRow(
       column(1),
       column(10, offset = 0, align = 'left',
              htmlOutput("docs")
       ),
       column(1)
    )
   
   
   
      
      
   
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$input_text <- renderText({ input$caption })
    
    
    output$value1 = renderText({ final_3(input$caption)[[1]] })
    output$value2 = renderText({ final_3(input$caption)[[2]] })
    output$value3 = renderText({ final_3(input$caption)[[3]] })
    
    output$docs = renderText({
        "<h3>Documentation</h3>
        <p>Welcome to the Text Predictor!</p>
        <h4>Overview</h4>
        <p>This app uses learned phrases from sampled text data to predict the next word from a given charactor input.</p>
        <h4>Usage</h4>
        The app will generate up to three different suggested words or phrases to complete the given charactor input. 
        <h5><i>Text Entry Box</i></h5>
        <p>Takes any character input.</p>
        <p>Will return up to three suggestions. The suggestions attempt to complete the input sentence or try to correct the spelling of the input by suggesting the closest spelled word present in the sampled text data. Ending spaces will make a difference as an ending space indicates to the algorithm that the last typed word has ended.</p>
        "
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

