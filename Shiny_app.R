library(shiny)
library(tidyr)
library(dplyr)
library(stringr)
library(igraph)
library(ggraph)
#install.packages('networkD3')
library(networkD3)
library(magrittr)

# Tidy text frame 
library(tidytext)
library(widyr)

# Tockenisation - words

skills <- df %>%
  select(id, skills)

#skillTokens <- skills %>% 
#  unnest_tokens(word, skills, strip_numeric = TRUE, strip_punc = TRUE) 
# anti_join(stop_words) %>%
# mutate(word = SnowballC::wordStem(word))

# Tockenisation - bigrams

skillBigrams <- skills %>% 
  unnest_tokens(bigram, skills, token ='ngrams', n=2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") 

# Match additional dimensions

metaData2 <- df %>%
  mutate(location = ifelse(is.na(df$location_name), 'UK~UK~UK~UK', df$location_name)) %>%
  separate(location, c('country','region'), sep='~', fill = 'left') %>%
  mutate(description = tolower(description)) %>%
  mutate(selfEmp = case_when(str_detect(description, "self-employed") | str_detect(description, "self employed") ~ TRUE,
                             TRUE ~ FALSE)) %>%
  mutate(zeroHrs = case_when(str_detect(description, "zero hour") ~ TRUE, TRUE ~ FALSE)) %>%
  select(id, region, selfEmp, zeroHrs)

# Join additional flags to skills list

df <- skillBigrams %>%
  left_join(metaData2, by= 'id')


# Shiny 

ui <- fluidPage(
  titlePanel('Skills Network'),
  fluidRow(
    column(2, 
           selectInput('input1', 'select region', c('All', as.character(unique(df$region)))),
           selectInput('input2', 'select self-employed (true)', c('All', as.character(unique(df$selfEmp)))),
           selectInput('input3', 'select zero-hours (true)', c('All', as.character(unique(df$zeroHrs))))
           
           ),
            
    mainPanel(
      tabsetPanel(type='tabs',
                  tabPanel('Skills Network', forceNetworkOutput('force')), 
                  tabPanel('Top Skills', plotOutput('topSkills_gg'))
                  )
                  
    )
  )
)

server <- function(input, output){
  df.filtered <- reactive({
    df.f <- df
    if(input$input1!='All'){
      df.f <- df.f %>%
        filter(region==input$input1)
    }
    if(input$input2!='All'){
      df.f <- df.f %>%
        filter(selfEmp==input$input2)
    }
    if(input$input3!='All'){
      df.f <- df.f %>%
        filter(zeroHrs==input$input3)
    }
    return(df.f)
  })
  
  output$force <- renderForceNetwork({
    
    bigram_graph <- df.filtered() %>%
      count(word1, word2, sort = TRUE) %>%
      top_n(75, n) %>%
      #filter(n>20000) %>%
      graph_from_data_frame()
    
    # D3 word networks
    
    wt <- cluster_walktrap(bigram_graph)
    members <- membership(wt)
    bigrams_d3 <- igraph_to_networkD3(bigram_graph, group=members)
    
    forceNetwork(Links = bigrams_d3$links, Nodes = bigrams_d3$nodes, Source = "source",
                 Target = "target", Value = "value", NodeID = "name", Group = 'group', 
                 linkDistance = 50, opacity = 0.8,  opacityNoHover = 1,
                 fontSize = 12, linkWidth = 2, zoom=TRUE)
    
    #return(skillNetwork)
  })
  
  
  topSkills <- reactive({
    # Display top n words across the corpus
    topWords <- df.filtered() %>% 
      count(word1, sort = TRUE) %>%
      mutate(word1 = reorder(word1, n)) %>%
      top_n(25) %>%
      ggplot(aes(word1, n)) +
      geom_col(fill= "#56B4E9") +
      xlab(NULL) +
      ylab("Word Occurances") +
      coord_flip() + 
      ggtitle('Top 25 skills') + 
      theme(axis.title.y = element_text(size = 10)) +
      theme_minimal()
    
    return(topWords)
    
  })
  
  output$topSkills_gg <- renderPlot({topSkills()})
  
  
}
  
shinyApp(ui=ui, server=server)