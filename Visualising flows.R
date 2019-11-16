# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Description: 
# JIRA: 
# Author: Mustafa Ellam
# Comments: 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

df <- sqlQuery(connection$hive, 'SELECT * FROM test;', stringsAsFactors = FALSE) 

# Restructure wide dataframe to graph format with nodes and links
graph <- df %>%
    group_by(key) %>%
    gather('var1', 'var2', 'var3', 'var4', 'var5', 'var6', key='stage', value='response', na.rm = TRUE) %>%
    arrange(key, created_date) %>%
    select(key, created_date, to_do_type_key, to_do_status, stage, response) %>%  
    mutate(source = paste(stage, response, sep = ": ")) %>%
    mutate(target = lead(source)) %>% # get destination node from response
    filter(target != 'NA') %>% #remove source nodes that do not have a reponse, this information is captured in the previous link
    filter(target != source) %>% # remove links which loop back on themselves
    ungroup() %>%
    select(source, target) %>%
    count(source, target) %>%
    filter(n>5) # show links that have more than 5 


library(igraph)
library(networkD3)

network <- graph  %>% 
    graph_from_data_frame()

wt <- cluster_walktrap(network)
members <- membership(wt)
d3 <- igraph_to_networkD3(network, group=members)

sankeyNetwork(Links = d3$links, Nodes = d3$nodes, 
              Source = 'source', 
              Target = 'target', 
              Value = 'value', 
              Nodekey = 'name', 
              fontSize = 12, nodeWkeyth = 30, sinksRight = TRUE
              )%>%
    saveNetwork(file = 'flow.html')
