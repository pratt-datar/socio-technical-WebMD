#Loading Data
library(tidyverse)
library(lubridate)
library(gridExtra)
library(sqldf)
library(scales)
library(igraph)
library(ggrepel)

# Use this function to read the data from csv file ----

getAllData <- function(){
  data <- read_csv("./data/all_data.csv")
  # replace NA values in forum column by 'cancer_exchange'
  data %>% replace_na(list(forum = 'cancer_exchange'))  
}

# use this function to get data for a particular forum ----

getForumData <- function(forum_name, data = NULL){
  if(is.null(data)){
    data = getAllData()
  }
  data %>% filter(forum == forum_name)
}

# Function to aggregate data bi-weekly ----

#aggredateTwoWeeks_pd <- function(data){
#  data <- data %>%
#    mutate(week_year = ceiling(week(date)/2))
#  
#  # assigning Week 53 to bi-weekly Week 26
#  data[data['week_year'] == 27,'week_year'] <- 26
#  
#  # group-by bi-week and aggregating
#  data <- data %>%
#    mutate(week_year = str_c(week_year,"-",year(date))) %>%
#    group_by(week_year) %>% 
#    summarise(start = first(date),count = n()) %>% 
#    arrange(start)%>% 
#    filter(start >= '2009-01-01', start < '2014-08-01')%>%
#    mutate(start = floor_date(start,"week"))
#  
#  # creating a new column to distinguish between WebMD design change
#  data[,'transition'] <- NA
#  data['transition'][data['start'] < '2010-02-28'] <- 'Before'
#  data['transition'][data['start'] >= '2010-02-28'] <- 'After'
#  data$transition <- as.factor(data$transition)
#  return(data)
#}

# Use this function to aggregate data bi-weekly

aggregateByTwoWeeks2<-function(data) {
  # creating a sequence of weekly dates from 2009 to 2014
  d = seq(ymd('2009-01-01'),ymd('2014-08-01'), by = 'weeks')
  
  # creating a dataframe for reference weekly dates
  dates = tibble(pdate = d) %>% mutate(yweek=str_c(year(pdate),week(pdate)))
  
  # Joining forum data with reference weekly data to generate weekly traffic count
  data<-data %>% select(date,uniqueID) %>% mutate(yweek = str_c(year(date),week(date))) %>% 
    right_join(dates,by=c("yweek"="yweek")) %>% mutate(count = ifelse(is.na(uniqueID),0,1))  %>%
    group_by(yweek) %>% summarise(date = min(pdate),total = sum(count)) %>% arrange(date) %>%
    
    # additional steps to generate bi-weekly traffic count
    mutate(idx = floor((row_number()+1)/2)) %>% group_by(idx) %>% summarise(date = min(date),total = sum(total))
  return(data)
}

# Use this function to plot the bi-weekly data

plotByTwoWeeks<-function(data, forum) {
  ggplot() + 
    # plotting entire traffic
    geom_area(data=data,aes(date,total),color = 'black')+
    
    # adding a geom area with date after the design change and filling it with gray color
    geom_area(data = data %>% filter(date >= "2010-03-01"),aes(date,total),fill="gray", color = 'black')+
    
    # assigning black white theme so that traffic data before the design change would be colored in black
    theme_bw()+
    
    # adding legend, xlabel and ylabel
    ggtitle(str_c("WebMD Traffic Trends: ",forum)) + xlab('Year') + ylab('BiWeekly Post Count') +
    
    # scaling on x-axis
    scale_x_date(breaks = date_breaks("year"),labels = date_format("20%y"))
}

# Plot function ----

#plot_aggdata <- function(data,forum){
#  ggplot(data) + 
#    geom_area(aes(x= as.Date(start), y = count, fill = transition), colour = "black") +
#    ggtitle(str_c("WebMD Traffic Trends: ",forum)) + xlab('Year') + ylab('BiWeekly Post Count') + 
#    theme(legend.position="none") + 
#    scale_x_date(breaks = date_breaks("year"),labels = date_format("20%y"))+
#    scale_fill_manual(values=c("#a3a3a3","#000000"))
#    
#}

# Generate all plots ----

all_plots <- function(data, save_location){
  # getting forum names
  forums <- unique(data$forum)
  
  # applying aggregation over all forums
  data_agg <- aggregateByTwoWeeks2(data)
  
  # plotting graph for all forum's aggregated traffic
  g_all <- plotByTwoWeeks(data_agg, 'Overall')
  
  # writing png image
  #ggsave(str_c(save_location,'overall','.png'))
  
  # writing to a pdf file
  pdf(str_c(save_location,"plots.pdf"), onefile = TRUE)
  grid.arrange(g_all)
  
  # plotting and saving plots for individual forums
  for (i in forums){
    forum_data <- getForumData(i,data)
    forum_data <- aggregateByTwoWeeks2(forum_data)
    g <- plotByTwoWeeks(forum_data,i)
    grid.arrange(g)
    #ggsave(str_c(save_location,i,'.png'))
  }
  dev.off()
}


# PART 2 Dyadic Interactions ----

linkByInferredReplies<-function(data, start_date = "2009-01-01", end_date = "2014-08-01") {
  data<-data %>%
    filter(date >= start_date, date < end_date) %>%
    select(qid,uniqueID,poster,inferred_replies)%>%
    filter(inferred_replies!="NULL") %>% rename(src=poster)
  
  cols<-str_c("X",(1:(max(str_count(data$inferred_replies,","))+1)))
  
  data %>% separate(inferred_replies,cols,sep=",") %>%
    gather(key="idx",value="target",-c(qid:src)) %>%
    filter(!is.na(target)) %>% select(-uniqueID,-idx)
}

## function to filter a particular time period and look for dyadic interactions ----
#getNetwork <- function(data,start_date, end_date){
#  data <- linkByInferredReplies(data, start_date, end_date)
#  
#  data_ <- data %>% group_by(qid, src, target) %>%
#    summarise(indi_count = n())
#  
#  # creating dyadic interactions
#  dyad <- as.tibble(sqldf("select a.qid, a.src, a.target, a.indi_count, min(a.indi_count,b.indi_coun#t) as final_count from data_ a inner join data_ b on a.qid = b.qid and b.src=a.target and b.target = #a.src"))
#    dyad <- dyad %>% filter(src < target) %>%
#    group_by(src,target) %>%
#    summarise(weight = sum(final_count)) %>%
#    arrange(-weight)
#  
#  #creating non-dyadic connections
#  non_dyad <- data %>% 
#    anti_join(data,by=c("qid"="qid","src"="target","target"="src")) %>%
#    arrange(qid)
#  
#  # finding people interacted only once per thread
#  intr_once <- unique(non_dyad$target)
#  
#  dyad_intr <- list()
#  dyad_intr[[1]] <- dyad
#  dyad_intr[[2]] <- non_dyad
#  dyad_intr[[3]] <- intr_once
#  
#  return(dyad)
#}

# Use this function to get dyadic relationship in a forum in the forum of dataframe

getRelationships2 <- function(data) {
  data %>% mutate(direction = pmin(src,target) == src)%>%
    mutate(s2 = pmin(src,target),target = pmax(src,target),src=s2) %>%
    group_by(qid,src,target) %>%
    summarise(weight = min(sum(direction),sum(!direction))) %>% filter(weight > 0) %>% ungroup() %>%
    group_by(src,target) %>% summarise(weight=sum(weight)) %>% arrange(-weight)
}

# Use this function to get links by replies in a forum in the form of graph

replyNetwork <- function(data){
  data <- data %>% 
    select(src, target) %>%
    mutate(s2 = pmin(src,target), target = pmax(src,target), src = s2) %>%
    select(-s2) %>%
    group_by(src,target) %>%
    summarise(weight = n())
  graph_from_data_frame(data, directed = FALSE)
}

# Use this function to get dyadic relationship in the form of graph 

relationshipNetwork <- function(data){
  data_ <- data %>% 
    group_by(qid, src, target) %>%
    summarise(indi_count = n())
  # creating dyadic interactions
  dyad <- as.tibble(sqldf("select a.qid, a.src, a.target, a.indi_count, min(a.indi_count,b.indi_count) as final_count from data_ a inner join data_ b on a.qid = b.qid and b.src=a.target and b.target = a.src")) %>%
    filter(src < target) %>%
    group_by(src,target) %>%
    summarise(weight = sum(final_count))
  graph_from_data_frame(dyad, directed = FALSE)
}


# Use this function to get modularity for all forums

getModularity <- function(data){
  forums <- unique(data$forum)
  forums <- forums[!forums %in% 'cancer_exchange']
  forum_df <- data.frame(forum_name = character(), reply_mod = character(), relation_mod = character(), stringsAsFactors = F)
  incoming_mapping <- read_csv("./data/forum_map.csv") %>% 
    select(corpus,incoming)
  for (i in forums){
    forum_data <- getForumData(i,data)
    forum_data <- linkByInferredReplies(forum_data,'2009-01-01','2010-03-01')
    reply.mod <- modularity(cluster_louvain(replyNetwork(forum_data)))
    relation.mod <- modularity(cluster_louvain(relationshipNetwork(forum_data)))
    forum_df[nrow(forum_df)+1,] <- c(i,reply.mod, relation.mod)
  }
  forum_df$reply_mod <- as.numeric(forum_df$reply_mod)
  forum_df$relation_mod <- as.numeric(forum_df$relation_mod)
  forum_df %>% inner_join(incoming_mapping, by =c('forum_name'='corpus'))
}
 
# use this function to plot modularity for all forums
 
modplot<- function(data){
  ggplot(data, aes(x=relation_mod,y=reply_mod,color = incoming))+
    geom_point(size = 3) +
    geom_label_repel(aes(x = relation_mod, y = reply_mod,label = forum_name), color = 'black')+
    scale_color_viridis_c()+
    #scale_colour_gradientn(colours = c('darkblue','lightblue','green','yellow'))+
    geom_abline(intercept = 0, slope = 1, color="red")+
    scale_x_continuous(name = 'relations', limits = c(0,0.9))+
    scale_y_continuous(name = 'replies', limits = c(0,0.9)) + 
    labs(title = 'Modularity Graph')
}

##### PART 3

evalFitness<-function(graph,corelist,cutoff=NULL) {
  g <- igraph::as_data_frame(graph) %>% mutate(f_core = from %in% corelist, t_core = to %in% corelist)
  g <- g %>% filter(f_core==t_core) %>% mutate(ideal=as.numeric(f_core&t_core))
  cor(g$weight,g$ideal)
}

findBestCore<-function(ranked,original,window = 30) {
  count = 1
  best = 0
  fitness = 0
  peak = 0
  while (count < length(ranked) & peak<window) {
    core<-ranked[1:count]
    eval<-evalFitness(original,core)
    if (!is.na(eval)) {
      if (eval >= fitness) {
        fitness = eval
        best = count
        peak = 0
      } else {
        peak=peak+1
      }
    }
    count=count+1
  } 
  return(list(core = ranked[1:best],fitness=fitness))
}


corePeripheryApproximation<-function(graph) {
  am <- as_adjacency_matrix(graph)
  ev <- eigen(am,symmetric = T,only.values=F)
  approx <-  ev$vectors[,1:2] %*% diag(ev$values[1:2]) %*% t(ev$vectors[,1:2])
  ranked <- (tibble(name = rownames(am), rank = rowSums(approx)) %>% arrange(-rank))$name
  return(findBestCore(ranked,graph))
}

#getCutoff <- function(rrelationship_data){
#  r <- relationship_data %>% group_by(weight) %>%
#    summarise(count = n()) %>% arrange(-weight) 
#}

#CorePeriphery_calc <- function(data, start = '2009-01-01', end = '2010-03-01'){
#  forums <- unique(data$forum)
#  forums <- forums[!forums %in% c('cancer_exchange','sports_medicine')]
#  forum_df <- data.frame(forum_name = character(), core_size = character(), fitness = character(), #request_ratio = character(), volume = character(), stringsAsFactors = F)
#  for (i in forums){
#    #cat(i)
#    #cat('\n')
#    forum_data <- getForumData(i,data)
#    number_top <- dim(forum_data %>% filter(date >= start, date < end) %>% filter(str_detect#(uniqueID, "_top")))[1]
#    volume <- dim(forum_data %>% filter(date >= start, date < end))[1]
#    number_replies <- volume - number_top
#    request_ratio <- round(number_top/number_replies,2)
#    #cat(request_ratio)
#    links <- linkByInferredReplies(forum_data,start,end)
#    relation <- relationshipNetwork(links)
#    core <- corePeripheryApproximation(relation)
#    forum_df[nrow(forum_df)+1,] <- c(i,length(core[[1]]), core[[2]], request_ratio, volume)
#  }
#  forum_df$core_size <- as.numeric(forum_df$core_size)
#  forum_df$fitness <- as.numeric(forum_df$fitness)
#  forum_df$request_ratio <- as.numeric(forum_df$request_ratio)
#  forum_df$volume <- as.numeric(forum_df$volume)
#  return(forum_df)
#}

# Use this function to build Core-Periphery analysis

buildDataForCPChangeAnalysis <- function(data,period1 = c("2009-01-01","2010-03-01"),
                                         period2 = c("2013-06-01","2014-08-01")) {
  
  f <- function(name, start, end) {
    # this will get the forum specific data
    forum_data <- getForumData(name,data)
    # this will build core-periphery network containing core and fitness
    cp <- corePeripheryApproximation(relationshipNetwork(linkByInferredReplies(forum_data, start, end)))
    # filtering forum data w.r.t start and end date
    df <- forum_data %>% filter(date >= start, date < end)
    # building a tibble containing volume, request ratio, core size, fitness and forum name
    tibble(forum = name,
           volume = nrow(df),
           rratio = nrow(df %>% filter(localID == -1))/nrow(df %>% filter(localID > -1)),
           core_size = length(cp$core),
           core_fitness = cp$fitness)}
  
  # finding unique forums 
  forums <- unique(data$forum)
  # filtering out cancer_exchange and sports_medicince forums because it creates errors
  forums <- forums[!forums %in% c('cancer_exchange','sports_medicine')]
  # internal function call to create final dataframe containing additional field period
  bind_rows(bind_rows(lapply(forums, function(x) f(x, period1[1], period1[2]))),
            bind_rows(lapply(forums, function(x) f(x, period2[1], period2[2]))), .id = "period")
}

# use this function to plot core-fitness graph

plotCpComparison <- function(data) {
  # creating a dataframe containing starting 14 month forum data
  d1 <- data %>% 
    filter(period == 1) %>% 
    select(forum, core_size, core_fitness) %>% 
    rename(x1 = core_size, y1 = core_fitness)
  
  # adding additional columns to distinguish between starting and ending period, and for plotting change in core_size. 
  data <- data %>% 
    left_join(d1) %>% 
    mutate(x1 = ifelse(period == 1, NA, x1))
  
   #ggplot object containing geom_segment to visualize change in core size and core fitness
  ggplot(data) + 
    geom_segment(aes(x = x1, y = y1, xend = core_size, yend = core_fitness)
                 , color = "gray", size = .5, arrow = arrow(length = unit(0.03, "npc"))) +
    geom_point(aes(x = core_size, y = core_fitness, size = volume, color = rratio)) +
    facet_wrap(~period, ncol = 2) + 
    scale_color_viridis_c()+
    ggtitle('Core-Periphery Analysis') + xlab('Core Size') + ylab('Fitness')
  }


#core_plot <- function(forum_df,time = 'Before'){
#  ggplot(forum_df, aes(x=core_size,y=fitness,color = request_ratio, size = volume)) + 
#    geom_point() + 
#    scale_color_viridis_c() +
#    labs(title = str_c('Design Change: ',time))
#}


# PART 4 -----

# Prepare for gephi visualization
generateGephiFiles<-function(list_of_edge_tables) {
  
  f<-function(l) {
    g<-graph_from_data_frame(l,F)  
    tibble(Id=corePeripheryApproximation(g)$core,core=1)
  }
  
  f1<-function(et) {
    tibble(Id=unique(c(et$src,et$target)))
  }
  
  cores <- bind_rows(lapply(list_of_edge_tables,f),.id="interval")
  edges <- bind_rows(list_of_edge_tables,.id="interval")
  nodes <- bind_rows(lapply(list_of_edge_tables,f1),.id="interval")
  nodes <- nodes %>% left_join(cores) %>% mutate(core=replace_na(core,0))
  
  write_csv(buildNodeTimestampTable(nodes),"nodes_for_gephi.csv")
  write_csv(buildEdgeTimestampTable(edges),"edges_for_gephi.csv")
  
  return(list(nodes = nodes,edges=edges))
}

# Timestamp generation for gephi
buildIntervalAttributeString<-function(periods,values) {
  t<-tibble(s = periods,e=periods+1,v=values) %>% mutate(st = str_c(s,e,v,sep=","),st = str_c("[",st,"]")) %>% summarise(st = str_c(st,collapse=";"))
  return(str_c("<",t$st,">"))
}


buildTimestampAttributeString<-function(periods,values) {
  t<-tibble(s = periods,v=values) %>% mutate(st = str_c(s,v,sep=","),st = str_c("[",st,"]")) %>% summarise(st = str_c(st,collapse=";"))
  return(str_c("<",t$st,">"))
}


buildIntervalString<-function(periods) {
  t<-tibble(s = periods,e=periods+1)%>% mutate(st = str_c(s,e,sep=","),st = str_c("[",st,"]")) %>% summarise(st = str_c(st,collapse=";"))
  return(str_c("<",t$st,">"))
}


buildTimestampString<-function(periods) {
  t<-tibble(s = periods)%>% summarise(st = str_c(as.character(s),collapse=","))
  return(str_c("<[",t$st,"]>"))
}


buildNodeTable<-function(node_data) {
  node_data <- node_data %>% ungroup() %>% mutate(core=as.numeric(core))
  node_data %>% group_by(id) %>% summarise(`Time Interval` = buildIntervalString(period),core=buildIntervalAttributeString(period,core)) %>% rename(Id=id)
}

buildEdgeTable<-function(edge_data) {
  edge_data %>% ungroup() %>% group_by(src,target) %>% summarise(`Time Interval` = buildIntervalString(interval),weight=buildIntervalAttributeString(interval,weight)) %>% rename(Source=src,Target=target,Weight=weight)
}

buildNodeTimestampTable<-function(node_data) {
  node_data <- node_data %>% ungroup() %>% mutate(core=as.numeric(core))
  node_data %>% group_by(Id) %>% summarise(`Timeset` = buildTimestampString(interval),core=buildTimestampAttributeString(interval,core)) %>% mutate(Label=Id)
}

buildEdgeTimestampTable<-function(edge_data) {
  edge_data %>% ungroup() %>% group_by(src,target) %>% summarise(`Timestamp` = buildTimestampString(interval),weight=buildTimestampAttributeString(interval,weight)) %>% rename(Source=src,Target=target,Weight=weight)
}

##### Part 3.A

# forums to run counts for core,periphery and extra-periphery members
chi16forums <- c('fibromyalgia', 'diabetes', 'multiple_sclerosis', 'breast_cancer', 'lupus', 'epilepsy', 'hiv_and_aids', 'asthma', 'add_and_adhd', 'alzheimers', 'hepatitis', 'parkinsons_disease', 'osteoporosis')

# Use this function to get extra-periphery members

getExtraPeriphery <- function(fname) {
  # list of all unique posters in a forum
  all_posters <- unique(fname$poster)
  # calculate dyadic relationships in a forum
  rel<-getRelationships2(linkByInferredReplies(fname))
  # unique members who are involved in a dyadic relationship
  rel_posters <- unique(c(rel$src,rel$target))
  return (setdiff(all_posters,rel_posters))
}

# Use this function to get core and periphery members

getCorePeripheryUsers<-function(fname) {
  # calculate dyadic relationships in a forum
  rnet <- relationshipNetwork(linkByInferredReplies(fname))
  # calculate core members
  core <- corePeripheryApproximation(rnet)$core
  # difference between core and other members in a dyadic relationship gives periphery members
  return(list(P=setdiff(V(rnet)$name,core),C=core))
}

getCPXUserTable<-function(fname) {
  # get core-periphery members in a forum
  cp<-getCorePeripheryUsers(fname)
  # create a tibble 
  cp<-bind_rows(lapply(cp,function(x) tibble(name=x)),.id="type")
  # get posters first and then if they are not in core or periphery group label them as X-Periphery
  d<-fname %>% select(poster) %>% 
    group_by(poster) %>% 
    summarise() %>% 
    rename(name=poster) %>%
    left_join(cp) %>% mutate(type=replace_na(type,"XP"))
  return(d)
}

linkByInferredReplies2<-function(data) {
  data<-data %>% select(qid,uniqueID,poster,inferred_replies) %>% filter(inferred_replies!="NULL") %>% rename(src=poster)
  cols<-str_c("X",(1:(max(str_count(data$inferred_replies,","))+1)))
  data %>% separate(inferred_replies,cols,sep=",") %>%
    gather(key="idx",value="target",-c(qid:src)) %>%
    filter(!is.na(target)) %>% select(-idx)
}

getCPXCounts2<-function(fname) {
  # this will return a tibble containing CPX members in posters 
  cpx_u<-getCPXUserTable(fname)
  #fdata<-getForumData(fname)
  
  # getting a count of comments removing posters 
  denom<-nrow(fname %>% filter(localID > -1))
  
  # linkByInferredReplies would get us links in an entire forum
  df<-linkByInferredReplies2(fname) %>% 
    left_join(cpx_u,by=c("src"="name")) %>% rename(src_type=type) %>%
    left_join(cpx_u,by=c("target"="name")) %>% rename(targ_type=type) %>% 
    filter(!is.na(targ_type)) %>% 
    mutate(type=str_c(src_type,"_",targ_type)) %>%
    select(uniqueID,type) %>% group_by(uniqueID,type) %>% summarise() %>% mutate(count=1) %>%
    spread(type,count,fill=0) %>% ungroup() %>% select(-uniqueID) %>%
    summarise_all(sum) %>% mutate(den = denom)
  users<-cpx_u %>% group_by(type) %>% summarise(count=n()) %>% spread(type,count)
  return(bind_cols(users,df))
  #return(df)
}

# function to generate chi table for all 13 forums
getchi <- function(data){
  C = 0
  P = 0
  XP = 0
  C_C = 0   
  C_P = 0
  C_XP = 0
  P_C = 0
  P_P = 0
  P_XP = 0
  XP_C = 0
  XP_P = 0
  XP_XP = 0
  den = 0
  for (i in chi16forums){
    forum_data <- getForumData(i,data)
    cpx_prop <- getCPXCounts2(forum_data)
    # summation for all forums
    C = C + cpx_prop$C
    P = P + cpx_prop$P
    XP = XP + cpx_prop$XP
    C_C = C_C + cpx_prop$C_C
    C_P = C_P + cpx_prop$C_P
    C_XP = C_XP + cpx_prop$C_XP
    P_C = P_C + cpx_prop$P_C
    P_P = P_P + cpx_prop$P_P
    P_XP = P_XP + cpx_prop$P_XP
    XP_C = XP_C + cpx_prop$XP_C
    XP_P = XP_P + cpx_prop$XP_P
    XP_XP = XP_XP + cpx_prop$XP_XP
    den = den + cpx_prop$den
  }
  # calculating proportions over entire forums
  tibble(
    C = C,
    P = P,
    XP = XP,
    C_C_perc = C_C*100/den,
    C_P_perc = C_P*100/den,
    C_XP_perc = C_XP*100/den,
    P_C_perc = P_C*100/den,
    P_P_perc = P_P*100/den, 
    P_XP_perc = P_XP*100/den,
    XP_C_perc = XP_C*100/den,
    XP_P_perc = XP_P*100/den,
    XP_XP_perc = XP_XP*100/den)
}

##### find conversations

findConversations<-function(data,people_vector,start="2009-01-01",end="2014-08-01") {
  d<-getForumData('diabetes',data) %>% filter(date >= start, date < end)
  tmp<-d %>% filter(poster %in% people_vector) %>% 
    group_by(qid,poster) %>% summarise() %>% ungroup() %>%
    group_by(qid) %>% summarise(count = n(),people = str_c(poster,collapse = ",")) %>% 
    filter(count == length(people_vector)) %>% select(qid)
  d %>% inner_join(tmp) %>% group_by(qid) %>% mutate(thread_start = min(date)) %>% ungroup() %>% 
    arrange(thread_start,localID)
}








