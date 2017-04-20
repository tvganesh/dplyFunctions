#0. Count percentage of NAs
getPercentNAs <- function(df){
  l <- NULL 
  a <- dim(df)
  for(i in 1:a[2]){
    
    l[i] <- sum(is.na(df[,i]))
    print(colnames(df[i]))
    #print(format(round(l[i]*100/a[1],2),nsmall=2))
    p= l[i]*100/a[1]
    print(sprintf("%2.2f %%",p))
  }
  
}
#0c. Check duplicates
checkDuplicates <- function(df){
  a <- df %>% distinct
  duplicates  <- dim(df)[1] - dim(a)[1] 
  dup <- paste("Number of duplicates is:",duplicates)
  print(dup)
}

# Size of dataframe
size <- function(df){
  a <- dim(df)
  print(paste("Size of dataframe is:",a[1],"x",a[2]),sep="")
}

#1. List the Labor Cost against service 
listTop10MeanActivityTime <- function(df1,feature){
  # Get the column names
  n <-colnames(df1)
  # Determine the column number
  num1 <- which(n ==feature)
  
  #
  df2 <- df1 %>% select(num1,ACTIVITY_TIME) %>% group_by_(colnames(df1)[num1])  %>%
    summarize(MeanActivityTime=as.numeric(format(round(mean(ACTIVITY_TIME)/3600,2),nsmall=2)),
    StdDeviationActivityTime=as.numeric(format(round(sd(ACTIVITY_TIME)/3600,2),nsmall=2))) %>%
    mutate(Percent= format(round(MeanActivityTime*100/sum(MeanActivityTime),2),nsmall=2)) %>% 
    arrange(desc(Percent))
  df2$Percent = paste(df2$Percent,"%",sep="")
  top10 <- head(df2,10)
  top10
}

#1a
listBottom10MeanActivityTime <- function(df1,feature){
  # Get the column names
  n <-colnames(df1)
  # Determine the column number
  num1 <- which(n ==feature)
  
  #
  df2 <- df1 %>% select(num1,ACTIVITY_TIME) %>% group_by_(colnames(df1)[num1])  %>%
    summarize(MeanActivityTime=as.numeric(format(round(mean(ACTIVITY_TIME)/3600,2),nsmall=2)),
    StdDeviationActivityTime=as.numeric(format(round(sd(ACTIVITY_TIME)/3600,2),nsmall=2))) %>%
    mutate(Percent= format(round(MeanActivityTime*100/sum(MeanActivityTime),2),nsmall=2)) %>% 
    arrange(Percent)
  df2$Percent = paste(df2$Percent,"%",sep="")
  bottom10 <- head(df2,10)
  bottom10
}

plotTop10MeanActivityTimeFeature <- function(df1,feature){
  # Get the column names
  n <-colnames(df1)
  # Determine the column number
  num1 <- which(n ==feature)
  df2 <- df1 %>% select(num1,ACTIVITY_TIME) %>% group_by_(colnames(df1)[num1])  %>%
    summarize(MeanActivityTime=as.numeric(format(round(mean(ACTIVITY_TIME)/3600,2),nsmall=2)),
    StdDeviationActivityTime=as.numeric(format(round(sd(ACTIVITY_TIME)/3600,2),nsmall=2))) %>%
    mutate(Percent= format(round(MeanActivityTime*100/sum(MeanActivityTime),2),nsmall=2)) %>% 
    arrange(desc(Percent))
  df2$Percent = paste(df2$Percent,"%",sep="")
  top10 <- head(df2,10)
  names(top10) <- c("feat","MeanActivityTime","StdDeviationActivityTime","Percent")

  atitle =paste("Heading", feature)
  
  #Use aes_string. Need to reorder
  ggplot(top10,aes(x=reorder(feat,-MeanActivityTime),y=MeanActivityTime,
                   fill=feat)) + geom_bar(stat="identity") + 
    geom_errorbar(width=.1, aes(ymin=MeanActivityTime-StdDeviationActivityTime, 
                                ymax=MeanActivityTime+StdDeviationActivityTime)) +
    scale_y_continuous(labels=comma) + 
    ggtitle(atitle)  + 
    xlab(feature) + ylab("Y") + 
    geom_text(aes(label=MeanActivityTime), vjust=-.3, color="black", size=4) +
    theme(plot.title = element_text(size=16, face="bold",hjust=0.5)) +
    theme(axis.text.x = element_text(size=14,angle = 90, hjust = 1)) +
    theme(text=element_text(size=14))
  
  
}

plotBottom10MeanActivityTimeFeature <- function(df1,feature){
  # Get the column names
  n <-colnames(df1)
  # Determine the column number
  num1 <- which(n ==feature)
  df2 <- df1 %>% select(num1,ACTIVITY_TIME) %>% group_by_(colnames(df1)[num1])  %>%
    summarize(MeanActivityTime=as.numeric(format(round(mean(ACTIVITY_TIME)/3600,2),nsmall=2)),
    StdDeviationActivityTime=as.numeric(format(round(sd(ACTIVITY_TIME)/3600,2),nsmall=2))) %>%
    mutate(Percent= format(round(MeanActivityTime*100/sum(MeanActivityTime),2),nsmall=2)) %>%  
    arrange(Percent)
  df2$Percent = paste(df2$Percent,"%",sep="")
  bottom10<- head(df2,10)
  names(bottom10) <- c("feat","MA","SD","Percent")
  
  atitle =paste("LowestMA", feature)
  
  #Use aes_string. Need to reorder
  ggplot(bottom10,aes(x=reorder(feat,MeanActivityTime),y=MeanActivityTime,
                   fill=feat)) + geom_bar(stat="identity") + 
    geom_errorbar(width=.1, aes(ymin=MeanActivityTime-StdDeviationActivityTime, 
                                ymax=MeanActivityTime+StdDeviationActivityTime)) +
    scale_y_continuous(labels=comma) + 
    ggtitle(atitle)  + 
    xlab(feature) + ylab("Y") + 
    geom_text(aes(label=MeanActivityTime), vjust=-.3, color="black", size=4) +
    theme(plot.title = element_text(size=16, face="bold",hjust=0.5)) +
    theme(axis.text.x = element_text(size=14,angle = 90, hjust = 1)) +
    theme(text=element_text(size=14))
  
  
}

plotActivityTimeBoxplot <- function(df1,feature){
  # Get the column names
  n <-colnames(df1)
  # Determine the column number
  num1 <- which(n ==feature)
  df2 <- df1 %>% select(num1,ACTIVITY_TIME) %>% group_by_(colnames(df1)[num1])  %>%
    summarize(MeanActivityTime=as.numeric(format(round(mean(ACTIVITY_TIME)/3600,2),nsmall=2))) %>%
    mutate(Percent= format(round(MeanActivityTime*100/sum(MeanActivityTime),2),nsmall=2)) %>% 
    arrange(desc(Percent))
  meanActivityTime <- head(df2,10)
  names(meanActivityTime) <- c("feat","MeanActivityTime","Percent")
  featMeanActivityTime <- meanActivityTime$feat
  
  df3 <- df1 %>% filter_(lazyeval::interp(~col %in% featMeanActivityTime, 
                                          col=as.name(colnames(df1)[num1])))
  
  atitle =paste("MA", feature)
  df3$ACT_TIME= as.numeric(format(round(df3$ACTIVITY_TIME/3600,2),nsmall=2))
  ggplot(df3,aes_string(x=colnames(df1)[num1],y="ACT_TIME",col=colnames(df1)[num1])) + 
    geom_boxplot() +
    scale_y_continuous(labels=comma) + 
    ggtitle(atitle)  + ylab("Y") +
    theme(axis.text.x = element_text(size=16,angle = 70, hjust = 1)) +
    theme(plot.title = element_text(size=16, face="bold",hjust=0.5)) 
    
  
}

plotFreqPoly <-  function(df1,feature){
  
  # Get the column names
  n <-colnames(df1)
  # Determine the column number
  num1 <- which(n ==feature)
  df2 <- df1 %>% select(num1,ACTIVITY_TIME) %>% group_by_(colnames(df1)[num1])  %>%
    summarize(MeanActivityTime=as.numeric(format(round(mean(ACTIVITY_TIME)/3600,2),nsmall=2))) %>%
    mutate(Percent= format(round(MeanActivityTime*100/sum(MeanActivityTime),2),nsmall=2)) %>% 
    arrange(desc(Percent))
  meanActivityTime <- head(df2,3)
  names(meanActivityTime) <- c("feat","MeanActivityTime","Percent")
  featMeanActivityTime <- meanActivityTime$feat
  
  df3 <- df1 %>% filter_(lazyeval::interp(~col %in% featMeanActivityTime, 
                                          col=as.name(colnames(df1)[num1])))
  atitle=paste("Frequency Count",colnames(df1)[num1])
  df3$ACT_TIME=df3$ACTIVITY_TIME/3600
  atitle =paste("Frequency counts of Activity time for", feature, "in hours")
  ggplot(df3, aes_string(x="ACT_TIME",col=colnames(df1)[num1])) +
    geom_freqpoly(binwidth = 0.277,size=1) +
    ggtitle(atitle) + xlab("X") + ylab("Count") +
    theme(plot.title = element_text(size=16, face="bold",hjust=0.5)) +
    theme(axis.text.x = element_text(size=16)) +
    theme(text=element_text(size=14))
}

#1. List activity count
listCounts <- function(df1,feature){
  # Get the column names
  n <-colnames(df1)
  # Determine the column number
  num1 <- which(n ==feature)
  
  df2 <- df1 %>% select(num1) %>% 
    group_by_(colnames(df1)[num1]) %>% summarise(ActivityCount=n()) %>% 
    arrange(desc(ActivityCount))
  activityCount <- head(df2,10)
  activityCount
  
  
}

#2. Plot activity counts
plotActivityCounts <- function(df1, feature){
  # Get the column names
  n <-colnames(df1)
  # Determine the column number
  num1 <- which(n ==feature)
  
  df2 <- df1 %>% select(num1) %>% 
    group_by_(colnames(df1)[num1]) %>% summarise(ActivityCount=n()) %>% 
    arrange(desc(ActivityCount))
  
  activityCount <- head(df2,10)
  names(activityCount) <- c("feat","ActivityCount")
  
  atitle =paste("Total count ", feature)
  ggplot(activityCount,aes(x=reorder(feat,-ActivityCount),y=ActivityCount,
                           fill=feat)) + geom_bar(stat="identity") +
    ggtitle(atitle)  +
    scale_y_continuous(labels=comma) + 
    xlab(feature) + ylab("Total count") + 
    geom_text(aes(label=ActivityCount), vjust=-.3, color="black", size=4) +
    theme(plot.title = element_text(size=12, face="bold",hjust=0.5)) +
    theme(axis.text.x = element_text(size=12,angle = 90, hjust = 1)) +
    theme(text=element_text(size=16))
}

plotMovingAverageTaskArrivalIntervals <- function(df1,feature,value){
  # Get the column names
  n <-colnames(df1)
  # Determine the column number
  num1 <- which(n ==feature)
  o <- df1[,num1]==value
  df2 <- df1[o,]
  p <- df2 %>%  select(START_TIME)  %>% arrange(START_TIME) 
  numRow=nrow(p)
  # Don't plot if there are too few points
  if(numRow >=5){
    b=seq(1,numRow)
    c <- cbind(b,p)
    c$interval=0
    for(i in 2:numRow){
      
      c$interval[i]=c$START_TIME[i] - c$START_TIME[i-1]
    }
    
    atitle=paste("Moving average of task arrival interval for",value)
    ggplot(c,aes(b,as.numeric(interval))) +geom_line() + geom_smooth(method="loess") +
      ggtitle(atitle)  +
      scale_y_continuous(labels=comma) + 
      xlab("Task number") + ylab("Task arrival intervals (hours)") + 
      theme(plot.title = element_text(size=12, face="bold",hjust=0.5)) 
  }
    
  
}

plotTaskForecast <- function(df1,feature,value){
  # Get the column names
  n <-colnames(df1)
  # Determine the column number
  num1 <- which(n ==feature)
  o <- df1[,num1]==value
  df2 <- df1[o,]
  p <- df2 %>%  select(START_TIME)  %>% arrange(START_TIME) 
  numRow=nrow(p)
  # Don't plot if there are too few points
  if(numRow >=5){
    b=seq(1,numRow)
    c <- cbind(b,p)
    c$interval=0
    for(i in 2:numRow){
      c$interval[i]=c$START_TIME[i] - c$START_TIME[i-1]
    }
    
    d <- select(c,START_TIME,interval)
    names(d) <-c("ds","y")
    m <- prophet(d)
    future <- make_future_dataframe(m, periods = 90)
    #tail(future)
    forecast <- predict(m, future)
    #tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
    plot(m, forecast)
  
  }
 
}

plotTaskForecastComponents <- function(df1,feature,value){
  # Get the column names
  n <-colnames(df1)
  # Determine the column number
  num1 <- which(n ==feature)
  o <- df1[,num1]==value
  df2 <- df1[o,]
  p <- df2 %>%  select(START_TIME)  %>% arrange(START_TIME) 
  numRow=nrow(p)
  # Don't plot if there are too few points
  if(numRow >=5){
  
    b=seq(1,numRow)
    c <- cbind(b,p)
    c$interval=0
    for(i in 2:numRow){
      c$interval[i]=c$START_TIME[i] - c$START_TIME[i-1]
    }
    
    d <- select(c,START_TIME,interval)
    names(d) <-c("ds","y")
    m <- prophet(d)
    future <- make_future_dataframe(m, periods = 90)
    #tail(future)
    forecast <- predict(m, future)
  
    
    prophet_plot_components(m, forecast)
  }
}

wordFreqPlot <- function(df1,feature,value){
  # Get the column names
  n <-colnames(df1)
  # Determine the column number
  num1 <- which(n ==feature)
  o <- df1[,num1]==value
  df2 <- df1[o,]
  
  comments=as.character(df2$COMMENTS)
  # Remove "/" if it is there
  value=gsub("/","",value)
  filename <- paste(feature,"-",value,".txt",sep="")
  conn_desc <- file(filename, "wt")  
  
  writeLines(comments,con=conn_desc,sep="\n")
  close(conn_desc)
  cname <- "."
  docs <- Corpus(DirSource(cname), readerControl=list(language="en_US"))
  docs <- tm_map(docs,stripWhitespace)
  docs <- tm_map(docs, removePunctuation)
  docs <- tm_map(docs, removeNumbers)   
  docs <- tm_map(docs, tolower)
  docs <- tm_map(docs, PlainTextDocument)  
  docs <- tm_map(docs, removeWords, stopwords("english"))
  docs <- tm_map(docs, removeWords, c("jan","feb","mar","apr","may","jun",
                                      "jul","aug","sep","oct","nov","dec"))
  
  dtm <- DocumentTermMatrix(docs)  
  freq <- sort(colSums(as.matrix(dtm)),decreasing=TRUE)
  save(freq,file="../freq/freq.RData")
  wf <- data.frame(word=names(freq), frequency=freq)   

  wf1 <- head(wf,15)
  
  if(nrow(wf1) >= 1){
    atitle <- paste("Word frequency in ",feature,"-",value,sep="")
     ggplot(wf1, aes(x=reorder(word,-frequency), 
                              frequency,fill=word)) +
        geom_bar(stat="identity") + xlab("Comment word") + ylab("Frequency") + ggtitle(atitle) +
        theme(axis.text.x=element_text(size=14,angle=45, hjust=1)) +
        theme(plot.title = element_text(size=14, face="bold",hjust=0.5))
  }

  
}


plotWordCloud <- function(freq,minimum_freq){
  set.seed(142)   
  wordcloud(names(freq), freq, min.freq=minimum_freq, scale=c(5, .1),colors=brewer.pal(6, "Dark2")) 
}

plotTaskActivityHourOfDay <- function(df1,feature,value){
  # Get the column names
  n <-colnames(df1)
  # Determine the column number
  num1 <- which(n ==feature)
  o <- df1[,num1]==value
  df2 <- df1[o,]
  
  b <- df2 %>% select(START_TIME,END_TIME,START_DAY) %>% 
    mutate(day=wday(START_TIME,label=TRUE),hour=hour(START_TIME)) 
  c <- b %>% group_by(START_DAY) %>% summarize(count=n())
  d <- inner_join(b,c,by="START_DAY")
  atitle= paste("Weekly  ",feature,"-",value,sep="")
  ggplot(d,aes(x=day,ymin=hour(START_TIME),ymax=hour(END_TIME),col=day))+ 
    geom_linerange(size = 1) +
    scale_y_continuous(limits = c(0,24)) +
    xlab("Weekday") + ylab("Hour of day") +
    geom_text(aes(label=count,y=1),col="black") +
    ggtitle(atitle) +
    theme(plot.title = element_text(size=16, face="bold",hjust=0.5)) +
    theme(text=element_text(size=16))

}
