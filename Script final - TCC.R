library("dplyr")
library("tidyr")
library("ggplot2")
library("emmeans")
library("textdata")
library(tidytext)
library(rtweet)
library("twitteR")
library(cowplot)
library(esquisse)


#######
# Carregamento dos dados

data<-read.csv("data_percp_amazon.csv",h=T,sep=";")
dim(data)
head(data)


data %>%
  group_by(week) %>%
  summarize(freq=n()) %>%
  ggplot() +
  aes(x = week, y = freq) +
  geom_line(size = 1.2,colour="#56B4E9") +
  theme_void()+
  ggtitle("a)")+
  theme(axis.line.y.left = element_line(size=1))+
  theme(axis.line.x.bottom = element_line(size=1))+
  theme(axis.text.x.bottom = element_text(colour="black",size=15))+
  theme(axis.text.y = element_text(colour="black",size = 15))+
  theme(axis.title.x= element_text(colour="black",size=15))+
  theme(axis.title.y = element_text(colour="black",size=15,angle = 90))+
  theme(plot.title = element_text(colour="black",size=15))+
  scale_y_continuous(name="Número de citações")+
  scale_x_continuous(name="Semana",breaks=c(0,2,4,6,8,10,12,14,16,18))+
  scale_colour_manual(values=c("#E69F00"))+
  theme(legend.title=element_blank())+
  theme(legend.position = "right")+
  theme(legend.text = element_text(colour="black",size=15))



##### limpeza


data $text <- gsub("#([a-z|A-Z|0-9|_])*","", data$text) # remove hashtags
data $text <- gsub('@([a-z|A-Z|0-9|_])*', '', data$text) # remove palavras com @ (menções)
data $text <- gsub('https://','', data$text) # removes https://
data $text <- gsub('https','', data$text) # removes https://
data$text <- gsub('http://','', data$text) # removes http://
data$text <- gsub('[^[:graph:]]', ' ', data$text) # removes graphic characters like emoticons
data$text <- gsub('[[:punct:]]', '', data$text) # removes punctuation
data$text <- gsub('[[:cntrl:]]', '', data$text) # removes control characters
data$text <- gsub("\\w*[0-9]+\\w*\\s*", "", data$text) # removes numbers
data$text <- gsub('_', ' ', data$text) # replace underline by space
data$text = gsub('[ \t]{2,}', ' ', data$text) ## remove unnecessarie spaces
data$text = gsub('^\\s+|\\s+$', ' ', data$text)   ## remove unnecessarie spaces
data$text <- tolower(data$text) # minor case
data$text  = gsub('\\n', ' ', data$text ) ### remove rows breaks


### Remoção de tweets duplicados baseado em texto e momento de publicação
data <- data[!duplicated(data %>% select("text", "date_time")),]
dim(data)
head(data)


##### Let's to add a id to identify tweets
data$tweet_id<-seq(from=1,length(data$text))





##3 tokenization
data_token<-data %>%  unnest_tokens(word, text)
dim(data_token)


#### stop words
##### Remoção de RT, Amazon e Rainforest
data_token<-data_token %>%
  filter(., word!="rt") %>%
  filter(., word!="amazon") %>%
  filter(.,word!="rainforest")

data_token$word[data_token$word=="fires"]<-"fire"

dim(data_token)
### remove words not useful
data(stop_words)  ### a df with classification of words

#### antijoin to remove this words
tidy_text <- data_token %>%
  anti_join(stop_words)

head(tidy_text)
dim(tidy_text)

### dicionário de sentimentos

text_sentiment <- tidy_text %>%
  inner_join(get_sentiments("bing"))


dim(text_sentiment)



######## análise exploraória[

## correct data information


data$day<-as.Date(data$day,"%d/%m/%y")


## variqação temporal no número de publicações

p_tweets<-data %>%
  group_by(day) %>%
  summarize(freq=n_distinct(tweet_id)) %>%
  ggplot() +
  aes(x = day, y = freq) +
  geom_line(size = 1.2,colour="#56B4E9")+
  theme_void()+
  theme(axis.line.y.left = element_line(size=1))+
  theme(axis.line.x.bottom = element_line(size=1))+
  theme(axis.text.x.bottom = element_text(colour="black",size=15))+
  theme(axis.text.y = element_text(colour="black",size = 15))+
  theme(axis.title.x= element_text(colour="black",size=15))+
  theme(axis.title.y = element_text(colour="black",size=15,angle = 90))+
  theme(plot.title = element_text(colour="black",size=15))+
  scale_y_continuous(name="Número de tweets")+
  scale_x_date(name="Dia de publicação",date_labels = "%d/%m",date_breaks = "2 week")+
  scale_colour_manual(values=c("#E69F00"))+
  theme(legend.title=element_blank())+
  theme(legend.position = "right")+
  theme(legend.text = element_text(colour="black",size=15))

tiff("Fig_tweets.tiff",width=3000, height=2000,res=300)
p_tweets
dev.off()


data %>%
  group_by(mont) %>%
  summarize(freq=n_distinct(tweet_id)) %>%
  arrange((freq))


### levantamento de palavras mnais frequentes

##20 mais frequentes




w20<-tidy_text %>%
  group_by(word) %>%
  summarize(freq=n(),
            perc=(freq/nrow(tidy_text))*100)%>%
  slice_max(., order_by =freq, n=20)

w20

w20$word <-as.factor(w20$word)
w20$word<-factor(w20$word,levels=w20$word[order(w20$freq)])
levels(w20$word)

write.csv(w20,"w20.csv")


### Fazer a figura de frequencias

w20_thin<-w20 %>%
  pivot_longer(!word, names_to="variable",values_to="value")

w20_thin$variable<-as.factor(w20_thin$variable)
levels(w20_thin$variable)<-c("Número de citações","Representatividade (%)")


p_freq<- w20_thin %>%
    filter(., variable=="Número de citações") %>%
    ggplot(.,color=variable,aes(x=word,y=variable,fill=value,alpha=value))+
    geom_tile()+
    scale_y_discrete(position = "right")+
    scale_x_discrete(limits = (levels(w20_thin$word)))+
    coord_flip()+
    theme_light()+
    scale_fill_gradient(low = "white", high = "#56B4E9")+
    theme(axis.text.x= element_text(colour="black",size=18))+
    theme(axis.text.y = element_text(colour="black",size = 18))+
    theme(axis.title.x= element_blank())+
    theme(axis.title.y = element_blank())+
    theme(legend.text = element_text(color="black", size=18))+
    theme(legend.title=element_text(colour="black",size = 18))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(panel.border = element_rect(color="black"))+
    geom_text(aes(label = round(value,2)),size=5,color="black",alpha=1)+
    theme(legend.position = "none")

p_perc<-w20_thin %>%
    filter(., variable=="Representatividade (%)") %>%
    ggplot(.,color=variable,aes(x=word,y=variable,fill=value,alpha=value))+
    geom_tile()+
    scale_y_discrete(position = "right")+
    scale_x_discrete(limits = (levels(w20_thin$word)))+
    coord_flip()+
    theme_light()+
    scale_fill_gradient(low = "white", high = "#009E73")+
    theme(axis.text.x= element_text(colour="black",size=18))+
    theme(axis.text.y = element_blank())+
    theme(axis.title.x= element_blank())+
    theme(axis.title.y = element_blank())+
    theme(legend.text = element_text(color="black", size=18))+
    theme(legend.title=element_text(colour="black",size = 18))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(panel.border = element_rect(color="black"))+
    geom_text(aes(label = round(value,2)),size=5,color="black",alpha=1)+
    theme(legend.position = "none")


tiff("Fig_tile_freq.tiff",width=3000, height=3000,res=300)
plot_grid(p_freq,p_perc,rel_widths =  c(0.8,0.6))
dev.off()


#### Variação temporal das 5 +
colorBlindBlack8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                       "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

w5<-tidy_text %>%
  group_by(word,day) %>%
  summarize(freq=n())%>%
  filter(word %in% c("carbon","fire","people","deforestation","indigenous"))

w5$day<-as.Date(w5$day,"%d/%m/%y")

esquisser(w5)

ggplot(w5) +
  aes(x = day, y = freq, colour = word) +
  geom_line(size = 0.5) +
  scale_color_hue(direction = 1) +
  theme_minimal()

w5_p<-ggplot(w5) +
  aes(x = day, y = freq, colour = word) +
  geom_line(size = 1.2) +
  theme_void()+
  theme(axis.line.y.left = element_line(size=1))+
  theme(axis.line.x.bottom = element_line(size=1))+
  theme(axis.text.x.bottom = element_text(colour="black",size=15))+
  theme(axis.text.y = element_text(colour="black",size = 15))+
  theme(axis.title.x= element_text(colour="black",size=15))+
  theme(axis.title.y = element_text(colour="black",size=15,angle = 90))+
  theme(plot.title = element_text(colour="black",size=15))+
  scale_y_continuous(name="Número de citações")+
  scale_x_date(name="Dia de publicação",date_labels = "%d/%m",date_breaks = "2 week")+
  scale_colour_manual(values=c("#E69F00", "#56B4E9", "#009E73",
                               "#F0E442","#CC79A7"))+
  theme(legend.title=element_blank())+
  theme(legend.position = "right")+
  theme(legend.text = element_text(colour="black",size=15))



tiff("Fig_time_5.tiff",width=3000, height=2000,res=300)
w5_p
dev.off()




####### Análise de sentimento

text_sentiment

text_sentiment$sentiment<-as.factor(text_sentiment$sentiment)
levels(text_sentiment$sentiment)<-c("Negativo","Positivo")

### Quantificar a proporção de cada sentimento em cada tweet

df_comp<-text_sentiment %>%
  group_by((tweet_id))%>%
  summarise(total_words=n_distinct(word),
            n_positive=n_distinct(word[sentiment=="Positivo"]),
            n_negative=n_distinct(word[sentiment=="Negativo"]),
            Positivo=(n_positive/total_words)*100,
            Negativo=(n_negative/total_words)*100)

## mudar o formato dos dados
df_comp<-df_comp[,c(1,2,5:6)] %>%
  gather(., key=type,value=perc,3:4)

summary(df_comp)


## let  s evaluate whether there are differences between positive and negative
m1<-glm((perc)~type,data=df_comp)
hist(resid(m1))
summary(m1)


cat=lsmeans(m1,list(pairwise~type),type="response")
cat

cat=emmeans:::cld.emmGrid(cat$`pairwise differences of type`,
                             alpha=0.05,
                             Letters=letters,      ### Use lower-case letters for .group
                             adjust="tukey",reverse=T)

cat



### let's to obtain the mean values of each category
gdata<-df_comp %>%
  group_by(type) %>%
  summarise(mean=mean(perc),
            sd=sd(perc))



#### a plot for the means
gdata$type<-as.factor(gdata$type)
levels(gdata$type)<-c("Negativo","Positivo")


p_comp<-ggplot(gdata) +
  aes(x = type, y = mean,fill=type) +
  geom_bar(stat="identity",width = 0.5,
           position=position_dodge(width=0.8),colour="black") +
  theme_void()+
  scale_y_continuous(name="Proporção média (%)",limits=c(0,100))+
  scale_x_discrete(name=NULL)+
  scale_fill_manual(values=c("#cc79a7","#56b4e9"))+
  theme(axis.text.x=element_text(colour="black",size=15))+
  theme(axis.text.y= element_text(colour="black",size = 15))+
  theme(axis.title.y = element_text(colour="black",size=15,angle=90))+
  theme(axis.title.x = element_text(colour="black",size = 15))+
  theme(plot.title = element_text(colour="black",size = 15))+
  theme(legend.text = element_text(color="black", size=15))+
  theme(legend.title=element_blank())+
  theme(legend.position = "none")+
  geom_text(aes(label=round(mean,2)),size=6,position=position_dodge(width=0.8),vjust=-2)+
  theme(axis.line.y.left = element_line(size=1.1))+
  theme(axis.line.x.bottom = element_line(size=1.1))


tiff("Fig_comp.tiff",width=2000, height=2000,res=300)
p_comp
dev.off()


######## composição tempo

df_comp_time<-text_sentiment %>%
  group_by(day)%>%
  summarise(total_words=n_distinct(word),
            n_positive=n_distinct(word[sentiment=="Positivo"]),
            n_negative=n_distinct(word[sentiment=="Negativo"]),
            Positivo=(n_positive/total_words)*100,
            Negativo=(n_negative/total_words)*100)

## mudar o formato dos dados
df_comp_time<-df_comp_time[,c(1,5:6)] %>%
  gather(., key=type,value=perc,2:3)

df_comp_time$day<-as.Date(df_comp_time$day,"%d/%m/%y")
#Negativo = 68.46975
#positivo = 31.53025

df_comp_time[df_comp_time$week==4 & df_comp_time$type=="Positivo",3]<-31.53025
df_comp_time[df_comp_time$week==4 & df_comp_time$type=="Negativo",3]<-68.46975

p_sent_time<-ggplot(df_comp_time) +
  aes(x = day, y = perc, colour = type) +
  geom_line(size = 1.2) +
  theme_void()+
  theme(axis.line.y.left = element_line(size=1))+
  theme(axis.line.x.bottom = element_line(size=1))+
  theme(axis.text.x.bottom = element_text(colour="black",size=15))+
  theme(axis.text.y = element_text(colour="black",size = 15))+
  theme(axis.title.x= element_text(colour="black",size=15))+
  theme(axis.title.y = element_text(colour="black",size=15,angle = 90))+
  theme(plot.title = element_text(colour="black",size=15))+
  scale_y_continuous(name="Proporção média")+
  scale_x_date(name="Dia de publicação",date_labels = "%d/%m",date_breaks = "2 week")+
  scale_fill_manual(values=c("#56b4e9", "#cc79a7"))+
  theme(legend.title=element_blank())+
  theme(legend.position = "right")+
  theme(legend.text = element_text(colour="black",size=15))



tiff("Fig_comp_time.tiff",width=3000, height=2000,res=300)
p_sent_time
dev.off()



########### Análise de Regras de Associação


library(arules)
library(arulesViz)

write.csv(select(tidy_text,tweet_id,word),"word_tweet.csv")


###########3 import the data as transaction data
trans = read.transactions("word_tweet.csv", h=T, sep = ",",format = "single",cols=c("tweet_id","word"))

n_tweets

(tidy_text %>%
  distinct(tweet_id) %>%
  nrow())*0.025

53330*0.01*2

1000/(tidy_text %>%
       distinct(tweet_id) %>%
       nrow())


### usei o padrão referente a 1000 tweets, que dá pouco mais de 1.8 %
######### obtain de full rules
rules<- apriori(trans, parameter = list(supp = 0.01875117, conf = 0.8, target="rules", minlen=2,maxlen=2))
rules
summary(rules)
########## obtain measures for rules
measures<-interestMeasure(rules,
                               measure=c("coverage","fishersExactTest"),
                               transactions = trans)

############ Add the measures to the rules data
rules@quality$fisher_p<-measures$fishersExactTest



rules




### script aproveitado do artigo
rules %>%
  sort(., by = "lift") %>%           	# Note: 2
  head(., n =64 ) %>%                       	# Note: 3
  inspect(.)


head(sort(subrules, by="lift"), 30)
subrules %>%
  sort(., by = "lift") %>%           	# Note: 2
  head(., n = 60) %>%                       	# Note: 3
  inspect(.)


########### export the data of rules

write(rules,
      file = "association_rules.csv",
      sep = ",",
      quote = TRUE,
      row.names = FALSE)

######### make a plot for the rules


plot(rules, method="graph", main="Principais associações",
     alpha=1,cex=1,labelCol="#000000",precision=3, engine="igraph",arrowSize=1)



