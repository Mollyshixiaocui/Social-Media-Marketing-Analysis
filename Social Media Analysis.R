library(plyr)
library(Rfacebook)
library(httr)
library(tm)
library(SnowballC)
library(reshape)
library(ggplot2)
library(gplots)
library(HH)
# setwd("~/Winter Quarter/Digital MKT")
# #the temporary token you get on https://developers.facebook.com/tools/explorer/
# token="CAACEdEose0cBABMeXhZAXoEazJ7O6h3TqGhsTS7B4ai5jJxuyTLZBGnwSE8hE2klPDBKtC2gZC9ekqBZAwolJJwlUZBx66dQJUGWZCD9pmbhpJ5GTF1SiByA4Rz9Ny6X4gcLZBMxfoZB74sTN3Bba9zriEqoP7r0RUNrP5MCaFg6sm1tTCoOZB5rGLq44uRT2E4r7EQQGZCqxkib7W4M9C2DNG"
# maty.facebook=getPage(page ="matyshp",token = token,n = 300)
# zarbee.facebook=getPage(page ="zarbees",token = token,n=500)
# convert time to POSIXIt class
# maty.facebook$time=strptime(maty.facebook$created_time,format = "%Y-%m-%dT%H:%M:%S%z")
# zarbee.facebook$time=strptime(zarbee.facebook$created_time,format = "%Y-%m-%dT%H:%M:%S%z")
# maty.facebook$weekdy=weekdays(maty.facebook$time)
# zarbee.facebook$weekday=weekdays(zarbee.facebook$time)
# zarbee.facebook$weekday = as.factor(zarbee.facebook$weekday)
# maty.facebook$type=as.factor(maty.facebook$type)
# zarbee.facebook$type=as.factor(zarbee.facebook$type)
# write.csv(maty.facebook,"MatyFacebook.csv")
# write.csv(zarbee.facebook,"ZarbeeFacebook.csv")

zarbee.facebook=read.csv("ZarbeeFacebook.csv")
maty.facebook=read.csv("MatyFacebook.csv")
mean(zarbee.facebook$likes_count)
colMeans(zarbee.facebook[,c(9,10,11)])
colMeans(maty.facebook[,c(9,10,11)])
#histagram of likes, comments and shares
library(reshape)
like=cbind(maty.facebook$likes_count,zarbee.facebook$likes_count)
likemelt=melt(like)[,2:3]
likemelt$X2=as.factor(likemelt$X2)
levels(likemelt$X2)=c("Maty's","Zarbee's")

library(ggplot2)
ggplot(likemelt,aes(value))+geom_bar(position = "dodge",aes(fill=X2))+xlim(c(0,200))+
  labs(title="Likes count of Maty's and Zarbee's",x="like_count",y="post_count")

# text analysis
library("tm")
library("SnowballC")
#sort Zarbee's post by like counts
zarbee.facebook=zarbee.facebook[order(-zarbee.facebook$likes_count),]
maty.facebook=maty.facebook[order(-maty.facebook$likes_count),]
#check the posts that have more than 500 likes
zarbee.facebook[zarbee.facebook$likes_count>500,]
#
#define a function to return the dtm
createDTM=function(content,wordstoremove){
  c=VCorpus(VectorSource(content))
  c=tm_map(c,removeWords,stopwords("english"))
  c=tm_map(c,stemDocument)
  c=tm_map(c,content_transformer(tolower))
  c=tm_map(c,content_transformer(removeNumbers))
  c=tm_map(c,content_transformer(removePunctuation))
  c=tm_map(c,removeWords,wordstoremove)
  dtm=DocumentTermMatrix(c)
  return(dtm)
}
#define a function to get sorted word frequency
sortedFreq=function(dtm){
  freq=colSums(as.matrix(dtm))
  freq=freq[order(freq,decreasing = TRUE)]
  return(freq)
}

removedwords=c("can","will","one","get","take","use","our","want","just","way","week","day",
               "what","need","want","this","via","year","here","time","the")
zarbee.dtm=createDTM(zarbee.facebook$message,removedwords)
zarbee.freq=colSums(as.matrix(zarbee.dtm))
zarbee.freq=zarbee.freq[order(zarbee.freq,decreasing = TRUE)]
head(zarbee.freq,15)
z.prop=zarbee.freq/300
head(z.prop,15)
#top50 likes posts
zarbee.like.dtm=createDTM(zarbee.facebook$message[1:50],removedwords)
zarbee.like.freq=sortedFreq(zarbee.like.dtm)
head(zarbee.like.freq,30)
z.like.prop=zarbee.like.freq/50
head(z.like.prop,30)
#compare more like posts and average posts
word=names(z.like.prop)[1:15]
word=append(word,c("coupon"),after = length(word))
average.post=z.prop[names(z.prop)%in%word]
morelikes.post=z.like.prop[names(z.like.prop)%in%word]
z.compare=merge(average.post,morelikes.post,by="row.names",
                suffixes=c(".average",".morelikes"))
z.compare.lift=z.compare$y/z.compare$x
z.compare=z.compare[order(-z.compare.lift),]
names(z.compare)=c("word","average","morelikes")
z.compare=melt(z.compare,id.vars = "word")
z.compare$word=as.character(z.compare$word)
ggplot(z.compare,aes(word,value))+geom_bar(aes(fill=variable),position = "dodge",stat = "identity")+
  labs(title="Word Appearence Proportion of Zarbee's average posts and post with more likes",
       y="proportion of appearence")

#maty 
maty.dtm=createDTM(maty.facebook$message,removedwords)
maty.freq=sortedFreq(maty.dtm)
head(maty.freq,15)
#top 50 likes posts
maty.like.dtm=createDTM(maty.facebook$message[1:50],removedwords)
maty.like.freq=sortedFreq(maty.like.dtm)
head(maty.like.freq,15)

#corelation of likes and share
cor(zarbee.facebook$likes_count,zarbee.facebook$shares_count)# 0.6790083

#analysis of weekdays
#visualization
z.week=aggregate(likes_count ~ weekday, data = zarbee.facebook, FUN = mean)
m.week=aggregate(likes_count ~ weekday, data = maty.facebook, FUN = mean)
ggplot(z.week,aes(weekday,likes_count))+geom_bar(stat = "identity")+labs(title="Zarbee's likes_count by weekdays")
ggplot(m.week,aes(weekday,likes_count))+geom_bar(stat = "identity")+labs(title="Maty's likes_count by weekdays")

#analysis of time(hour)
zarbee.facebook$hour = as.factor(format(zarbee.facebook$time,format = "%H"))
z.hour = aggregate(likes_count ~ hour, data = zarbee.facebook, FUN = mean)
ggplot(z.hour,aes(hour,likes_count))+geom_bar(stat = "identity")+labs(title="Zarbee's likes_count by hours")
#ANOVA 
fit = aov(likes_count ~ weekday + hour +hour:weekday, subset(zarbee.facebook,likes_count < 300))
summary(fit)
#ANOVA visualization
interaction2wt(likes_count ~ weekday + hour +hour:weekday, subset(zarbee.facebook,likes_count < 300))

#analysis of frequency
zarbee.facebook$date = as.factor(format(zarbee.facebook$time,format = "%Y-%m-%d"))# basically one post per day



#twitter
library(twitteR)
setup_twitter_oauth("FCpRNwWq1JpCE3JMPqc8dZOsO",
                             "r2J9ejaOURlzNUY297JF7jpaLzlXSiNVYt1qeKgKqL2wL54KYZ",
                             "701192123849248768-pWeYzzIp6fUgu6xe62zoVx3UeT0mukE",
                             "yx3UeObLZeRKLveSlJAX4uvsKpoUp66ohAtGTFCvpeMxh")
timeline=userTimeline("matyshp",n=200,includeRts = TRUE)
class(timeline)
timeline=twListToDF(timeline)

timeline$date=as.Date(timeline$created)
timeline[timeline$date=="2016-02-17",]

#instagram API
full_url <- oauth_callback()
full_url <- gsub("(.*localhost:[0-9]{1,5}/).*", x=full_url, replacement="\1")
print(full_url)
instagram <- oauth_endpoint(
  authorize = "https://api.instagram.com/oauth/authorize",
  access = "https://api.instagram.com/oauth/access_token")
myapp <- oauth_app("maty's", "daf463a3ae514f4597d495c505bb50d8", 
                   "523399de0ed24be1ae9fd43515d6672f")
ig_oauth <- oauth2.0_token(instagram, myapp,scope="public_content",  type = "application/x-www-form-urlencoded",cache=FALSE)
tmp <- strsplit(toString(names(ig_oauth$credentials)), '"')
token <- tmp[[1]][4]
username="matyshp"
install.packages("RJSONIO")
library(RJSONIO)
install.packages("RCurl")
library(RCurl)
#
getwd()
download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="C:/Users/csx/Documents/certificate")
?download.file


user_info <- fromJSON(getURL(paste('https://api.instagram.com/v1/users/search?q=',username,
                                   '&access_token=',token,sep="")),unexpected.escape = "keep")
length(user_info$data)
length(user_info)

#instagram_RCurl
intsa.maty.full=getURL("https://www.instagram.com/matyshp/")
insta.maty.full=strsplit(intsa.maty.full,split = "\n")[[1]]
insta.maty.full=insta.maty.full[240]# vector with length 1
#insta.maty.full=insta.maty.full[grep("nodes",insta.maty.full)]
insta.maty.full=strsplit(insta.maty.full,split=",")[[1]]
insta.maty.full=insta.maty.full[31:325]# by this can find the attributes we want.
?substring
class(insta.maty.full)
str(insta.maty.full)
length(insta.maty.full)
is.vector(insta.maty.full)
insta.maty.date=insta.maty.full[grep("")]
  