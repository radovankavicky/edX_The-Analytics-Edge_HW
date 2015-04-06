#Problem 1
Songs = read.csv("songs.csv")
str(Songs)
summary(Songs)
Year2010Songs=subset(Songs,year == "2010")
str(Year2010Songs)
MichaelJacksonSongs=subset(Songs,artistname == "Michael Jackson")
str(MichaelJacksonSongs)
MichaelJacksonSongsTop10=subset(MichaelJacksonSongs,Top10 =="1")
table(Songs$timesignature)
Songs[which.max(Songs$tempo),]

#Problem 2
SongsTrain = subset(Songs, year <= "2009")
SongsTest = subset(Songs, year == "2010")
str(SongsTrain)
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]
SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)
summary(SongsLog1)

#Problem 3
cor(SongsTrain$loudness,SongsTrain$energy)
SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
summary(SongsLog2)
SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(SongsLog3)

#Problem 4
SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
predictTest = predict(SongsLog3, type="response",newdata=SongsTest)
table(SongsTest$Top10, predictTest > 0.45)

