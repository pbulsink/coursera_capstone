#Read in data
library(R.utils)
contwitter<-file("./data/en_US/en_US.twitter.txt")
conblogs<-file("./data/en_US/en_US.blogs.txt")
connews<-file("./data/en_US/en_US.news.txt")

file_twitter<-readLines(contwitter)
file_news<-readLines(connews)
file_blog<-readLines(conblogs)

close(contwitter)
close(connews)
close(conblogs)

rm(contwitter)
rm(connews)
rm(conblogs)
