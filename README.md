# Project: Words 4 Music

### [Project Description](doc/project4_desc.md)

![image](http://cdn.newsapi.com.au/image/v1/f7131c018870330120dbe4b73bb7695c?width=650)

Term: Fall 2016

+ [Data link](https://courseworks2.columbia.edu/courses/11849/files/folder/Project_Files?preview=763391)-(**coursework login required**)
+ [Data description](doc/readme.html)
+ Contributor's name: Jiani Tian
+ Projec title: Can we know what the song is going to say when we only listen the melody?
+ Project summary: In this project, I try to find the associations between the lyrics and the melody between a song. And when we listen a new song, we may infer the topics and lyrics of the this song (word ranks of the dictionary words).
+ Methods:I mainly tried two methods to do this.

Firstly, based on the the naive thoughts, I want to do classify songs into several categories using cluster methods based on their features. And count the word distritbution directly. When we want to know a new song's topic and their lyrics (words rank), we just need to find which cluster this song should belongs to. This method highly depend on the features we choose, have large variance when choosing different cluster methods (hclust,kmeans) and didn't explore too much on the associations between the lyrics and melody features. So I finally decided not to use this method.

Secondly, I used multinomial regression method to find the relationship between lyrics and music features. Firstly I use topic modeling method to classify the lyrics into 18 topics (cross validation to tune the parameters), and use PCA to cluster music features (divide the features mainly into two categories, statistcs features and time series features, since if we combine them directly, the statistics features will only take a small percentage). I tried general linear regression and the effect is not good enough as the dependent variable matrix have large number of 0 and will lead to the coefficient matrix vary largely. So finally I use multinomial regression and get the rank.
	
