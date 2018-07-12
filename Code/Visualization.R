emoji_df = read.csv('/Users/hsnu130427/Twitter/Code/emoji_df.csv',header = TRUE)
tsne_T = read.csv('/Users/hsnu130427/Twitter/Code/tsne_T.csv',header = TRUE)
new_trans_df = read.csv('/Users/hsnu130427/Twitter/Code/new_trans_df.csv',header = TRUE)

View(tsne_T)
View(new_trans_df)

name <- sapply(colnames(emoji_df), FUN = substring, first=2)
name[1] <- 'word'
colnames(emoji_df) <- name

#combine '1f' with emoji's number in tsne_T & new_trans_df
emoji_vec = c(sapply('1f', FUN = paste0, new_trans_df[,1]))
tsne_T[,1] = emoji_vec 
new_trans_df[,1] = emoji_vec

#plot emoji with ggplot2
library(emoGG)

ggplot(data = tsne_T, aes(x=dim1,y=dim2)) +
  xlim(-15,15) + ylim(-15,15) +
  geom_emoji(emoji='1f39e') 
  

'''
?????????geom_emoji?????????aes???????????????????????????
'''

#use ggimage for aes in geom_emoji
#ref: https://guangchuangyu.github.io/cn/2017/04/ggimage/
library('Rcpp')
library('ggimage')
library('ggplot2')

'''
tsne
'''

del_tsne = tsne_T[tsne_T$X %in% c('1f39e','1f3cb','1f3d5','1f3dd','1f3df',
                              '1f3fb','1f3fc','1f3fd') == FALSE,]

del_tsne = tsne_T[sapply(tsne_T[,1], FUN = startsWith, '1f3') == FALSE,]
del_tsne = del_tsne[sapply(del_tsne[,1], FUN = startsWith, '1f4') == FALSE,]
del_tsne = del_tsne[sapply(del_tsne[,1], FUN = startsWith, '1f5') == FALSE,]
del_tsne = del_tsne[sapply(del_tsne[,1], FUN = startsWith, '1f64') == FALSE,]
del_tsne = del_tsne[sapply(del_tsne[,1], FUN = startsWith, '1f9') == FALSE,]

del_pca = new_trans_df[new_trans_df$X %in% del_tsne$X,]
#all
ggplot(data =del_tsne, aes(x=dim1, y=dim2)) + 
  ggimage::geom_emoji(aes(image = del_tsne$X)) 

#
ggplot(data =del_tsne, aes(x=dim1, y=dim2)) + 
  ggimage::geom_emoji(aes(image = del_tsne$X)) +
  xlim(-10,10) + ylim(-15,10)

'''
PCA
'''

del_pca = new_trans_df[new_trans_df$X %in% del_tsne$X,]
#all
ggplot(data =del_pca, aes(x=dim1, y=dim2)) + 
  ggimage::geom_emoji(aes(image = del_pca$X)) 

ggplot(data = del_pca, aes(x=dim1, y=dim2)) + 
  ggimage::geom_emoji(aes(image = del_pca$X)) +
  xlim(-2.5,2.5) + ylim(-2.5,2.5)

#doesn't work
mtcars$am[mtcars$am==1] <- "1f697"
mtcars$am[mtcars$am==0] <- "1f68c"
ggplot(mtcars, aes(wt, mpg, emoji=am)) + ..
  geom_emoji()

ggplot(tsne_T, aes(dim1, dim2, emoji=X)) + 
  geom_point() + 
  geom_emoji()

#plot based on words
library(Rtsne)
r_tsne = Rtsne(unique(emoji_df[,-1]))
r_tsne[,'word'] = emoji_df[,1]
ggplot(tsne_T, aes(dim1, dim2, label=word)) + 
  geom_point() +
  geom_text(aes(label=word),hjust=0,vjust=0)

