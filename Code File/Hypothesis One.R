library(MASS)
library(corrplot)
library(xtable)
library(knitr)

songs <- read.csv("songsv2.csv")
songs <- songs[-1]



# Introduction hypotheses latex 
# This will not run in R, only in Latex or RMarkdown
\documentclass{article}
\begin{document}

\textbf{Hypothesis One}
$$ H_o =\text{Danceability is not significantly related to any of the audio statistics in our data} $$
$$ H_a =\text{Danceability is significantly related to at least one of the audio statistics in our data} $$ 
  
\textbf{Hypothesis Two}
$$ H_o =\text{Average danceability is not affected by explicitness} $$
$$ H_a =\text{The average danceability does differ between explicit and clean songs} $$
  
\textbf{Hypothesis Three}
$$ H_o =\text{Average danceability is not affected by song genre} $$
$$ H_a =\text{The average danceability does differ between song genres} $$

\end{document}


# Hypothesis one EDA
# Get numerically coded predictors
songs_sub = songs[, c(3, 6:17)]
colnames(songs_sub) = c("Duration", "Popularity", "Danceability", "Energy", "Key",
                        "Loudness", "Mode", "Speechiness", "Acousticness", "Instrumentalness", 
                        "Liveness", "Valence", "Tempo")
# Create correlation matrix
cor_songs = cor(songs_sub)
# Plot correlation
corrplot(cor_songs, tl.col="black", tl.cex=0.8, tl.srt=70)




# Hypothesis one Analysis
# Credit to Sebastian Rivera-Chepetla for this code
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
# Get correlations
res2 =rcorr(as.matrix(songs_sub[-c(5,7)]))
matrix_df = flattenCorrMatrix(res2$r, res2$P)
#Isolate significant correlations
m_mod = matrix_df[matrix_df$p<0.05,]
r = m_mod$row=="Danceability"
# M_Mod contains predictors significantly associated with danceability
m_mod[r,]
xtable(m_mod[r,])






