

tr.. <- read.csv(file="EdinTrcts/TractRelationshipFile.csv")

tr.. <- tr..[!is.na(tr..$STATE00),]

e.. <- read.csv(file="EdinTrcts/Ed15_Tract.csv")

e00.. <- read.csv(file="EdinTrcts/Ed00_Tract.csv")

#Correct Names
e..$GEOID10 <- e..$GEOID00
e..$GEOID00 <- NULL

e00..$p_BA25over <- as.numeric(as.character(e00..$p_BA25over))
tr.. <- merge(tr.., e00..[, c("GEOID00", "p_BA25over")], by.x="GEOID00", by.y="GEOID00", all.x=TRUE)

e..$p_BA25_00 <- NA

tr.. <- na.omit(tr..)

for( i in seq(1:nrow(e..))){
  
  s.. <- tr..[ tr..$GEOID10==e..$GEOID10[i], ]
   
  p_BA25_00 <- sum( (s..$p_BA25over * s..$POPPCT10) )/ sum( s..$POPPCT10)
  
  e..$p_BA25_00[i] <- p_BA25_00
  print(p_BA25_00)
  rm(p_BA25_00, s..)
  print(round(i/nrow(e..),2)*100)
}
e..$p_BA25over <- as.numeric(as.character(e..$p_BA25over ))
e..$PCTpointCHG <- e..$p_BA25over - (e..$p_BA25_00*100)


write.csv(e.., file="EdinTrcts/Ed_Change2010shapes.csv")

r.. <- read.csv(file="EdinTrcts/Ed_Change2010shapes.csv")

#Alameda 43% Educated vERSUS 35%
#SF 54% versus  45%
# Santa Clara 40.5% versus 48%
#LA versus 25% versus  31% Educated

range(e..$PCTpointCHG, na.rm=T)

STuffffff <- tapply(tr..$POP10, tr..$GEOID10, mean, na.rm=T)

e..$POPWEIGHT <- STuffffff[paste(e..$GEOID10)]

e..$POPWEIGHT

sum( (e..$PCTpointCHG * e..$POPWEIGHT), na.rm=TRUE) / sum(e..$POPWEIGHT, na.rm=TRUE)

