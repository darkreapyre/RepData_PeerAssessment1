## Option 1
df[is.na(df$traits), "traits"] <- tapply(df$traits, df$group, mean,  
                                         na.rm=TRUE)[ df[is.na(df$traits),"group"] ]

## Option 2
f=function(x){
        x<-as.numeric(as.character(x)) #first convert each column into numeric if it is from factor
        x[is.na(x)] =median(x, na.rm=TRUE) #convert the item with NA to median value from the column
        x #display the column
}
ss=data.frame(apply(df,2,f))

## option 3
new <- lapply( dfrm, function(x) x[x=="Hi"] <- median(as.numeric(as.character(x)), na.rm=TRUE) )

#If they need to be numeric you can do this afterwards:
        
newnum <- lapply(new, function(x) as.numeric(as.character(x))
                         
#It's possible you may need to also use as.data.frame to get then back to the original class.
