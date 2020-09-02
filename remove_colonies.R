# script which removes all rows in a dataframe with 
# the colony names in them. 

remove_colonies <- function(df, colonynames, colname)
{
  dfcopy <- df
  
  for (each in colonynames)
  {
    print(dim(dfcopy))
    dfcopy <- dfcopy[dfcopy[,colname]!=each,]
    print(dim(dfcopy))
  }
  return(dfcopy)
}
