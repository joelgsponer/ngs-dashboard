##########
#QC Panel#
##########
output$googleVisTest <-renderGvis({
df=data.frame(date=seq(1,100), 
              "QC"=runif(100,0,100)
              )
              
Bar <- gvisAreaChart(df
  ,options=list(
    series="[
       {color:'green'}
    ]"
    ,curveType="function"
    ,gvis.editor="Edit me!"
    ,explorer="{ actions: ['dragToZoom', 'rightClickToReset'] }"
  )
)
})
