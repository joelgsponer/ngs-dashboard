###########
# Classes #
###########

#Experiment
setClass("Experiment"
         , representation( 
	           user = "character"
	     )      
)

setMethod("show", "Experiment", function(object){
	cat("#User                   = ",object@user,"\n") 
})

