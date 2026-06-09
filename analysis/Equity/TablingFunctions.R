library(flextable)
library(officer)
testPooledModel <- diversionPooledResults[["Full"]]
tpmSummary <- summary(testPooledModel)
   
createPooledModelSummary <- function(pr) {
   # get the summary
   prS <- data.table(summary(pr))
   
   # calc the 95% confidence interval
   confLevel <- 0.95
   z <- qnorm(1-(1-confLevel)/2)
   prS[,LowerEstimate:=estimate-z*std.error]
   prS[,UpperEstimate:=estimate+z*std.error]
   
   # exponentiate the coeffiecients and confints
   prS[,OddsRatio:=round(exp(estimate),digits=2)]
   prS[,CILower:=round(exp(LowerEstimate),digits=2)]
   prS[,CIUpper:=round(exp(UpperEstimate),digits=2)]
   
   prS[,.(term,OddsRatio,CILower,CIUpper,round(p.value,digits=4))]
}

createPooledModelSummaryDoc <- function(pr,experimentName,experimentVariant,prName) {
   prSummary <- createPooledModelSummary(pr)
   ft <- flextable::qflextable(prSummary)
   fn <- paste0("doc/",experimentName,"_",experimentVariant,"_Pooled_Summary_",prName,".docx")
   
   #tab_model(pr,file=fn)
   
   # Create a Word document and add the table
   doc <- officer::read_docx() %>%
      officer::body_add_par(paste("Experiment: ", experimentName), style = "heading 1") %>%
      officer::body_add_par("") %>%
      body_add_flextable(ft)
   
   # Save the document
   print(doc, target = fn)
}

#print(createPooledModelSummary(testPooledModel))
#createPooledModelSummaryDoc(testPooledModel,"Test","Test","Test")
#shell.exec(paste0(getwd(),"/doc/Test_Test_Pooled_Summary_Test.docx"))