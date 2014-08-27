pdf(file = "Exploratory_Data_Analysis.pdf", width = 11.69, height = 8.27)
IndiviPlot(filtered_pid[7], newdf)
dev.off()

pdf(file = "Plot_after_adjustment.pdf", width = 11.69,height = 8.27)
Indivi_Partition_Matrix(filtered_pid[9], newdf)
dev.off()


pdf(file = "Plot_with_nlme_function_superimposed.pdf", width = 11.69,height = 8.27)
drawCombineALL()
dev.off()

pdf(file = "Plot_For_individual_observation.pdf" , width = 11.69, height = 8.27)
for(i in 1:29){
	drawCombineIndivi(i)
}
dev.off()