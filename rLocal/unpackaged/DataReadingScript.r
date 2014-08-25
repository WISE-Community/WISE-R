############## DIRECTORIES #####################################
# You will need to set the following paths to the directories on your home computer
# Windows uses a double backslash \\ between directories
# Mac OS uses a single forward slash between / directories 
options(java.parameters = "-Xmx4096m"); #increase Java's memory, necessary for large workbooks (many tabs)
library(xlsx)

# Address of folder containing All Student Work excel files
dir.excel <- "C:\\Users\\FILL"
# Before running create a directory for outputs from this analysis (e.g. prepost scores)
dir.out <- "C:\\Users\\FILL"
# Before running create a directory for scored work that will be inputted back into this data
dir.scored <- "C:\\Users\\FILL"
# May need special export, completed following as needed
dir.table <- ""
dir.sensor <- ""
dir.mysystem <- ""
dir.grapher <- ""
dir.cargraph <- ""
dir.box2d <- ""

wise <- read.xlsx.wiseDir(excelDirectory);

# set the parent project ids, 
Parent.Project.Id.pretest = c(FILL)
Parent.Project.Id.posttest = c(FILL)
Parent.Project.Id.unit = c(FILL)

# Create step Id, In some cases may need to use Activity Numbers if Pretest/Posttest are combined into same unit
wise <- assignStepId(wise, "Unit", Parent.Project.Id = Parent.Project.Id.unit)
wise <- assignStepId(wise, "Pretest", Parent.Project.Id = Parent.Project.Id.pretest)
#wise = assignStepId(wise, "Pretest", Parent.Project.Id = Parent.Project.Id.pretest, Activity.Nums = 1)
#wise = assignStepId(wise, "Posttest", Parent.Project.Id = Parent.Project.Id.posttest, Activity.Nums = 2)
wise <- assignStepId(wise, "Posttest", Parent.Project.Id = Parent.Project.Id.posttest)


# There are two primary methods of finding a condition, one is by workgroup id, one is by parent project id
if (TRUE){
	# Workgroup ID
	wise$Condition <- applyCondition(wise, method="Workgroup.Id.mod", Parent.Project.Ids=Parent.Project.Id.unit, Workgroup.Id.mod=2, as.data.frame.out=FALSE)
} else {
	# Parent Project Id, should be more than one, use Parent.Project.Conditions to specify condition for each Parent.Project.Ids in same order
	wise$Condition <- applyCondition(wise, method="Parent.Project.Id", Parent.Project.Ids=Parent.Project.Id.unit, Parent.Project.Conditions=c(1,2), as.data.frame.out=FALSE)
}

# Import work from special export
if (nchar(dir.box2d)>0){
	box2d <- read.xlsx.special(dir.box2d);
	wise <- transferColValues(wise, box2d, targetColName="Student.Work.Part.1", sourceColName="Student.Work");
} 
if (nchar(dir.table)>0){
	table <- read.xlsx.special(dir.table);
	wise <- transferColValues(wise, table, targetColName="Student.Work.Part.1", sourceColName="Student.Work");
} 
if (nchar(dir.sensor)>0){
	sensor <- read.xlsx.special(dir.sensor);
	wise <- transferColValues(wise, sensor, targetColName="Student.Work.Part.1", sourceColName="Student.Work");
} 
if (nchar(dir.grapher)>0){
	grapher <- read.xlsx.special(dir.grapher);
	wise <- transferColValues(wise, grapher, targetColName="Student.Work.Part.1", sourceColName="Student.Work");
} 
if (nchar(dir.cargraph)>0){
	cargraph <- read.xlsx.special(dir.cargraph);
	wise <- transferColValues(wise, cargraph, targetColName="Student.Work.Part.1", sourceColName="Student.Work");
} 
if (nchar(dir.mysystem)>0){
	mysystem <- read.xlsx.special(dir.mysystem);
	wise <- transferColValues(wise, mysystem, targetColName="Student.Work.Part.1", sourceColName="Student.Work");
} 
# Given New Information, reset URev and IURev (Number of unique revisions, and inversed)
wise$URev.Num = getRevisionNumber(wise,FALSE, TRUE);
wise$IURev.Num = getRevisionNumber(wise,TRUE, TRUE);

# export pretest and posttest to out directory for human scoring
prepost.out <- write.wisedata.frame.prepost(wise, dir.out = dir.out, filename="PrePost.xlsx")

# import scored responses from scored directory
wise <- readForColValues.prepost (wise, paste(dir.scored,"PrePost-scored.xlsx",sep=""), sourceColNames.except = names(prepost.out), postfixes = c("\\.Pretest","\\.Posttest"), sheetIndex=2)

# if a rubric (e.g. rubrics.standard) is available, do auto scored
if (!is.null(rubrics.standard)) wise$Research.Score = score(wise, score.rubrics=rubrics.standard ,as.data.frame.out=FALSE)

# what pretest and posttest items should be included into a total score?
total.names.pretest <- character()
if (length(which(names(wise)=="KI.Score")>0) total.names.pretest <- c(total.names.pretest, paste("KI.Score.max",unique(subset(wise, Is.Pretest==TRUE&!is.na(KI.Score))$Step.Id), sep="."))
if (length(which(names(wise)=="Research.Score")>0) total.names.pretest <- c(total.names.pretest, paste("Research.Score.max",unique(subset(wise, Is.Pretest==TRUE&!is.na(Research.Score))$Step.Id), sep="."))
total.names.posttest <- character() 
if (length(which(names(wise)=="KI.Score")>0) total.names.posttest <- c(total.names.posttest, paste("KI.Score.max",unique(subset(wise, Is.Pretest==TRUE&!is.na(KI.Score))$Step.Id), sep="."))
if (length(which(names(wise)=="Research.Score")>0) total.names.posttest <- c(total.names.posttest, paste("Research.Score.max",unique(subset(wise, Is.Pretest==TRUE&!is.na(Research.Score))$Step.Id), sep="."))

# compile data into a new "wide" data frame for regression analysis
magna <- magna.carta.holy.grail(subset(wise, Step.Id%in%unique(subset(wise,!is.na(Research.Score)|!is.na(KI.Score))$Step.Id)) , by="Wise.Id.1", step.identifier = "Step.Id", select.numerical = sapply(grep("Index|Time\\.Spent\\.Seconds|Rev\\.Num|^URev\\.Num|Research\\.Score|^KI\\.|^C\\.|.*?Score",names(wise), value=TRUE),as.name), FUNS.numerical = c("sum", "mean", "max", "first","last", "first.to.last"), total.names.pretest=total.names.pretest, total.names.posttest=total.names.posttest, include.median.splits=FALSE)
