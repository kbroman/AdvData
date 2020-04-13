#### create dataset for homework 4
# NHANES data with variables for predicting diabetes

# installed RNHANES from github
### devtools::install_github("SilentSpringInstitute/RNHANES")
library(RNHANES)
library(data.table) # used for fread()

# cycles to use
cycles <- c("1999-2000", "2001-2002", "2003-2004")

# create cache directory if necessary
if(!dir.exists("_cache")) dir.create("_cache")

# download variables
var_file <- "_cache/variables.rds"
if(file.exists(var_file)) {
    variables <- readRDS(var_file)
} else {
    variables <- nhanes_variables()
    saveRDS(variables, var_file)
}
# demographics, dietary, examination, laboratory, questionaire
# age, waist, blood relatives, height, cholesterol, leg, weight, bmi, race hbp, income, alc, smoke, edu, exer, gend

# load training and test data from github.com/guhjy/NHANES-diabetes
url <- "https://raw.githubusercontent.com/guhjy/NHANES-diabetes/master/data/"
files <- c("diabetes_data_train.csv", "diabetes_data_test.csv")
temp_dat <- vector("list", length(files))
for(i in seq_along(files)) {
    local_file <- file.path("_cache", files[i])
    if(!file.exists(local_file)) {
        download.file(paste0(url, files[i]), local_file)
    }
    temp_dat[[i]] <- data.table::fread(local_file)
}
orig <- do.call("rbind", temp_dat)


# variables used
orig_var <- colnames(orig) # "status" (last column) is the outcome

# also need these variables for diabetes trait + exclude pregnant subjects
# DIQ010 = diabetes (via doctor)
# LB2GLU =
# LBXGLUSI plasma glucose in SI (mg/dL / 18)
# RHD143 = currently pregnant
# SEQ060 = currently pregnant
# RHQ141 = currently pregnant
# RHD143 = currently pregnant

# look for rows with these variables
var_info <- vector("list", ncol(orig)-1)
names(var_info) <- orig_var[-ncol(orig)]
for(i in seq_along(var_info)) {
    var_info[[i]] <-
        variables[variables$variable_name == names(var_info)[i],
                  c("variable_name", "variable_description", "data_file_name", "cycle")]
    var_info[[i]] <-
        var_info[[i]][var_info[[i]]$cycle %in% cycles,]
}
var_info <- lapply(var_info, function(a) a[1:3,][order(a$cycle[1:3]),])

# add in the others
other_var <- c("DIQ010", "LBXGLUSI", "LBDGLUSI", "RHD143", "SEQ060", "RHQ141")

other_var_info <- vector("list", length(other_var))
names(other_var_info) <- other_var
for(i in seq_along(other_var)) {
    other_var_info[[i]] <-
        variables[variables$variable_name == other_var[i],
                  c("variable_name", "variable_description", "data_file_name", "cycle")]
    other_var_info[[i]] <-
        other_var_info[[i]][other_var_info[[i]]$cycle %in% cycles,]
}
other_var_info[[2]] <- rbind(other_var_info[[2]], other_var_info[[3]])
other_var_info[[4]] <- rbind(other_var_info[[4]], other_var_info[[5]], other_var_info[[6]])
other_var_info <- other_var_info[c(1,2,4)]
other_var_info <- lapply(other_var_info, function(a) a[1:3,][order(a$cycle[1:3]),])

files <- sapply(var_info, function(a) a[a$cycle=="1999-2000", "data_file_name"][1])
ufiles <- unique(files)

# "Lab13" for 1999-2000 but l13 for 2001-2002 and 2003-2004
# "SEQ" for 1999-2000 but RHQ for 2001-2002, 2003-2004
# "LAB10AM" for 1999-2000 (LBXGLUSI), "L10AM_B" for 2001-2002, "L10AM_C" for 2003-2004

ufiles <- c(ufiles[ufiles != "Lab13"], "DIQ")
ufiles_1999 <- c("Lab13", "SEQ", "LAB10AM")
ufiles_2001 <- c("l13", "RHQ", "L10AM")


# download data
raw_data <- "_cache/raw_data_as_list.rds"
if(file.exists(raw_data)) {
    dat <- readRDS(raw_data)
} else {
    dat <- setNames(lapply(ufiles, nhanes_load_data, cycles, destination="_cache"), ufiles)
    dat_a <- setNames(lapply(ufiles_1999, nhanes_load_data, cycles[1], destination="_cache"), ufiles_1999)
    dat_bc <- setNames(lapply(ufiles_2001, nhanes_load_data, cycles[-1], destination="_cache"), ufiles_2001)

    for(i in seq_along(dat_a)) {
        dat_bc[[i]] <- c(list(dat_a[[i]]), dat_bc[[i]])
    }
    names(dat_bc) <- names(dat_a)
    dat <- c(dat, dat_bc)
    saveRDS(dat, raw_data)
}


# create data dictionary
## variable_name
## variable_description
## name_1999
## file_1999
## name_2001
## file_2001
## name_2003
## file_2003

all_var_info <- c(var_info, other_var_info)

# strip starting and ending white space
clean_begend <- function(str) sub("\\s+$", "", sub("^\\s+", "", str))

data_dictionary <-
    data.frame(variable_name=setNames(sapply(all_var_info, function(a) a$variable_name[1]), NULL),
               variable_description=setNames(sapply(all_var_info, function(a) clean_begend(a$variable_description[1])), NULL),
               name_1999=setNames(sapply(all_var_info, function(a) a$variable_name[1]), NULL),
               name_2001=setNames(sapply(all_var_info, function(a) a$variable_name[2]), NULL),
               name_2003=setNames(sapply(all_var_info, function(a) a$variable_name[3]), NULL),
               file_1999=setNames(sapply(all_var_info, function(a) a$data_file_name[1]), NULL),
               file_2001=setNames(sapply(all_var_info, function(a) a$data_file_name[2]), NULL),
               file_2003=setNames(sapply(all_var_info, function(a) a$data_file_name[3]), NULL),
               stringsAsFactors=FALSE)

# pull out just the key columns
for(j in 1:3) {
    varcol <- paste0("name_", c(1999, 2001, 2003)[j])
    var <- data_dictionary[,varcol]

    for(i in seq_along(dat)) {
        keep <- c("SEQN", var[data_dictionary$file_1999 == names(dat)[i]])

        dat[[i]][[j]] <- dat[[i]][[j]][, keep, drop=FALSE]
    }
}

# rbind the pieces
combined <- dat
for(i in seq_along(dat)) {
    colnames(dat[[i]][[2]]) <- colnames(dat[[i]][[3]]) <- colnames(dat[[i]][[1]])
    combined[[i]] <- do.call("rbind", dat[[i]])
}


# not missing diabetes individuals
diq <- setNames(combined$DIQ$DIQ010, combined$DIQ$SEQN)
has_diabetes_data <- names(diq)[!is.na(diq) & diq <= 2]

glu <- setNames(combined$LAB10AM$LBXGLUSI, combined$LAB10AM$SEQN)
has_glu <- names(glu)[!is.na(glu)]

keep <- unique(c(has_diabetes_data, has_glu))

# drop and pad the datasets
for(i in seq_along(combined)) {
    # drop individuals that don't have diabetes phenotype
    combined[[i]] <- combined[[i]][combined[[i]]$SEQN %in% keep,]

    # pad with the missing individuals
    to_add <- keep[!(keep %in% combined[[i]]$SEQN)]
    x <- matrix(nrow=length(to_add), ncol=ncol(combined[[i]]))
    colnames(x) <- colnames(combined[[i]])
    x <- as.data.frame(x)
    x$SEQN <- to_add
    combined[[i]] <- rbind(combined[[i]], x)

    rownames(combined[[i]]) <- combined[[i]]$SEQN
    combined[[i]] <- combined[[i]][keep,]
}

# paste them all together
for(i in seq_along(combined)[-1]) {
    combined[[1]] <- cbind(combined[[1]], combined[[i]][,-1,drop=FALSE])
}
combined <- combined[[1]]

combined <- combined[,c("SEQN", data_dictionary$variable_name)]

# drop pregnant
combined <- combined[is.na(combined$SEQ060) | combined$SEQ060 != 1,]

# drop those <20
combined <- combined[combined$RIDAGEYR >= 20,]

# alcohol > 365 -> NA
combined$ALQ120Q[!is.na(combined$ALQ120Q) & combined$ALQ120Q>365] <- NA

# BPQ020 == 9 -> NA
combined$BPQ020[!is.na(combined$BPQ020) & combined$BPQ020==9] <- NA

# MCQ250A == 7 or 9 -> NA
combined$MCQ250A[!is.na(combined$MCQ250A) & combined$MCQ250A>=7] <- NA

# age smoking  > 800 -> NA
combined$SMD030[!is.na(combined$SMD030) & combined$SMD030 > 150] <- NA

# activity >= 7 -> NA
combined$PAQ180[!is.na(combined$PAQ180) & combined$PAQ180>=7] <- NA

# education >= 7 -> NA
combined$DMDEDUC2[!is.na(combined$DMDEDUC2) & combined$DMDEDUC2>=7] <- NA

# household income >=77 -> NA
combined$INDHHINC[!is.na(combined$INDHHINC) & combined$INDHHINC>=77] <- NA


# diabetes: DIQ010 not missing and == 1
# LBXGLUSI >= 7.0

# better names for the variables
better_names <-
    c(SEQN="id",
      ALQ120Q="alcohol",
      BMXBMI="bmi",
      BMXHT="height",
      BMXLEG="upper_leg_length",
      BMXWAIST="waist",
      BMXWT="weight",
      BPQ020="high_bp",
      DMDEDUC2="education",
      INDHHINC="household_income",
      LBXTC="cholesterol",
      MCQ250A="diabetic_relatives",
      PAQ180="activity",
      RIAGENDR="gender",
      RIDAGEYR="age",
      RIDRETH1="race",
      SMD030="age_smoking",
      DIQ010="diabetic",
      LBXGLUSI="glucose",
      SEQ060="pregnant")

# add individual ID
data_dictionary <-
    rbind(data.frame(variable_name="SEQN",
                     variable_description="individual identifier",
                     name_1999="SEQN",
                     name_2001="SEQN",
                     name_2003="SEQN",
                     file_1999=NA,
                     file_2001=NA,
                     file_2003=NA,
                     stringsAsFactors=FALSE),
          data_dictionary)

# check that things are lined up
stopifnot(all(names(better_names) == colnames(combined)),
          all(names(better_names) == data_dictionary$variable_name))

# change names
colnames(combined) <- setNames(better_names, NULL)
data_dictionary$variable_name <- setNames(better_names, NULL)

# diabetic: glucose >= 7
combined$diabetic[!is.na(combined$diabetic) & combined$diabetic>2] <- NA
combined$diabetic[!is.na(combined$glucose) & combined$glucose >= 7] <- 1

# update data dictionary
data_dictionary[18,2] <- "diabetic, based on doctor diagnosis or glucose >= 126 mg/dL"
data_dictionary[18,3:8] <- NA


# drop columns
combined <- combined[,1:18]
data_dictionary <- data_dictionary[1:18,]
combined <- combined[!is.na(combined$diabetic),]

# add codes
data_dictionary <- cbind(data_dictionary,
                         codes=c(NA,NA,NA,NA,NA,NA,NA,
                                 "1=yes, 2=no",
                                 "1=less than 9th grade, 2=9-11th, 3=high school, 4=some college, 5=college grad",
                                 "1=<$5k,2=$5-10k,3=$10-15k,4=$15-20k,5=$20-25k,6=$25-35k,7=$35-45k,8=$45-55k,9=$55-65k,10=$65-75k,11=>$75k,12=>$20k,13=$<20k",
                                 NA, "1=yes, 2=no",
                                 "1=does not walk much; 2=walks a lot; 3=light work; 4=heavy work",
                                 "1=female, 2=male",
                                 NA, "1=mexican american, 2=other hispanic, 3=non-hispanic white, 4=non-hispanic black, 5=other",
                                 "0=never smoked cigarettes regularly",
                                 "1=yes, 2=no"))


combined$id <- as.numeric(combined$id)
data_dictionary <- data_dictionary[,c(1,9,2:8)]

# save to CSV files
write.table(combined, "nhanes_diabetes.csv", row.names=FALSE, col.names=TRUE,
            quote=TRUE, sep=",")

write.table(data_dictionary, "nhanes_diabetes_data_dict.csv", row.names=FALSE, col.names=TRUE,
            quote=TRUE, sep=",")
