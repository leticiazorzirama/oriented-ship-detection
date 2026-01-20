# # install bibliometrix package and dependencies
# install.packages("bibliometrix", dependencies=TRUE)

# load bibliometrix package
library(bibliometrix)

# import and convert bibliographic export files
## zotero (zot)
zot <- convert2df("zotero.bib", dbsource = "generic", format = "bibtex")

## web of science (wos)
wos <- convert2df("wos2026.txt", dbsource = "wos", format = "plaintext")

## scopus
sco <- convert2df("scopus2026.csv", dbsource = "scopus", format = "csv")

# track sources
zot$source <- "zotero"
wos$source <- "wos"
sco$source <- "scopus"

# # merge bibliographic data frames from supported bibliogtraphic DBs
full <- mergeDbSources(zot, wos, sco, remove.duplicated = FALSE, verbose = TRUE)

# # prepare for remove duplicated
## set priorities
priority <- c("wos", "scopus", "zotero")
full$source_priority <- match(full$source, priority)

## normalization
full$DI <- tolower(full$DI)
full$DI <- gsub("https?://(dx\\.)?doi.org/", "", full$DI)
#full$TI <- tolower(trimws(full$TI))

## remove duplicated by DOI
# order by DOI and by priority
full <- full[order(full$DI, full$source_priority), ]
# save non-empty duplicateds
dup_doi <- duplicated(full$DI) & !is.na(full$DI)
# save removed records in a report 
report_doi <- full[dup_doi, c("TI", "DI", "source")]
# use dup_doi as a filter to remove records from the full bibliographic data frame
# create new bibligraphic data frame full_doi
full_doi <- full[!dup_doi, ]

# sanity check before full_biblio
nrow(full)        # before remove duplicated
nrow(full_doi)    # after remove by DOI

# save full_biblio
full_biblio <- full_doi

# # export final bibliographic data frame
write.csv(full_biblio, "full_biblio.csv", row.names = FALSE, fileEncoding = "UTF-8")

# analyze full bibliographic data frame
ana_full <- biblioAnalysis(full)

# top 20
## plot and summarize
plot(ana_full_20, k=20)
ana_full_20_sum <- summary(ana_full, k=20)

## export full bibliographic summary top 20 to .txt file
## divert output to "ana_full_sum.txt"
sink("ana_full_20_sum.txt") 
## print the list (this output goes to the file, not the console)
print(ana_full_20_sum) 
## stop the diversion and close the file connection
sink() 

# top 50?
## plot and summarize
# plot(ana_full_20, k=20)
# ana_full_20_sum <- summary(ana_full, k=20)

# ## export full bibliographic summary top 50 to .txt file
# ## divert output to "ana_full_sum.txt"
# sink("ana_full_20_sum.txt") 
# ## print the list (this output goes to the file, not the console)
# print(ana_full_20_sum) 
# ## stop the diversion and close the file connection
# sink() 

# slice full bibliographic data frame to keep only field tags of interest
## check metadata before slicing it
colnames(full)

## complete list of field tags: https://www.bibliometrix.org/documents/Field_Tags_bibliometrix.pdf
sub <- full[,c("DI", "PY", "AU", "TI", "SO", "AB", "DT", "CT", "DE", "TC", "SC")]

# export sub bibliographic data frame
write.csv(sub, "sub_biblio.csv", row.names = FALSE, fileEncoding = "UTF-8")

