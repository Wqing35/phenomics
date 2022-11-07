library(RISmed)

res <- EUtilsSummary("Phenomics", type="esearch", db="pubmed", mindate=2001, maxdate=2002,datetype = 'pdat')
summary(res)
QueryId(res)


a <- EUtilsGet(res)

Affiliation(a)[[2]]
Author(a)[[2]]
ArticleTitle(a)[[2]]













