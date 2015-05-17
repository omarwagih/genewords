args <- commandArgs(trailingOnly = TRUE)
# args = c('~/Development/genewords/', 'hello world')
setwd(file.path(args[1], 'public', 'r'))
args <- args[-1]

suppressPackageStartupMessages({
  library(RSQLite)
  library(sqldf)
  library(stringi)
})

slidingKmers <- function(txt, k){
  iter = 1:(nchar(txt)-k+1)
  s = iter 
  e = iter+k-1
  data.frame(start=s, end=e, len=e-s+1, kmer=substring(txt, s,e), stringsAsFactors = F)
}

slidingKmersAllPos <- function(wordf, min.char=2){
  wordf = tolower(wordf)
  if(nchar(wordf) < min.char) return(data.frame(NULL))
  t = lapply(min.char:nchar(wordf), function(i) slidingKmers(wordf, i))
  do.call(rbind, t)
}

GeneWord2 <- function(kmers, s=0, e=0, data=list()){
  #print(s)
  dat = kmers
  
  # Pick only ones we need
  dat = dat[(dat$start > e & dat$end > e) | (dat$start < s & dat$end < s),]
  
  # End condition
  if(nrow(dat) == 0) return(do.call(rbind, data))
  
  # Pick max size
  dat = dat[dat$len == max(dat$len),]
  # Pick earliest kmer
  dat = dat[which.min(dat$start),]
  
  z = dat  
  data = append(data, list(z))
  #writeLines(sprintf('new_start=%s', dat$end+1))
  
  #writeLines('-----')
  s = ifelse(s==0, dat$start, pmin(s, dat$start))
  e = ifelse(e==0, dat$end, pmax(e, dat$end))
  GeneWord2(kmers, s=s, e=e, data)
}


sentence = paste(args, collapse=' ')

#sentence = 'favourite'
sentence = tolower(sentence)
substr(sentence, 1,1) = toupper(substr(sentence, 1,1) )
txt = gsub('[^\\w\\s]', '', sentence, perl = T)
txt = strsplit(txt, '\\s+')[[1]]

kmers = lapply(txt, slidingKmersAllPos)

all_kmers = unique( unlist(lapply(kmers, function(x) x$kmer), F, F) )

# Get genes that match kmers from database
to_check = paste0( paste0("'", all_kmers, "'"), collapse=',')
sql_cmd = sprintf("SELECT * FROM Uniprot WHERE word IN (%s)", to_check)
up <- sqldf(sql_cmd, dbname = "uniprot.sqlite")

# Split by word contained in gene
up_split = split(up, up$word)
# Pick minimal junk, if more than one pick randomly
up_split = lapply(up_split, function(z){
  z = z[z$junk == min(z$junk),]
  random_row = sample(nrow(z), 1)
  z[random_row,]
})
# Keep kmers that have a gene
kmers = lapply(kmers, function(z) z[z$kmer %in% names(up_split),])

final = sapply(1:length(kmers), function(i){
  
  .word = word = txt[[i]]
  kmer_data = kmers[[i]]
  if(nrow(kmer_data) > 0){
    gene_word = GeneWord2(kmer_data)
    gene_word = gene_word[order(gene_word$start),]
    offset = 0 
    
    for(j in 1:nrow(gene_word)){
      z = gene_word[j,]
      m = up_split[[z$kmer]]
      tag = sprintf('<span class="tooltip-item" word="%s" acc="%s" ent="%s" prot="%s" gene="%s" org="%s">%s</span>', 
                    m$word, m$acc, m$entry_id, m$prot, m$gene, m$org, m$word)
      stri_sub(word, z$start + offset, z$end + offset) = tag
      offset = nchar(word) - nchar(.word) # How many extra characters did we add
    }
  }
  word
})

ret = paste0(paste(final, collapse=' '), '.')
writeLines(ret)