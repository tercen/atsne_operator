suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tercen))
  
do.tsne = function(data, max_iter=1000, dims=2, exaggeration_iter=250, perplexity=30, theta=0.5){
  filename = tempfile()
  out.filename = tempfile()
  
  on.exit({
    if (file.exists(filename)){
      file.remove(filename);
    }
    if (file.exists(out.filename)){
      file.remove(out.filename);
    }
  })
  
  write.filename = file(filename, "wb")
  writeBin(as.vector(data), write.filename, size=4)
  close(write.filename)
  
  cmd = 'bin/atsne_cmd'
  args = paste('--iterations', max_iter, 
                  '--target_dimensions', dims,
                  '--exaggeration_iter', exaggeration_iter, 
                  '--perplexity', perplexity,
                  '--theta', theta, 
                  filename,
                  out.filename,
                  ncol(data),
                  nrow(data),
                  sep = ' ')
   
  system2(cmd, args)
  
  read.filename = file(out.filename, "rb")
  
  tsne.data = readBin(read.filename, double(), size=4, n = 2*ncol(data))
  
  close(read.filename)
  
  tsne.matrix = matrix(tsne.data, nrow = ncol(data), ncol = 2, byrow = TRUE)
  
  colnames(tsne.matrix) = c('tsne.1', 'tsne.2')
  
  return(tsne.matrix)
}
 
(ctx = tercenCtx())  %>% 
  as.matrix(fill=as.double(ctx$op.value('fill'))) %>% 
  do.tsne(max_iter = as.integer(ctx$op.value('max_iter')),
          exaggeration_iter  = as.integer(ctx$op.value('exaggeration_iter')),
          dims = as.integer(ctx$op.value('dims')),
          perplexity = as.integer(ctx$op.value('perplexity')),
          theta = as.double(ctx$op.value('theta')))%>% 
  as.data.frame() %>% 
  mutate(.ci = seq_len(nrow(.))-1) %>%
  ctx$addNamespace() %>%
  ctx$save()


 
  
  
  
  
  
  
  
  
  
  
