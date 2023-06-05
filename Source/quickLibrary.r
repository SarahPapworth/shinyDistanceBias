files = list.files  ( file.path (getwd(), "R") )
for  ( i in 1:length (files)){
    source( file.path (getwd(), "R", files[i]))
}
