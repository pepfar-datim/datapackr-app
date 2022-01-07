sendDataPackToS3 <- function(d, datapath) {

  #Write an archived copy of the file
  s3 <- paws::s3()
  object_tags <- createS3BucketTags(d)

  object_name <- paste0("datapack_archives/",
                        gsub(" ", "_", d$info$sane_name), "_", format(Sys.time(), "%Y%m%d_%H%m%s"),
                        ".xlsx")

  # Load the file as a raw binary
  read_file <- file(datapath, "rb")
  raw_file <- readBin(read_file, "raw", n = file.size(datapath))
  close(read_file)

  r <- tryCatch({
    foo <- s3$put_object(Bucket = Sys.getenv("AWS_S3_BUCKET"),
                         Body = raw_file,
                         Key = object_name,
                         Tagging = object_tags,
                         ContentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
    flog.info("Datapack Archive sent to S3", name = "datapack")
    TRUE
  },
  error = function(err) {
    flog.info("Datapack could not be archived", name = "datapack")
    flog.info(err, name = "datapack")
    FALSE
  })

  return(r)

}
