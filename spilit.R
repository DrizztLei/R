file_set <- read.csv("./csvInfo", head = FALSE)
file_set <- file_set[[1]]
limit <- 100

for(file_name in file_set)
{
    file_name <- as.character(file_name)
    print(file_name)

    if(1)
    {
        info <- read.csv(file_name, head = FALSE, sep = ",")

        if(length(info) == 1)
        {
            info <- read.csv(file_name, head = FALSE, sep = "\t")
        }

        if(length(info) == 1)
        {
            info <- read.csv(file_name, head = FALSE, sep = " ")
        }

        if(length(info) == 1)
        {
            print(file_name)
            print("ERROR FOR SEP NOT TELL APART")
            q()
        }

       length <- length(info)
       count <- 1

       if(1)
        {

            for(i in seq(1, length(length)))
            {
                index <- i

                for(j in seq(index , length))
                {
                    if(index == j || count > limit)
                    {
                        next
                    }

                    x <- info[, index]
                    y <- info[, j]

                    if(NA %in% x || NA %in% y)
                    {
                        next
                    }

                    if(length(x) != length(y))
                    {
                        next
                    }

                    x <- as.double(x)
                    y <- as.double(y)

                    rx <- range(x)
                    ry <- range(y)

                    max_x <- rx[2]
                    min_x <- rx[1]

                    max_y <- ry[2]
                    min_y <- ry[1]

                    if(max_x == min_x || max_y == min_y)
                    {
                        next
                    }

                    sequence <- paste("_", count, "_.csv", sep = "")
                    out_name <- gsub(".csv", sequence, x = file_name, fixed = TRUE)
                    print(out_name)

                    data_frame <- data.frame(x, y)

                    write.table(data_frame,
                                file = out_name,
                                append = FALSE,
                                quote = FALSE,
                                sep = ",",
                                col.names = FALSE,
                                row.names = FALSE
                                )
                    count <- count + 1
                }
            }
        }
    }
}
