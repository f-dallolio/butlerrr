# str_pad_num
x <- c(1,21,301,401)
str_pad_num(x)

# str_format
x <- gsub("\\s","_",rownames(mtcars))
str_format(x) # default: justify - "left
str_format(x, justify = "right")
str_format(x, justify = "centre")
str_format(x, width = 6)
str_format(x, width = 6, ellipsis = "**")

# str_enum
x <- gsub("\\s","_",rownames(mtcars))[1:12]
str_enum(x)
# different separator
str_enum(x, sep = "- ")
# numbers are not padded if `pad_num` is FALSE
str_enum(x, pad_num = FALSE)
# strings not of same width if `format_str` is FALSE
str_enum(x, format_str = FALSE)



# str_embrace
x <- gsub("\\s","_",rownames(mtcars))[1:12]
str_embrace(x, left = "-")
str_embrace(x, left = "-", right = "--")
str_embrace(x, left = "-", .c = str_oxford)

str_parens(x)
str_parens(x, "round")
str_parens(x, "round", .c = str_oxford)
str_parens(x, "squared")
str_parens(x, "curly")
str_parens(x, "angle")

# str_oxford
str_oxford(rownames(mtcars)[1:4])
str_oxford(rownames(mtcars)[1:4], or = TRUE)


#
