library(tidyverse)
library(fs)

s = "Hello world!"
sa = c("hello", "world", "!")

# string lenght
str_length(s)
str_length(sa)

# string concatenation
str_c("Hello", "world!", sep=" ")
# vectorized
str_c(c("1", "2", "3"), c("a", "b", "c"))
str_c("prefix-", c("a", "b", "c"), "-suffix")

# REGULAR EXPR
# .: matches any character.
# \d: matches any digit.
# \s: matches any whitespace (e.g. space, tab, newline).
# [abc]: matches a, b, or c.
# [^abc]: matches anything except a, b, or c.
# ^ matches the start of the string.
# $ matches the end of the string.

# string view all
Gregory <- "To move is to stir, and to be valiant is to stand: therefore, if thou art moved, tho"
str_view_all(Gregory, "move")
str_view_all(Gregory, "[aeiou]\\s") #any character in a set
str_view_all(Gregory, "[^aeiou]\\s") #any character not in a set

str_view(Gregory, "^..") #start of the string
str_view(Gregory, "..$") #end of the string

# |: unioun
# ?: 0 or 1
# +: 1 or more
# *: 0 or more

CCCP <- c("P", "CP", "CCP", "CCCP")
str_view(CCCP, "CC?")
str_view(CCCP, "CC+")
str_view(CCCP, "CC*")

#Tools
# detect - which strings match a pattern
# count - matches
# extract - the content of matches
# replace - matches with new values
# split - a string based on a match

str_detect("apple", "p")
str_detect(c("apple", "banana", "pear"), "e")

str_count("apple", "p")
str_count(c("apple", "banana", "pear"), "a")

str_extract(Gregory, "mov.?")
str_extract_all(Gregory, "mov.?")

str_replace("apple", "[aeiou]", "-")
str_replace_all("apple", "[aeiou]", "-")

str_split("banana", "n")
str_split(Gregory, "\\s")
