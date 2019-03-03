= Some Title
Your Name
:toc2:
:numbered:
:data-uri:
:duration: 120

== What is this About?

=== Some simple asciidoc

* A list with a https://en.wikipedia.org/wiki/Hyperlink[link].
* Yet another entry in the list.

== Including Code
Do not use the _include_ macro provided by asciidoc!
Instead prefix all source statements with 
----
#+ code = readLines("file_to_be_sourced")
#+ include = FALSE
----
It est use
----
#+ code = readLines("src/sum.R")
#+ include = FALSE
source("src/sum.R")
----
to produce


[source,r]
----
a  <- c(2, 3, 4, 10) # <1>
value <- 0 # <2>
for (a_i in a) { # <3>
    value <- value + a_i  # <4>
}
print(value) # <5>
----

----
## [1] 19
----





== A new section



[source,r]
----
my_sum <- function(x) {
    value <- 0
    for (x_i in x) {
        value <- value + x_i 
    }
    return(value)
}
----


=== A subsection


[source,r]
----
print(value)
----

----
## [1] 19
----





[source,r]
----
print(my_sum(1:3))
----

----
## [1] 6
----



Inline code does not work: Object +value+ has value +r value+.

