

[source,r]
----
# Begin Exclude Linting
----


= Some Title
Your Name
:toc2:
:numbered:
:data-uri:
:duration: 120

== What is this About?

//begin_no_slide
This will not show up on slides.
//end_no_slide

=== Some simple asciidoc

* A list with a https://en.wikipedia.org/wiki/Hyperlink[link].
* Yet another entry in the list.

== Including Code
Do not use the _include_ macro provided by asciidoc!+ 


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


== Only sections give new slides //slide_only
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



Some inline code: Object +value+ has value +r value+.

== Next slide //slide_only

=== Second subsection


[source,r]
----
# End Exclude Linting
----


