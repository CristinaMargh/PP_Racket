# Racket: Suffix trees
We will represent a suffix tree as a list of branches, where each branch corresponds to a child of the root.
By branch, we mean a pair between a label and the subtree rooted at the node below the label.
To use functions that work on lists, each tag will be represented not as a string, but as a list of characters.
Main functions used:
* first-branch takes a suffix tree (ST) and returns its first branch (a label-subtree pair)
* other-branches takes a ST and returns the ST without its first branch (a list of branches, as the original ST was)
* get-ch-branch receives an ST and a character ch and returns that branch of the ST whose label begins with the character ch, or false if no such branch exists
* get-branch-label gets a branch of an ST and returns its label
* get-branch-subtree gets a branch of an ST and returns the subtree under its label
* longest-common-prefix-of-list takes a non-empty list of words starting with the same character and returns the longest common prefix of all words in the list
* match-pattern-with-label is used to search for a pattern (a substring) in a text, using the ST associated with the text
* st-has-pattern? receives an ST and a template and returns true if the template appears in the ST, false if it does not
* get-suffixes takes a text (a list of characters with the $ special character added to the end) and returns all the suffixes of the text (in descending order of length)
* ast-func and cst-func represent functions to label a ST
* suffixes->st takes a label function (like ast-func or cst-func), a list of suffixes (all suffixes of a text) and an alphabet (the alphabet used by the text) and returns:
 The AST associated with the text, if we call suffixes->st with the tagging function ast-func
 The CST associated with the text, if we call suffixes->st with the labeling function cst-func
* longest-common-substring takes two texts text1 and text2 and calculates their longest common substring, like this:
construct the suffix tree ST1 for text1
determines all S-suffixes of the text text2 (without the trailing $, so that it is not counted as a character common to the 2 texts)
for each suffix in S (from longest to shortest), find its longest match with the text text1 (traversing the relevant paths in ST1); the longest such match is the final result
if there are multiple longest common substrings, the function returns the one found first
* repeated-substring-of-given-length takes a text and a natural number len and searches for a substring of length len that is repeated in the text
* For stage 4 we use streams.
