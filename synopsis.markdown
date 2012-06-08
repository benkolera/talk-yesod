
# Yesod Web Framework and Template Haskell

This month, we have another first-time BFPG speaker! Ben Kolera will give an
introduction to Yesod, a Haskell web application framework with compact
embedded templating languages, and which emphasises end-to-end type safety.

We will also give an introduction to Template Haskell and Quasiquotation, both
Haskell language extensions used extensively in Yesod. Speaker TBC.

Talks will be at an intermediate to advanced level, though we strongly
encourage beginners to come and get excited about the possibilities!

## Type-safe web development with Yesod and Haskell

Developing and maintaining web applications within the constraints of a
stateless client-server protocol is hard enough, without also having to worry
about cross-site scripting attacks, SQL injection, and broken links.

The Yesod Web Framework takes much of the worry out of web development, by
ensuring that most common web programming errors are caught by the compiler.
This means you can focus your development and testing on the interesting parts
of your application. Yesod also comes with a number of compact embedded
domain-specific templating languages, which give the benefit of type safety,
without the overhead.

Under the hood, Yesod uses the excellent GHC compiler and concurrent runtime,
and high-quality libraries for streaming data, so you get great performance by
default.

In this talk, Ben will lead you through the main features of the Yesod Web
Framework, including a number of example applications.

### About Ben Kolera

Ben is an experienced Perl, Java and Scala developer. His enthusiasm for
Haskell causes him to dance at his desk, much to the amusement of his team at
iseek. Ben is also a mad keen fitness fanatic, road cyclist and Muay Thai
student who loves a physical challenge. Tough Mudder Brisbane 2013, anyone? In
the future, Ben hopes to share his interest in functional programming through
teaching, and we welcome his first steps in that direction at BFPG!

## Compile-time metaprogramming with Template Haskell

Sometimes, it is easier to programmatically generate code than to write it by
hand. Template Haskell supports compile-time generation and manipulation of
Haskell code as abstract syntax trees, and allows generated code to be
"spliced" into hand-written code at compile time. Quasiquotes further support
the generation of Haskell code from arbitrary embedded domain-specific
languages.

In this talk, we will introduce Template Haskell by analogy with the much less
sophisticated approach of running code-generation programs from Makefile rules.

