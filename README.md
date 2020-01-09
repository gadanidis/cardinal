# cardinal 🐦

`cardinal` is a very simple Haskell program that computes the result of an
election using [combined approval
voting](https://en.wikipedia.org/wiki/Combined_approval_voting), a type of
[cardinal voting system](https://en.wikipedia.org/wiki/Cardinal_voting).

The program takes one or more ballot files as arguments.
The ballot file is a file containing one or more ballots, one ballot per line.

A ballot consists of an arbitrary number of comma-separated votes.
The format for a vote is `candidate|vote`, where `candidate` is an arbitrary
string and `vote` is one of `Support`, `Oppose`, `Abstain`.
The program strips the ballot file of spaces before computing the result, so
`haskell | Support` is the same as `haskell|Support` or `haskell | S u p p o r
t`.

The file `sample.txt` in this repository is an example ballot file.
