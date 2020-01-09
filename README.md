# cardinal 🐦

`cardinal` is a very simple Haskell program that computes the result of an
election using [combined approval
voting](https://en.wikipedia.org/wiki/Combined_approval_voting), a type of
[cardinal voting system](https://en.wikipedia.org/wiki/Cardinal_voting).

The program takes one argument, which is the path of the ballot file.
The ballot file is a file containing all the ballots, one per line.

A ballot consists of an arbitrary number of comma-separated votes.
The format for a vote is `candidate: vote` (space optional), where `candidate`
is an arbitrary string and `vote` is one of `Support`, `Oppose`, `Abstain`.

The file `sample.txt` in this repository is an example ballot file with two
ballots.
