# cardinal

The program takes one argument, which is the path of the ballot file.
The ballot file is a file containing all the ballots, one per line.

A ballot consists of an arbitrary number of comma-separated votes.
The format for a vote is `candidate: vote` (space optional), where `candidate`
is an arbitrary string and `vote` is one of `Support`, `Oppose`, `Abstain`.

`sample.txt` in this repository shows an example ballot file with two ballots.
