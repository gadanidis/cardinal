# cardinal 🐦

## Description and usage

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

The file `sample.txt` in this repository is an example ballot file representing
an election for a position with candidates `john`, `meg` and `kyle`. `meg` wins
with a score of 3, `john` is the runner-up with a score of 2, and `kyle` comes
in last with a score of -3.

## Installation instructions

1. Install [the Haskell Tool Stack](https://www.haskellstack.org): `curl -sSL
   https://get.haskellstack.org/ | sh`
2. Clone this repository: `git clone https://github.com/gadanidis/cardinal`
3. Navigate to the repository, build, and install: `cd cardinal && stack build
   && stack install`
