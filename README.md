# Minions

Minions is a fast parallel SSH tool written in haskell. It uses haskell's
concurrent channel abstraction to return results quickly and incrementally.

### Usage:

```minions -h <file with list of hostnames> <ssh command to run>```

```
$ minions -h list_of_hostnames.txt uname
host1 (0.37s)
Darwin

host2 (1.2s)
Darwin

host3 (1.3s)
Darwin
$
```

There's also a small mode that prints the results in a compact format:

```
$ minions -s -h list_of_hostsnames.txt uname
host1: Darwin
host2: Darwin
host3: Darwin
$
```

### Installation

First install ghc via your system's package manager. On OSX it's as simple as:

```brew install haskell-platform```

then install minions with:

```cabal install minions```
