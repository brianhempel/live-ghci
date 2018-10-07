# Live GHCi

Live GHCi lets you put comments in your program...

```haskell
-- 5*5 ==>
```

...that are evaluated whenever the file is saved:

```haskell
-- 5*5 ==> 25
```

If re-evaluation of all special comments is getting too slow, you can change a `==>` to a `=>` and it will be skipped.

## Setup

Start Live GHCi as follows:

```
$ live-ghci
```

Live GHCi watches all Haskell files in the current and deeper subdirectories for changes.