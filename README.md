# Christmas-themed procedural gift-wrapping paper generator

This program generates a postscript file that can be printed out and used for wrapping Christmas presents.

![Example](example.png)

I've been hacking on this a little bit every year, mostly on the evening of the 23rd or in the morning of the 24th of December. No, I won't document it, make it configurable, or improve code readability, but still, maybe someone can enjoy it.

## Usage:

Install the [Haskell Stack](https://www.haskellstack.org), and then:

```
stack build
stack exec ChristmasPaper > paper.ps
```
