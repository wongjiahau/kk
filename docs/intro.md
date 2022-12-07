# Introduction

KK is a type safe programming language that encourage users to natural
language-like (NLL) programs.

It is the successor of [Keli Language](https://keli-language.gitbook.io/doc/),
with similar motivations.

Most programming languages that allows NLL syntax either lacks a sufficient
type system, like LISP, or have complicated parsing rules, like Agda Mixfix
functions.

## Examples

The following snippets represents valid KK expressions that can be user-defined:

Cron job:

```kk
backup database .every day at (time 10 30 pm)
```

Async utility:

```
write (data) to "./hello.kk"
  .retry up to 3 times
```

String functions:

```kk
"Hello world" .replace each "l" with "o"
```

Conditional:

```kk
if (x > 3) then { "Yes" } else { "No" }
```
