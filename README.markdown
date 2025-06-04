polcyr
======

Polish Latin orthography to Cyrillic orthography converter.

It uses ẃ, ḿ and ṕ to mark softness in codas (*żuraw* is *żuraẃ*,
*karp* - *karṕ*, etc.). Soft coda *b* and *f* are unmarked because
"b with acute" and "f with acute" are combining characters, thus not
treated as a single character in Lisp.
