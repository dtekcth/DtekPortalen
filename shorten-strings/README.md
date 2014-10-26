# Shorten strings

A unified interface for shorten-ellipsis'ing string-like types.

`String`, `Text`, and `yesod-markdown` provides a `Markdown` instance.

~~~ { .haskell }
shorten 12 "some really really really long string"
-- ==> "some real..."
~~~
