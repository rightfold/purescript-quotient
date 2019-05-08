# purescript-quotient

Quotient type approximation in PureScript.

This library provides a newtype around any other type, but to get a value out you first have to apply a normalization function (such as `mod 256`, or `abs`, or `toUpperCase`). This allows zero-overhead wrapping of foreign data that is potentially not normalized, at the cost of unwrapping being more expensive. That's all the library really does.
