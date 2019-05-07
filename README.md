# purescript-quotient

Quotient type approximation in PureScript. In other [words](https://github.com/rightfold/purescript-quotient/pull/3#issuecomment-445951908):

> The library provides a newtype around any other type, but to get it out you first have to apply a normalization function (such as `mod 256`, or `abs`, or `toUpperCase`). This allows zero-overhead wrapping of foreign data that is potentially not normalized, at the cost of unwrapping being more expensive. That's all the library really does.
