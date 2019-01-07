# Quirinius

Quirinius is a simple type-safe EDSL for database queries.

Notable features:

  * Queries expressed compositionally using `(&)` (inspired by Rails active record queries)
  * Works with "normal" Haskell records (see below)
  * Type-safety obtained through GHC's `HasField` class
  * Supports inner queries
  * Integrates with [persistent](https://hackage.haskell.org/package/persistent) (providing multiple backends)

"Normal records" means that a type such as

```haskell
data Person = Person
  { name :: Text
  , age  :: Int
  }
```

can be used as is, provided that it corresponds to a table in the database. Note, however, that referring to fields inside queries requires `OverloadedLabels`. That is, you have to use `#name` to refer to the `name` column in a query.

See the [examples/Test.hs](examples/Test.hs) to get a feeling for how Quirinius works.

## Implementation

An interesting aspect of the implementation is the minimal use of generics and absence of TemplateHaskell. Only two type classes need to be derived for Haskell records that represent database tables:

  * `TableRecord` for getting the table name
      - Default instance derives table name from type name.
  * `QueryResult` for deserializing results from the database
      - Default instance works for ordinary Haskell records.

## Shortcomings

  * Only a restricted form of select queries are supported, no write operations.
  * Joins are not (yet) supported.
  * Quirinius has no knowledge of schemas.
  * Queries cannot have shared temporary variables.
      - (Could be solved by CSE.)
