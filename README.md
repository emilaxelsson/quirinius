# Quirinius

Quirinius is a simple type-safe EDSL for database queries.

Notable features:

  * Works with normal Haskell records
  * Type-safety obtained through typed record labels
  * Supports inner queries
  * Integrates with [persistent](https://hackage.haskell.org/package/persistent) (providing multiple backends)

See the [examples/Test.hs](examples/Test.hs) to get a feeling for how Quirinius works.

## Implementation

An interesting aspect of the implementation is the minimal use of generics and absence of TemplateHaskell. Only two type classes need to be derived for Haskell records that represent database tables:

  * `TableRecord` for getting the table name
      - Default instance derives table name from type name.
  * `QueryResult` for deserializing results from the database
      - Default instance works for ordinary Haskell records.

## Shortcomings

  * Only a restricted form of select queries are supported, no write operations.
  * Quirinius has no knowledge of schemas.
  * Queries cannot have shared temporary variables.
      - (Could be solved by CSE.)
