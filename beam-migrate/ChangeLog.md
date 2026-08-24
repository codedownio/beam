# 0.6.1.0

## Interface changes

* `Constraint` is now a data type carrying both the constraint syntax and the
  check that constraint contributes to the schema. The `Constraint` constructor
  is retained as a bidirectional pattern synonym, so `Constraint <syntax>` keeps
  working as both a constructor and a pattern.
* `FieldReturnType`'s `field'` method now takes `[FieldConstraint be]` rather
  than `[BeamSqlBackendColumnConstraintDefinitionSyntax be]`. Instances that
  thread the list through unchanged (the only sensible implementation) need no
  edit.
* `IsSql92ReferentialActionSyntax` gained `referentialActionRestrictSyntax`.
  The table-level `foreignKeyConstraintSyntax` has always accepted
  `ForeignKeyActionRestrict`; this completes the column-level constraint syntax.

## Added features

* Added `references`, a `REFERENCES` column constraint that is recorded as a
  `TableHasForeignKey` predicate rather than a `TableColumnHasConstraint`.

  Backends report foreign keys as `TableHasForeignKey` when reading a live
  database, so a `REFERENCES` recorded as a `TableColumnHasConstraint` produces
  a predicate that no database can satisfy: `verifySchema` fails against a
  database beam itself just created, and `autoMigrate` can never converge.
  Declaring the constraint with `references` makes the predicate round-trip, and
  lets the solver see the dependency between the two tables so that
  `createSchema` orders their `CREATE TABLE`s correctly.

  `Constraint (referencesConstraintSyntax ...)` still produces the old,
  non-round-tripping check, so existing code is unaffected.

## Bug fixes

* The `SET NOT NULL` and `DROP NOT NULL` action providers only apply to
  `NOT NULL` constraints now. Their guards had been commented out with a
  `TODO`, so the solver treated `ALTER TABLE ... SET NOT NULL` as a way to
  establish *any* `TableColumnHasConstraint` — including ones it cannot
  establish, such as a column-level `UNIQUE`. That produced migrations which
  silently did not do what they claimed, and flooded the search graph with
  edges that change nothing observable: on a schema whose predicates cannot be
  satisfied, the solver went from taking over two minutes at eight tables to
  concluding "not possible" in milliseconds at forty.

## Updated dependencies

* Tightened bounds on `haskell-src-exts`, with a minimum version of 1.23.
* Tightened bounds on `hashable`, with a minimum version of 1.4.
* Tightened bounds on `aeson`, with a minimum of version 2.0.

# 0.6.0.0

## Interface changes

* `BeamSqlBackendHasSerial` is now polymorphic over the column's underlying
  integer type. The class gained a new parameter `n` and `genericSerial` now
  has type `FieldReturnType 'True 'False be (SqlSerial n) a => Text -> a`,
  matching the corresponding generalisation of `genericSerial` in
  `beam-core` (#534). Existing instances must be updated to declare which
  integer types they support (e.g. `Int16`, `Int32`, `Int64`).

## Added features

* Added a `weekField` definition to the `IsSql92ExtractFieldSyntax` instance
  for `HsExpr`, in support of the new `week_` extract field in `beam-core`.

## Updated dependencies

* Bumped the lower bound on `beam-core` to `0.11`.

# 0.5.5.0

## Added features

* Added support for foreign key constraints:

  * New `ForeignKeyAction` datatype representing possible actions when updating
    or deleting a row with referencing foreign keys.
  * The `IsSql92TableConstraintSyntax` typeclass now has an additional method,
    `foreignKeyConstraintSyntax`, for constructing foreign key constraint syntax.
  * Introduce `addTableForeignKey` for declaring new foreign key constraints.

* Added support for temporary tables:

  * The `BeamHasTempTables` typeclass is used for backends to emit
    `CREATE TEMPORARY TABLE` syntax.
  * The `runCreateTempTable` command allows creating a temporary table.
    The returned table entity can be used in queries like any other table.

## Bug fixes

* Fix an issue in which `beam-migrate` would fail to migrate a unique index
  to a non-unique index or vice-versa.

* Fixed an issue in which the migration solver could end up taking an
  exponentially long time to conclude that migration isn't possible.

# 0.5.4.0

## Added features

* Added support for declaring secondary indices on tables. User API is the
  `addTableIndex` function, `selectorColumnName` and `foreignKeyColumns` helpers.
  Backend support goes through new `IsSql92CreateDropIndexSyntax` (which carries
  a per-backend `Sql92CreateIndexOptionsSyntax` type family) and
  `IsSql92UniqueIndexSyntax` (for index uniqueness constraints).

## Updated dependencies

* Updated the upper bound on `parallel` to include `parallel-3.3.0.0`
* Updated the upper bound on `time` to include `time-1.14`

# 0.5.3.2

## Dependencies

* Removed explicit dependency on `ghc-prim`, which was not used directly.
* Updated the upper bound to include `containers-0.8`.

# 0.5.3.1

## Bug fixes

* Removed the `IsString` instance for `DatabaseSchema`, which allowed for the use of database schemas that did not exist.

# 0.5.3.0

## Added features

* Added support for creating database schemas and associated tables with `createDatabaseSchema` and `createTableWithSchema`, as well as dropping schemas with `dropDatabaseSchema` (#716).

# 0.5.2.1

## Added features

 * Loosen some version bounds

# 0.5.2.0

## Added features

 * `IN (SELECT ...)` syntax via `inSelectE`

# 0.5.1.2

## Added features

 * Aeson 2.0 support

# 0.5.1.1

## Added features

 * GHC 9.2 and 9.0 support

# 0.5.1.0

## Added features

 * Expose `IsNotNull` class

## Bug fixes

 * Order log entries when verifying migration status

# 0.5.0.0

## Interface changes

 * Removed instances for machine-dependent ambiguous integer types `Int` and `Word`
 * Require `MonadFail` for `BeamMigrationBackend`

## Added features

 * GHC 8.8 support
 * `checkSchema`: Like `verifySchema`, but detects and returns unexpected
   predicates found in the live database

## Bug fixes

 * Map `Int16` to `smallIntType` instead of `intType`
 * Suppress creation of primary key constraints for tables with no primary keys

## 0.4.0.0

## 0.3.2.0

Added `haskellSchema` shortcut

## 0.3.1.0

Add `Semigroup` instances to prepare for GHC 8.4 and Stackage nightly

## 0.3.0.0

* Re-introduce backend parameter as `Database` type class
* Move beam migration log schema to beam-migrate, since many
  applications will want to easily manage a database using the
  haskell-based migrations
* Add `bringUpToDate` and `bringUpToDateWithHooks` to
  `Database.Beam.Migrate.Simple`, which can be used to bring a
  database up to date with the given migration.

## 0.2.0.0  -- 2018-01-20

* First version. Released on an unsuspecting world.
