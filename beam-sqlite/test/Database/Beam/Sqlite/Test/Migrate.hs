module Database.Beam.Sqlite.Test.Migrate (tests) where

import Control.Exception (try, IOException)
import Data.List (isInfixOf)
import Database.SQLite.Simple hiding (field)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.List.NonEmpty as NE
import Data.Int (Int32)
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions (SqlSerial(..))
import Database.Beam.Sqlite
import Database.Beam.Sqlite.Migrate
import Database.Beam.Migrate
import Database.Beam.Migrate.Simple

import Database.Beam.Sqlite.Test
import Database.Beam.Sqlite.Syntax (SqliteIndexOptions)

tests :: TestTree
tests = testGroup "Migration tests"
  [ verifiesPrimaryKey
  , verifiesNoPrimaryKey
  , verifiesIndex
  , verifiesUniqueIndex
  , migrateNonUniqueToUniqueIndex
  , verifiesForeignKey
  , addingForeignKeyFails
  , verifiesForeignKeyActions
  , foreignKeyActionsWork
  , idempotentMigration
  , migratesMissingNotNull
  , doesNotUseNotNullForOtherConstraints
  , verifiesColumnReferences
  , columnReferencesRoundTrip
  , columnReferencesOrderCreateTable
  , columnReferencesCarryActions
  ]

newtype WithPkT f = WithPkT
  { _with_pk_value :: C f Bool
  } deriving (Generic, Beamable)

instance Table WithPkT where
  newtype PrimaryKey WithPkT f = Pk (C f Bool)
    deriving (Generic, Beamable)

  primaryKey = Pk . _with_pk_value

data WithPkDb entity = WithPkDb
  { _with_pk :: entity (TableEntity WithPkT)
  } deriving (Generic, Database Sqlite)

withPkDbChecked :: CheckedDatabaseSettings Sqlite WithPkDb
withPkDbChecked = defaultMigratableDbSettings

newtype WithoutPkT f = WithoutPkT
  { _without_pk_value :: C f Bool
  } deriving (Generic, Beamable)

instance Table WithoutPkT where
  data PrimaryKey WithoutPkT f = NoPk
    deriving (Generic, Beamable)

  primaryKey _ = NoPk

data WithoutPkDb entity = WithoutPkDb
  { _without_pk :: entity (TableEntity WithoutPkT)
  } deriving (Generic, Database Sqlite)

withoutPkDbChecked :: CheckedDatabaseSettings Sqlite WithoutPkDb
withoutPkDbChecked = defaultMigratableDbSettings

verifiesPrimaryKey :: TestTree
verifiesPrimaryKey = testCase "verifySchema correctly detects primary key" $
  withTestDb $ \conn -> do
    execute_ conn "create table with_pk (with_pk_value bool not null primary key)"
    testVerifySchema conn withPkDbChecked

verifiesNoPrimaryKey :: TestTree
verifiesNoPrimaryKey = testCase "verifySchema correctly handles table with no primary key" $
  withTestDb $ \conn -> do
    execute_ conn "create table without_pk (without_pk_value bool not null)"
    testVerifySchema conn withoutPkDbChecked

testVerifySchema
  :: Database Sqlite db
  => Connection -> CheckedDatabaseSettings Sqlite db -> Assertion
testVerifySchema conn db =
  runBeamSqlite conn (verifySchema migrationBackend db) >>= \case
    VerificationSucceeded -> return ()
    VerificationFailed failures ->
      fail $ "Verification failed: " ++ show failures

-- Shared table type for index tests

newtype IdxT f = IdxT
  { _idx_value :: C f Int32
  } deriving (Generic, Beamable)

instance Table IdxT where
  newtype PrimaryKey IdxT f = IdxPk (C f Int32)
    deriving (Generic, Beamable)
  primaryKey = IdxPk . _idx_value

data IdxDb entity = IdxDb
  { _idx_tbl :: entity (TableEntity IdxT)
  } deriving (Generic, Database Sqlite)

verifiesIndex :: TestTree
verifiesIndex = testCase "verifySchema correctly detects a secondary index" $
  withTestDb $ \conn -> do
    execute_ conn "create table idx_tbl (idx_value int not null primary key)"
    execute_ conn "create index idx_tbl_value on idx_tbl (idx_value)"
    let db :: CheckedDatabaseSettings Sqlite IdxDb
        db = defaultMigratableDbSettings `withDbModification`
              (dbModification @_ @Sqlite)
                { _idx_tbl =
                    addTableIndex "idx_tbl_value" (defaultIndexOptions @SqliteCommandSyntax)
                      (\t -> selectorColumnName _idx_value t NE.:| []) }
    testVerifySchema conn db

verifiesUniqueIndex :: TestTree
verifiesUniqueIndex = testCase "verifySchema correctly detects a UNIQUE secondary index" $
  withTestDb $ \conn -> do
    execute_ conn "create table idx_tbl (idx_value int not null primary key)"
    execute_ conn "create unique index idx_tbl_value_uniq on idx_tbl (idx_value)"
    let db :: CheckedDatabaseSettings Sqlite IdxDb
        db = defaultMigratableDbSettings `withDbModification`
              (dbModification @_ @Sqlite)
                { _idx_tbl =
                    let idxOpts = setUniqueIndexOptions @SqliteCommandSyntax True
                                $ defaultIndexOptions @SqliteCommandSyntax
                    in
                    addTableIndex "idx_tbl_value_uniq" idxOpts
                      (\t -> selectorColumnName _idx_value t NE.:| []) }
    testVerifySchema conn db

-- | Check that we can change the uniqueness of an index in a migration
migrateNonUniqueToUniqueIndex :: TestTree
migrateNonUniqueToUniqueIndex =
  testCase "autoMigrate can change a non-unique index to a unique index" $
  withTestDb $ \conn -> do
    let nonUnique :: CheckedDatabaseSettings Sqlite IdxDb
        nonUnique =
          defaultMigratableDbSettings `withDbModification`
            (dbModification @_ @Sqlite)
              { _idx_tbl =
                  addTableIndex "idx_tbl_value"
                    (defaultIndexOptions @SqliteCommandSyntax)
                    (\t -> selectorColumnName _idx_value t NE.:| []) }
        unique :: CheckedDatabaseSettings Sqlite IdxDb
        unique =
          defaultMigratableDbSettings `withDbModification`
            (dbModification @_ @Sqlite)
              { _idx_tbl =
                  addTableIndex "idx_tbl_value"
                    (setUniqueIndexOptions @SqliteCommandSyntax True $
                     defaultIndexOptions @SqliteCommandSyntax)
                    (\t -> selectorColumnName _idx_value t NE.:| []) }
    runBeamSqlite conn $ autoMigrate migrationBackend nonUnique
    runBeamSqlite conn $ autoMigrate migrationBackend unique

-- Foreign key tests

data ParentT f = ParentT
  { _parent_id :: C f Int32
  } deriving (Generic, Beamable)

instance Table ParentT where
  newtype PrimaryKey ParentT f = ParentPk (C f Int32)
    deriving (Generic, Beamable)
  primaryKey = ParentPk . _parent_id

data ChildT f = ChildT
  { _child_id        :: C f Int32
  , _child_parent_id :: PrimaryKey ParentT f
  } deriving (Generic, Beamable)

instance Table ChildT where
  newtype PrimaryKey ChildT f = ChildPk (C f Int32)
    deriving (Generic, Beamable)
  primaryKey = ChildPk . _child_id

data FkDb entity = FkDb
  { _fk_parent :: entity (TableEntity ParentT)
  , _fk_child  :: entity (TableEntity ChildT)
  } deriving (Generic, Database Sqlite)

verifiesForeignKey :: TestTree
verifiesForeignKey = testCase "verifySchema detects a plain foreign key" $
  withTestDb $ \conn -> do
    execute_ conn "create table fk_parent (parent_id int not null primary key)"
    execute_ conn "create table fk_child  (child_id int not null primary key, \
                  \child_parent_id int not null, \
                  \foreign key (child_parent_id) references fk_parent (parent_id))"
    let db :: CheckedDatabaseSettings Sqlite FkDb
        db = defaultMigratableDbSettings `withDbModification`
              (dbModification @_ @Sqlite)
                { _fk_child =
                    addTableForeignKey (_fk_parent db)
                      (foreignKeyColumns _child_parent_id)
                      primaryKeyColumns
                      ForeignKeyNoAction
                      ForeignKeyNoAction
                    <> modifyCheckedTable id
                         (ChildT { _child_id        = "child_id"
                                 , _child_parent_id = ParentPk "child_parent_id" }) }
    testVerifySchema conn db

-- Schema used to test solver behaviour when a foreign key migration is impossible.
-- Needs to be somewhat large to trigger the exponential blowup in search space.

data WideParentT f = WideParentT
  { _wp_id :: C f Int32
  , _wp_a  :: C f Int32
  , _wp_b  :: C f Int32
  , _wp_c  :: C f Int32
  , _wp_d  :: C f Int32
  } deriving (Generic, Beamable)

instance Table WideParentT where
  newtype PrimaryKey WideParentT f = WideParentPk (C f Int32)
    deriving (Generic, Beamable)
  primaryKey = WideParentPk . _wp_id

data WideChildT f = WideChildT
  { _wc_id        :: C f Int32
  , _wc_parent_id :: PrimaryKey WideParentT f
  , _wc_a         :: C f Int32
  , _wc_b         :: C f Int32
  , _wc_c         :: C f Int32
  , _wc_d         :: C f Int32
  } deriving (Generic, Beamable)

instance Table WideChildT where
  newtype PrimaryKey WideChildT f = WideChildPk (C f Int32)
    deriving (Generic, Beamable)
  primaryKey = WideChildPk . _wc_id

data WideFkDb entity = WideFkDb
  { _wide_parent :: entity (TableEntity WideParentT)
  , _wide_child  :: entity (TableEntity WideChildT)
  } deriving (Generic, Database Sqlite)

-- | Check that the solver doesn't grow an enormous search space trying to find
-- a way to add a foreign key constraint (which SQLite cannot do to an existing
-- table).
addingForeignKeyFails :: TestTree
addingForeignKeyFails =
  testCase "autoMigrate fails promptly when asked to add a foreign key" $
  withTestDb $ \conn -> do
    let noFk :: CheckedDatabaseSettings Sqlite WideFkDb
        noFk = defaultMigratableDbSettings
        withFk :: CheckedDatabaseSettings Sqlite WideFkDb
        withFk = noFk `withDbModification`
                   (dbModification @_ @Sqlite)
                     { _wide_child =
                         addTableForeignKey (_wide_parent withFk)
                           (foreignKeyColumns _wc_parent_id)
                           primaryKeyColumns
                           ForeignKeyNoAction
                           ForeignKeyNoAction }
    runBeamSqlite conn $ autoMigrate migrationBackend noFk
    result <- try @IOException (runBeamSqlite conn $ autoMigrate migrationBackend withFk)
    case result of
      Left e
        | "Could not determine migration" `isInfixOf` show e
        -> return ()
      Left e  -> assertFailure $ unlines
        [ "unexpected exception from autoMigrate:"
        , "  - " ++ show e
        ]
      Right _ -> assertFailure $ unlines
        [ "expected autoMigrate to fail:"
        , "  SQLite cannot add foreign key constraints to existing tables"
        ]

verifiesForeignKeyActions :: TestTree
verifiesForeignKeyActions =
  testCase "verifySchema detects a foreign key with ON DELETE & ON UPDATE actions" $
  withTestDb $ \conn -> do
    execute_ conn "create table fk_parent (parent_id int not null primary key)"
    execute_ conn "create table fk_child  (child_id int not null primary key, \
                  \child_parent_id int not null, \
                  \foreign key (child_parent_id) references fk_parent (parent_id) \
                  \on delete cascade on update restrict)"
    let db :: CheckedDatabaseSettings Sqlite FkDb
        db = defaultMigratableDbSettings `withDbModification`
              (dbModification @_ @Sqlite)
                { _fk_child =
                    addTableForeignKey (_fk_parent db)
                      (foreignKeyColumns _child_parent_id)
                      primaryKeyColumns
                      ForeignKeyActionRestrict
                      ForeignKeyActionCascade
                    <> modifyCheckedTable id
                         (ChildT { _child_id        = "child_id"
                                 , _child_parent_id = ParentPk "child_parent_id" }) }
    testVerifySchema conn db

-- | Verifies that foreign key actions are enforced at runtime.
foreignKeyActionsWork :: TestTree
foreignKeyActionsWork =
  testCase "foreign key actions" $
  withTestDb $ \conn -> do
    -- Enable foreign key support (required for SQLite)
    execute_ conn "PRAGMA foreign_keys = ON"
    let db :: CheckedDatabaseSettings Sqlite FkDb
        db = defaultMigratableDbSettings `withDbModification`
              (dbModification @_ @Sqlite)
                { _fk_child =
                    addTableForeignKey (_fk_parent db)
                      (foreignKeyColumns _child_parent_id)
                      primaryKeyColumns
                      ForeignKeyActionCascade
                      ForeignKeyActionCascade
                }
        unc = unCheckDatabase db
    runBeamSqlite conn $ autoMigrate migrationBackend db

    -- Insert two parents and three children (two for parent 1, one for parent 2).
    runBeamSqlite conn $ do
      runInsert $ insert (_fk_parent unc) $ insertValues
        [ ParentT 1, ParentT 2 ]
      runInsert $ insert (_fk_child unc) $ insertValues
        [ ChildT 1 (ParentPk 1), ChildT 2 (ParentPk 1), ChildT 3 (ParentPk 2) ]

    -- ON UPDATE CASCADE: changing parent_id 1 → 10 should cascade to child rows.
    runBeamSqlite conn $
      runUpdate $ update (_fk_parent unc)
        (\p -> _parent_id p <-. val_ 10)
        (\p -> _parent_id p ==. val_ 1)
    childrenOf10 <- runBeamSqlite conn $ runSelectReturningList $ select $
      filter_ (\c -> let ParentPk pid = _child_parent_id c in pid ==. val_ 10) $ all_ (_fk_child unc)
    assertEqual "two children should now reference updated parent id 10"
      2 (length childrenOf10)
    childrenOf1 <- runBeamSqlite conn $ runSelectReturningList $ select $
      filter_ (\c -> let ParentPk pid = _child_parent_id c in pid ==. val_ 1) $ all_ (_fk_child unc)
    assertEqual "no children should still reference old parent id 1"
      0 (length childrenOf1)

    -- ON DELETE CASCADE: deleting parent 2 should remove its child row.
    runBeamSqlite conn $
      runDelete $ delete (_fk_parent unc) (\p -> _parent_id p ==. val_ 2)
    childrenOf2 <- runBeamSqlite conn $ runSelectReturningList $ select $
      filter_ (\c -> let ParentPk pid = _child_parent_id c in pid ==. val_ 2) $ all_ (_fk_child unc)
    assertEqual "child of deleted parent 2 should be removed"
      0 (length childrenOf2)
    allChildren <- runBeamSqlite conn $ runSelectReturningList $ select $
      all_ (_fk_child unc)
    assertEqual "only the two children of parent 10 should remain"
      2 (length allChildren)

--------------------------------------------------------------------------------
-- Ensure migration is idempotent (no data loss due to internal tables)

data SimpleT f = SimpleT
  { _auto_incr_id :: C f (SqlSerial Int32)
  } deriving (Generic, Beamable)

instance Table SimpleT where
  newtype PrimaryKey SimpleT f = AutoIncrPk (C f (SqlSerial Int32))
    deriving (Generic, Beamable)
  primaryKey = AutoIncrPk . _auto_incr_id

data SimpleDb entity = SimpleDb
  { _simple_tbl :: entity (TableEntity SimpleT)
  } deriving (Generic, Database Sqlite)

simpleCheckedDb :: CheckedDatabaseSettings Sqlite SimpleDb
simpleCheckedDb = defaultMigratableDbSettings

idempotentMigration :: TestTree
idempotentMigration =
  testCase "autoMigrate is idempotent" $
  withTestDb $ \conn -> do
    runBeamSqlite conn $ autoMigrate migrationBackend simpleCheckedDb
    -- Insert a row so that sqlite_sequence is populated in sqlite_master.
    execute_ conn "INSERT INTO simple_tbl DEFAULT VALUES"
    -- Make sure we don't think a migration would lose data due to the
    -- presence of the sqlite_sequence table.
    runBeamSqlite conn $ autoMigrate migrationBackend simpleCheckedDb

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- SET NOT NULL / DROP NOT NULL are only used for NOT NULL constraints

data NullableT f = NullableT
  { _nullable_id    :: C f Int32
  , _nullable_value :: C f Int32
  } deriving (Generic, Beamable)

instance Table NullableT where
  newtype PrimaryKey NullableT f = NullablePk (C f Int32)
    deriving (Generic, Beamable)
  primaryKey = NullablePk . _nullable_id

data NullableDb entity = NullableDb
  { _nullable_tbl :: entity (TableEntity NullableT)
  } deriving (Generic, Database Sqlite)

-- | The solver still offers @SET NOT NULL@ for an actually-missing @NOT NULL@.
--
-- SQLite has no @ALTER TABLE ... ALTER COLUMN@, so its 'setNotNullSyntax'
-- renders as a no-op and the constraint is not really added; all this checks is
-- that the action is still reachable. See the beam-postgres suite for the
-- end-to-end version.
migratesMissingNotNull :: TestTree
migratesMissingNotNull =
  testCase "autoMigrate finds a plan for a missing NOT NULL constraint" $
  withTestDb $ \conn -> do
    execute_ conn "create table nullable_tbl (nullable_id int not null primary key, \
                  \nullable_value int)"
    let db :: CheckedDatabaseSettings Sqlite NullableDb
        db = defaultMigratableDbSettings
    runBeamSqlite conn $ autoMigrate migrationBackend db

-- | A column constraint that a backend cannot report (here, a column-level
-- @UNIQUE@) must not be \"satisfied\" by emitting @SET NOT NULL@.
doesNotUseNotNullForOtherConstraints :: TestTree
doesNotUseNotNullForOtherConstraints =
  testCase "SET NOT NULL is not offered for non-NOT NULL column constraints" $
  withTestDb $ \conn -> do
    let db :: CheckedDatabaseSettings Sqlite NullableDb
        db = evaluateDatabase $ migrationStep "initial" $ const $
               NullableDb
                 <$> createTable "nullable_tbl"
                       (NullableT (field "nullable_id" int notNull)
                                  (field "nullable_value" int notNull unique))
    -- Build the table exactly as the migration describes it, so the only
    -- unsatisfied predicate is the column-level UNIQUE.
    execute_ conn "create table nullable_tbl (nullable_id INT NOT NULL, \
                  \nullable_value INT NOT NULL UNIQUE, primary key (nullable_id))"

    result <- try @IOException (runBeamSqlite conn $ autoMigrate migrationBackend db)
    case result of
      Left e
        | "Could not determine migration" `isInfixOf` show e
        -> return ()
      Left e -> assertFailure $ unlines
        [ "unexpected exception from autoMigrate:"
        , "  - " ++ show e
        ]
      Right _ -> assertFailure $ unlines
        [ "expected autoMigrate to fail:"
        , "  a column-level UNIQUE is not reported by getDbConstraints, so the"
        , "  predicate is unsatisfiable; the solver must not pretend that"
        , "  ALTER TABLE ... SET NOT NULL establishes it"
        ]

--------------------------------------------------------------------------------
-- Column-level REFERENCES is checked as a table-level foreign key

data RefParentT f = RefParentT
  { _ref_parent_id :: C f Int32
  } deriving (Generic, Beamable)

instance Table RefParentT where
  newtype PrimaryKey RefParentT f = RefParentPk (C f Int32)
    deriving (Generic, Beamable)
  primaryKey = RefParentPk . _ref_parent_id

data RefChildT f = RefChildT
  { _ref_child_id     :: C f Int32
  , _ref_child_parent :: PrimaryKey RefParentT f
  } deriving (Generic, Beamable)

instance Table RefChildT where
  newtype PrimaryKey RefChildT f = RefChildPk (C f Int32)
    deriving (Generic, Beamable)
  primaryKey = RefChildPk . _ref_child_id

data RefDb entity = RefDb
  { _ref_parent :: entity (TableEntity RefParentT)
  , _ref_child  :: entity (TableEntity RefChildT)
  } deriving (Generic, Database Sqlite)

refDbStepsWith :: ForeignKeyAction -> ForeignKeyAction
               -> MigrationSteps Sqlite () (CheckedDatabaseSettings Sqlite RefDb)
refDbStepsWith onUpdate onDelete =
  migrationStep "initial" $ const $
    RefDb
      <$> createTable "ref_parent"
            (RefParentT (field "ref_parent_id" int notNull))
      <*> createTable "ref_child"
            (RefChildT (field "ref_child_id" int notNull)
                       (RefParentPk (field "ref_child_parent" int notNull
                                       (references "ref_parent" ("ref_parent_id" NE.:| [])
                                                   onUpdate onDelete))))

refDbSteps :: MigrationSteps Sqlite () (CheckedDatabaseSettings Sqlite RefDb)
refDbSteps = refDbStepsWith ForeignKeyNoAction ForeignKeyNoAction

refDbWith :: ForeignKeyAction -> ForeignKeyAction -> CheckedDatabaseSettings Sqlite RefDb
refDbWith onUpdate onDelete = evaluateDatabase (refDbStepsWith onUpdate onDelete)

refDbChecked :: CheckedDatabaseSettings Sqlite RefDb
refDbChecked = refDbWith ForeignKeyNoAction ForeignKeyNoAction

verifiesColumnReferences :: TestTree
verifiesColumnReferences =
  testCase "verifySchema accepts a column-level REFERENCES" $
  withTestDb $ \conn -> do
    execute_ conn "create table ref_parent (ref_parent_id INT NOT NULL, \
                  \primary key (ref_parent_id))"
    execute_ conn "create table ref_child (ref_child_id INT NOT NULL, \
                  \ref_child_parent INT NOT NULL REFERENCES ref_parent (ref_parent_id), \
                  \primary key (ref_child_id))"
    testVerifySchema conn refDbChecked

-- | A schema declared with 'references' verifies against the database its own
-- migration produces, and needs no further migration.
columnReferencesRoundTrip :: TestTree
columnReferencesRoundTrip =
  testCase "a column-level REFERENCES round-trips through getDbConstraints" $
  withTestDb $ \conn -> do
    -- createTable steps have no down migration, so bringUpToDate treats them as
    -- irreversible and declines to run them under the default hooks.
    let hooks = defaultUpToDateHooks { runIrreversibleHook = pure True }
    runBeamSqlite conn (bringUpToDateWithHooks hooks migrationBackend refDbSteps) >>= \case
      Nothing -> assertFailure "bringUpToDate declined to run the migration"
      Just (_ :: CheckedDatabaseSettings Sqlite RefDb) -> pure ()
    testVerifySchema conn refDbChecked

-- | The solver knows @ref_child@ depends on @ref_parent@, so it creates the
-- referenced table first.
columnReferencesOrderCreateTable :: TestTree
columnReferencesOrderCreateTable =
  testCase "createSchema orders a column-level REFERENCES after its target" $
  withTestDb $ \conn -> do
    execute_ conn "PRAGMA foreign_keys = ON"
    runBeamSqlite conn $ createSchema migrationBackend refDbChecked
    testVerifySchema conn refDbChecked

columnReferencesCarryActions :: TestTree
columnReferencesCarryActions =
  testCase "a column-level REFERENCES carries ON UPDATE and ON DELETE actions" $
  withTestDb $ \conn -> do
    let db = refDbWith ForeignKeyActionRestrict ForeignKeyActionCascade
    runBeamSqlite conn $ createSchema migrationBackend db
    testVerifySchema conn db
