{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Database.Beam.Migrate.SQL.Tables
  ( -- * Table manipulation

    -- ** Creation and deletion
    createTable, createTableWithSchema 
  , dropTable
  , preserve

    -- ** @ALTER TABLE@
  , TableMigration(..)
  , ColumnMigration(..)
  , alterTable

  , renameTableTo, renameColumnTo
  , addColumn, dropColumn

    -- * Schema manipulation
  , DatabaseSchema(databaseSchemaName), createDatabaseSchema, dropDatabaseSchema, existingDatabaseSchema

    -- * Field specification
  , DefaultValue, Constraint(Constraint), NotNullConstraint
  , ConstraintCheck(..)

  , field

  , defaultTo_, notNull, unique, references

    -- ** Internal classes
    --    Provided without documentation for use in type signatures
  , FieldReturnType(..)
  , FieldConstraint(..)
  , IsNotNull
  ) where

import Database.Beam
import Database.Beam.Schema.Tables
import Database.Beam.Backend.SQL
import Database.Beam.Backend.SQL.AST (TableName(..))
import Database.Beam.Query.Internal (tableNameFromEntity)

import Database.Beam.Migrate.Types
import Database.Beam.Migrate.Checks
import Database.Beam.Migrate.SQL.Types
import Database.Beam.Migrate.SQL.SQL92

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Writer.Strict
import Control.Monad.State

import Data.Coerce (coerce)
import Data.Kind (Type)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import Data.Typeable
import qualified Data.Kind as Kind (Constraint)

import GHC.TypeLits

import Lens.Micro ((^.))

-- * Table manipulation

-- | Add a @CREATE TABLE@ statement to this migration
--
--   The first argument is the name of the table.
--
--   The second argument is a table containing a 'FieldSchema' for each field.
--   See documentation on the 'Field' command for more information.
--
--   To create a table in a specific schema, see 'createTableWithSchema'.
createTable :: ( Beamable table, Table table
               , BeamMigrateSqlBackend be )
            => Text -> TableSchema be table
            -> Migration be (CheckedDatabaseEntity be db (TableEntity table))
createTable = createTableWithSchema Nothing

-- * Schema manipulation

-- | Represents a database schema. To create one, see 'createDatabaseSchema'; 
--   to materialize one, see 'existingDatabaseSchema'.
newtype DatabaseSchema 
  = DatabaseSchema{databaseSchemaName :: Text}
  deriving (Eq, Show)

-- | Add a @CREATE SCHEMA@ statement to this migration
--
--   To create a table in a specific schema, see 'createTableWithSchema'.
--   To drop a schema, see 'dropDatabaseSchema'.
--   To materialize an existing schema for use in a migration, see 'existingDatabaseSchema'.
createDatabaseSchema :: BeamMigrateSchemaSqlBackend be
                     => Text
                     -> Migration be DatabaseSchema
createDatabaseSchema nm = do
  upDown (createSchemaCmd (createSchemaSyntax (schemaName nm))) Nothing
  pure $ DatabaseSchema nm

-- | Add a @DROP SCHEMA@ statement to this migration.
--
--   Depending on the backend, this may fail if the schema is not empty. 
--
--   To create a schema, see 'createDatabaseSchema'.
--   To materialize a 'DatabaseSchema', see 'existingDatabaseSchema
dropDatabaseSchema :: BeamMigrateSchemaSqlBackend be
                   => DatabaseSchema
                   -> Migration be ()
dropDatabaseSchema (DatabaseSchema nm) 
  = upDown (dropSchemaCmd (dropSchemaSyntax (schemaName nm))) Nothing

-- | Materialize a schema for use during a migration.
--
--   Example usage, where @NewDB@ has one more table than @OldDB@ in the @my_schema@ schema:
--
-- @
-- migrationStep :: 'CheckedDatabaseSettings' be OldDB
--               -> 'Migration' be ('CheckedDatabaseSettings' be NewDB)
-- migrationStep (OldDB oldtable)= do
--   schema <- 'existingDatabaseSchema' "my_schema"
--   pure $ NewDB \<$\> pure oldtable
--                \<*\> 'createTableWithSchema' (Just schema) "my_table"
-- @
existingDatabaseSchema :: Text -> Migration be DatabaseSchema
existingDatabaseSchema = pure . DatabaseSchema

-- | Add a @CREATE TABLE@ statement to this migration, with an explicit schema
--
--   The first argument is the name of the schema, while the second argument is the name of the table.
--
--   The second argument is a table containing a 'FieldSchema' for each field.
--   See documentation on the 'Field' command for more information.
--
--   Note that the database schema is expected to exist; see 'createDatabaseSchema' to create
--   a database schema.
createTableWithSchema :: ( Beamable table, Table table
                         , BeamMigrateSqlBackend be )
                      => Maybe DatabaseSchema -- ^ Schema name, if any
                      -> Text       -- ^ Table name 
                      -> TableSchema be table
                      -> Migration be (CheckedDatabaseEntity be db (TableEntity table))
createTableWithSchema maybeSchemaName newTblName tblSettings =
  do let pkFields = allBeamValues (\(Columnar' (TableFieldSchema name _ _)) -> name) (primaryKey tblSettings)
         tblConstraints =
          case NE.nonEmpty pkFields of
            Nothing  -> []
            Just pks -> [ primaryKeyConstraintSyntax pks ]
         createTableCommand =
           createTableSyntax Nothing (tableName (coerce <$> maybeSchemaName) newTblName)
                             (allBeamValues (\(Columnar' (TableFieldSchema name (FieldSchema schema) _)) -> (name, schema)) tblSettings)
                             tblConstraints
         command = createTableCmd createTableCommand

         tbl' = changeBeamRep (\(Columnar' (TableFieldSchema name _ _)) -> Columnar' (TableField (pure name) name)) tblSettings

         fieldChecks = changeBeamRep (\(Columnar' (TableFieldSchema _ _ cs)) -> Columnar' (Const cs)) tblSettings
        
         tblChecks = [ TableCheck (\tblName _ -> Just (SomeDatabasePredicate (TableExistsPredicate tblName))) ] ++
                     primaryKeyCheck

         primaryKeyCheck =
           case allBeamValues (\(Columnar' (TableFieldSchema name _ _)) -> name) (primaryKey tblSettings) of
             [] -> []
             cols -> [ TableCheck (\tblName _ -> Just (SomeDatabasePredicate (TableHasPrimaryKey tblName cols))) ]
         
         -- If a schema has been defined explicitly, then it should be part of checks
         schemaCheck = 
            case maybeSchemaName of
              Nothing -> []
              Just (DatabaseSchema sn) -> [ SomeDatabasePredicate (SchemaExistsPredicate sn) ] 

     upDown command Nothing
     pure (CheckedDatabaseEntity 
            (CheckedDatabaseTable 
              (DatabaseTable (coerce <$> maybeSchemaName) newTblName newTblName tbl') 
              tblChecks 
              fieldChecks
            ) 
            schemaCheck
          )

-- | Add a @DROP TABLE@ statement to this migration.
dropTable :: BeamMigrateSqlBackend be
          => CheckedDatabaseEntity be db (TableEntity table)
          -> Migration be ()
dropTable (CheckedDatabaseEntity (CheckedDatabaseTable dt _ _) _) =
  let command = dropTableCmd (dropTableSyntax (tableNameFromEntity dt))
  in upDown command Nothing

-- | Copy a table schema from one database to another
preserve :: CheckedDatabaseEntity be db e
         -> Migration be (CheckedDatabaseEntity be db' e)
preserve (CheckedDatabaseEntity desc checks) = pure (CheckedDatabaseEntity desc checks)

-- * Alter table

-- | A column in the process of being altered
data ColumnMigration a
  = ColumnMigration
  { columnMigrationFieldName :: Text
  , columnMigrationFieldChecks :: [FieldCheck] }

-- | Monad representing a series of @ALTER TABLE@ statements
newtype TableMigration be a
  = TableMigration (WriterT [BeamSqlBackendAlterTableSyntax be] (State (TableName, [TableCheck])) a)
  deriving (Monad, Applicative, Functor)

-- | @ALTER TABLE ... RENAME TO@ command
renameTableTo :: BeamMigrateSqlBackend be
              => Text -> table ColumnMigration
              -> TableMigration be (table ColumnMigration)
renameTableTo newName oldTbl = TableMigration $ do
  (TableName curSchema curNm, chks) <- get
  tell [ alterTableSyntax (tableName curSchema curNm) (renameTableToSyntax newName) ]
  put (TableName curSchema curNm, chks)
  return oldTbl

-- | @ALTER TABLE ... RENAME COLUMN ... TO ...@ command
renameColumnTo :: BeamMigrateSqlBackend be
               => Text -> ColumnMigration a
               -> TableMigration be (ColumnMigration a)
renameColumnTo newName column = TableMigration $ do
  (TableName curSchema curNm, _) <- get
  tell [ alterTableSyntax (tableName curSchema curNm)
           (renameColumnToSyntax (columnMigrationFieldName column) newName) ]
  pure column { columnMigrationFieldName = newName }

-- | @ALTER TABLE ... DROP COLUMN ...@ command
dropColumn :: BeamMigrateSqlBackend be
           => ColumnMigration a -> TableMigration be ()
dropColumn column = TableMigration $ do
  (TableName curSchema curNm, _)<- get
  tell [ alterTableSyntax (tableName curSchema curNm)
           (dropColumnSyntax (columnMigrationFieldName column)) ]

-- | @ALTER TABLE ... ADD COLUMN ...@ command
addColumn :: BeamMigrateSqlBackend be
          => TableFieldSchema be a
          -> TableMigration be (ColumnMigration a)
addColumn (TableFieldSchema nm (FieldSchema fieldSchemaSyntax) checks) =
  TableMigration $
  do (TableName curSchema curNm, _) <- get
     tell [ alterTableSyntax (tableName curSchema curNm) (addColumnSyntax nm fieldSchemaSyntax) ]
     pure (ColumnMigration nm checks)

-- | Compose a series of @ALTER TABLE@ commands
--
--   Example usage
--
-- @
-- migrate (OldDb oldTbl) = do
--   alterTable oldTbl $ \oldTbl' ->
--     field2 <- renameColumnTo "NewNameForField2" (_field2 oldTbl')
--     dropColumn (_field3 oldTbl')
--     renameTableTo "NewTableName"
--     field4 <- addColumn (field "ANewColumn" smallint notNull (defaultTo_ (val_ 0)))
--     return (NewTable (_field1 oldTbl') field2 field4)
-- @
--
--   The above would result in commands like:
--
-- @
-- ALTER TABLE <oldtable> RENAME COLUMN <field2> TO "NewNameForField2";
-- ALTER TABLE <oldtable> DROP COLUMN <field3>;
-- ALTER TABLE <oldtable> RENAME TO "NewTableName";
-- ALTER TABLE "NewTableName" ADD COLUMN "ANewColumn" SMALLINT NOT NULL DEFAULT 0;
-- @
--
alterTable :: forall be db db' table table'
            . (Table table', BeamMigrateSqlBackend be)
           => CheckedDatabaseEntity be db (TableEntity table)
           -> (table ColumnMigration -> TableMigration be (table' ColumnMigration))
           -> Migration be (CheckedDatabaseEntity be db' (TableEntity table'))
alterTable (CheckedDatabaseEntity (CheckedDatabaseTable dt tblChecks tblFieldChecks) entityChecks) alterColumns =
 let initialTbl = runIdentity $
                  zipBeamFieldsM
                      (\(Columnar' fd :: Columnar' (TableField table) x)
                        (Columnar' (Const checks) :: Columnar' (Const [FieldCheck]) x) ->
                         pure (Columnar' (ColumnMigration (fd ^. fieldName) checks)
                               :: Columnar' ColumnMigration x))
                      (dbTableSettings dt) tblFieldChecks

     TableMigration alterColumns' = alterColumns initialTbl
     ((newTbl, cmds), (TableName tblSchema' tblNm', tblChecks')) =
       runState (runWriterT alterColumns')
                ( TableName (dbTableSchema dt) (dbTableCurrentName dt)
                , tblChecks )

     fieldChecks' = changeBeamRep (\(Columnar' (ColumnMigration _ checks) :: Columnar' ColumnMigration a) ->
                                     Columnar' (Const checks) :: Columnar' (Const [FieldCheck]) a)
                                  newTbl

     tbl' :: TableSettings table'
     tbl' = changeBeamRep (\(Columnar' (ColumnMigration nm _) :: Columnar' ColumnMigration a) ->
                              Columnar' (TableField (pure nm) nm) :: Columnar' (TableField table') a)
                          newTbl
 in forM_ cmds (\cmd -> upDown (alterTableCmd cmd) Nothing) >>
    pure (CheckedDatabaseEntity (CheckedDatabaseTable
                                  (DatabaseTable tblSchema' (dbTableOrigName dt)
                                     tblNm' tbl')
                                   tblChecks' fieldChecks') entityChecks)

-- * Fields

-- | Build a schema for a field. This function takes the name and type of the
-- field and a variable number of modifiers, such as constraints and default
-- values. GHC will complain at you if the modifiers do not make sense. For
-- example, you cannot apply the 'notNull' constraint to a column with a 'Maybe'
-- type.
--
-- Example of creating a table named "Employee" with three columns: "FirstName",
-- "LastName", and "HireDate"
--
-- @
-- data Employee f =
--   Employee { _firstName :: C f Text
--            , _lastName  :: C f Text
--            , _hireDate  :: C f (Maybe LocalTime)
--            } deriving Generic
-- instance Beamable Employee
--
-- instance Table Employee where
--    data PrimaryKey Employee f = EmployeeKey (C f Text) (C f Text) deriving Generic
--    primaryKey = EmployeeKey \<$\> _firstName \<*\> _lastName
--
-- instance Beamable PrimaryKey Employee f
--
-- data EmployeeDb entity
--     = EmployeeDb { _employees :: entity (TableEntity Employee) }
--     deriving Generic
-- instance Database EmployeeDb
--
-- migration :: IsSql92DdlCommandSyntax syntax => Migration syntax () EmployeeDb
-- migration = do
--   employees <- createTable "EmployeesTable"
--                  (Employee (field "FirstNameField" (varchar (Just 15)) notNull)
--                            (field "last_name" (varchar Nothing) notNull (defaultTo_ (val_ "Smith")))
--                            (field "hiredDate" (maybeType timestamp)))
--   return (EmployeeDb employees)
-- @
field :: ( BeamMigrateSqlBackend be
         , FieldReturnType 'False 'False be resTy a )
      => Text -> DataType be resTy -> a
field name (DataType ty) = field' (Proxy @'False) (Proxy @'False) name ty Nothing Nothing []

-- ** Default values

-- | Represents the default value of a field with a given column schema syntax and type
newtype DefaultValue be a = DefaultValue (BeamSqlBackendExpressionSyntax be)

-- | Build a 'DefaultValue' from a 'QExpr'. GHC will complain if you supply more
-- than one default value.
defaultTo_ :: BeamMigrateSqlBackend be
           => (forall s. QExpr be s a)
           -> DefaultValue be a
defaultTo_ (QExpr e) =
  DefaultValue (e "t")

-- ** Constraints

-- | How a column constraint is reflected in the checked database.
--
-- Most constraints have no dedicated predicate and are recorded as a
-- 'TableColumnHasConstraint' holding the rendered constraint syntax. A
-- @REFERENCES@ constraint is different: backends report foreign keys as
-- 'TableHasForeignKey' when reading a live database, so recording one as a
-- 'TableColumnHasConstraint' produces a predicate that no database can ever
-- satisfy.
--
-- @since 0.6.1.0
data ConstraintCheck
  = ConstraintChecksColumn
    -- ^ Recorded as a 'TableColumnHasConstraint' on the column.
  | ConstraintChecksForeignKey Text (NE.NonEmpty Text) ForeignKeyAction ForeignKeyAction
    -- ^ Recorded as a 'TableHasForeignKey' on the containing table, naming the
    -- referenced table and columns and the @ON UPDATE@ and @ON DELETE@ actions.

-- | Represents a constraint in the given column schema syntax
data Constraint be
  = MkConstraint (BeamSqlBackendConstraintSyntax be) ConstraintCheck

-- | Build a 'Constraint' from raw constraint syntax.
--
-- The constraint is recorded as a 'TableColumnHasConstraint'. For @REFERENCES@
-- constraints prefer 'references', which records a 'TableHasForeignKey' so that
-- the predicate round-trips through a backend's @getDbConstraints@.
pattern Constraint :: BeamSqlBackendConstraintSyntax be -> Constraint be
pattern Constraint syntax <- MkConstraint syntax _ where
  Constraint syntax = MkConstraint syntax ConstraintChecksColumn
{-# COMPLETE Constraint #-}

newtype NotNullConstraint be
  = NotNullConstraint (Constraint be)

-- | The SQL92 @NOT NULL@ constraint
notNull :: BeamMigrateSqlBackend be => NotNullConstraint be
notNull = NotNullConstraint (Constraint notNullConstraintSyntax)

-- | SQL @UNIQUE@ constraint
unique :: BeamMigrateSqlBackend be => Constraint be
unique = Constraint uniqueColumnConstraintSyntax

-- | SQL @REFERENCES@ column constraint, checked as a table-level foreign key.
--
-- Emits the same @REFERENCES tbl (cols)@ column constraint that
-- @'Constraint' . 'referencesConstraintSyntax'@ does, but records it as a
-- 'TableHasForeignKey' predicate rather than a 'TableColumnHasConstraint'. That
-- is the predicate backends produce when they read foreign keys back out of a
-- live database, so a schema declared this way verifies against itself and the
-- solver can see the dependency between the two tables.
--
-- The referenced table is taken to be in the default schema, and no @MATCH@
-- clause is emitted, because 'TableHasForeignKey' can express neither. Use
-- @'Constraint' ('referencesConstraintSyntax' ...)@ if you need those, bearing
-- in mind that the resulting predicate will not round-trip.
--
-- @since 0.6.1.0
references :: forall be
            . BeamMigrateSqlBackend be
           => Text              -- ^ referenced table
           -> NE.NonEmpty Text  -- ^ referenced columns
           -> ForeignKeyAction  -- ^ @ON UPDATE@ action
           -> ForeignKeyAction  -- ^ @ON DELETE@ action
           -> Constraint be
references refTbl refCols onUpdate onDelete =
  MkConstraint
    (referencesConstraintSyntax refTbl (NE.toList refCols) Nothing
       (referentialAction onUpdate) (referentialAction onDelete))
    (ConstraintChecksForeignKey refTbl refCols onUpdate onDelete)
  where
    referentialAction :: ForeignKeyAction
                      -> Maybe (BeamSqlBackendReferentialActionSyntax be)
    referentialAction ForeignKeyNoAction = Nothing
    referentialAction ForeignKeyActionCascade = Just referentialActionCascadeSyntax
    referentialAction ForeignKeyActionSetNull = Just referentialActionSetNullSyntax
    referentialAction ForeignKeyActionSetDefault = Just referentialActionSetDefaultSyntax
    referentialAction ForeignKeyActionRestrict = Just referentialActionRestrictSyntax

-- ** 'field' variable arity classes

-- | A column constraint that has been resolved to its definition syntax,
-- paired with the check it contributes to the enclosing table.
--
-- @since 0.6.1.0
data FieldConstraint be
  = FieldConstraint (BeamSqlBackendColumnConstraintDefinitionSyntax be) ConstraintCheck

class FieldReturnType (defaultGiven :: Bool) (collationGiven :: Bool) be resTy a | a -> be resTy where
  field' :: BeamMigrateSqlBackend be
         => Proxy defaultGiven -> Proxy collationGiven
         -> Text
         -> BeamMigrateSqlBackendDataTypeSyntax be
         -> Maybe (BeamSqlBackendExpressionSyntax be)
         -> Maybe Text -> [ FieldConstraint be ]
         -> a

instance FieldReturnType 'True collationGiven be resTy a =>
  FieldReturnType 'False collationGiven be resTy (DefaultValue be resTy -> a) where
  field' _ collationGiven nm ty _ collation constraints (DefaultValue e) =
    field' (Proxy @'True) collationGiven nm ty (Just e) collation constraints

instance FieldReturnType defaultGiven collationGiven be resTy a =>
  FieldReturnType defaultGiven collationGiven be resTy (Constraint be -> a) where
  field' defaultGiven collationGiven nm ty default_' collation constraints (MkConstraint e check) =
    field' defaultGiven collationGiven nm ty default_' collation
      (constraints ++ [ FieldConstraint (constraintDefinitionSyntax Nothing e Nothing) check ])

instance ( FieldReturnType defaultGiven collationGiven be resTy (Constraint be -> a)
         , IsNotNull resTy ) =>
  FieldReturnType defaultGiven collationGiven be resTy (NotNullConstraint be -> a) where
  field' defaultGiven collationGiven nm ty default_' collation constraints (NotNullConstraint c) =
    field' defaultGiven collationGiven nm ty default_' collation constraints c

instance ( FieldReturnType 'True collationGiven be resTy a
         , TypeError ('Text "Only one DEFAULT clause can be given per 'field' invocation") ) =>
  FieldReturnType 'True collationGiven be resTy (DefaultValue be resTy -> a) where

  field' = error "Unreachable because of GHC Custom Type Errors"

instance ( FieldReturnType defaultGiven collationGiven be resTy a
         , TypeError ('Text "Only one type declaration allowed per 'field' invocation")) =>
  FieldReturnType defaultGiven collationGiven be resTy (DataType be' x -> a) where
  field' = error "Unreachable because of GHC Custom Type Errors"

instance ( BeamMigrateSqlBackend be, HasDataTypeCreatedCheck (BeamMigrateSqlBackendDataTypeSyntax be) ) =>
  FieldReturnType defaultGiven collationGiven be resTy (TableFieldSchema be resTy) where
  field' _ _ nm ty default_' collation constraints =
    TableFieldSchema nm (FieldSchema (columnSchemaSyntax ty default_' constraintSyntaxes collation)) checks
    where constraintSyntaxes = map (\(FieldConstraint cns _) -> cns) constraints

          checks = FieldCheck (\tbl field'' -> SomeDatabasePredicate (TableHasColumn tbl field'' ty :: TableHasColumn be))
                 : map constraintCheck constraints

          constraintCheck (FieldConstraint cns ConstraintChecksColumn) =
            FieldCheck $ \tbl field'' ->
              SomeDatabasePredicate (TableColumnHasConstraint tbl field'' cns :: TableColumnHasConstraint be)
          constraintCheck (FieldConstraint _ (ConstraintChecksForeignKey refTbl refCols onUpdate onDelete)) =
            FieldCheck $ \tbl field'' ->
              SomeDatabasePredicate
                (TableHasForeignKey tbl (field'' NE.:| []) (QualifiedName Nothing refTbl)
                                    refCols onUpdate onDelete)

type family IsNotNull (x :: Type) :: Kind.Constraint where
  IsNotNull (Maybe x) = TypeError ('Text "You used Database.Beam.Migrate.notNull on a column with type" ':$$:
                                   'ShowType (Maybe x) ':$$:
                                   'Text "Either remove 'notNull' from your migration or 'Maybe' from your table")
  IsNotNull x = ()
