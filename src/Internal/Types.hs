module Internal.Types
  (Content, Length, Edit(..), Version, Revision(..), Response(..), PackedEdit(..), PackedRevision(..))
where


type Content = String
type Length = Int
type Count = Int
data Edit = Insert Char | Preserve | Remove
  deriving (Show)

type Version = Int
newtype Revision = Revision ([Edit], Version)
  deriving (Show)

data PackedEdit = Inserts String | Preserves Count | Removes Count
  deriving (Show)
newtype PackedRevision = PackedRevision ([PackedEdit], Version)
  deriving (Show)

data Response  = CommitSuccessful Version
               | CheckoutOnly Revision
               | NoChanges
  deriving (Show)
