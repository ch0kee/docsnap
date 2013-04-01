module Internal.Types
  (Content, Length, Edit(..), Version, Revision(..), Response(..))
where


type Content = String
type Length = Int
data Edit = Insert Char | Preserve | Remove
  deriving (Show)

type Version = Int
data Revision = Revision ([Edit], Version)
  deriving (Show)

data Response  = CommitSuccessful Version
              | CheckoutOnly Revision
  deriving (Show)
