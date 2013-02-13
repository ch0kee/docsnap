module Internal.Types 
  (Edit)
where


type Index = Int
type Length = Int
type Content = String
data Edit = Insert (Index, Content)
--        | Modify (Index, Content) egyelore emulaljuk insert,remove parral
          | Remove (Index, Length) 
          
