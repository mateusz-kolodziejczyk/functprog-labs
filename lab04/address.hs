--Based on Get Programming with Haskell, Will Kurt.

data  Location = Wit | Itc | Oth | Ucd | Tcd
type Name = (String, String)

office :: Location -> String
office Wit = "Waterford institute of Technology, Cork Road, Waterford, Ireland, X91 K0EK."
office Itc  = "Carlow Institute of Technology, Dublin Road, Carlow, Ireland, R93 V960."
office Oth = "OTH Regensburg, Seybothstraße 2, 93053 Regensburg, Germany."
office Ucd = "University College Dublin, Belfield, Dublin 4, Ireland, D04 V1W8"
office Tcd = "Trinity College Dublin, the University of Dublin College Green Dublin 2,Ireland, D02 PN40 "


tcdOffice :: Name -> String
tcdOffice name =
    nametext ++ ", " ++ office Tcd
  where
    nametext = snd name

ucdOffice :: Name -> String
ucdOffice name =
    nametext ++ ", " ++ office Ucd
  where
    nametext = fst name ++ "  " ++ snd name ++ "  Esq"

witOffice :: Name -> String
witOffice name =
  if lastName < "L"
    then nametext ++ ",  Lower Floor, Main Building  " ++ office Wit
    else nametext ++ ",  Top Floor, Main Building  " ++ office Wit
  where
    nametext = fst name ++ "  " ++ snd name
    lastName = snd name

itcOffice :: Name -> String
itcOffice name =   nametext ++ ",  " ++ office Itc
  where
    nametext = fst name ++ "  " ++ snd name

othOffice :: Name -> String
othOffice name =   nametext ++ ",  " ++ office Oth
  where
    nametext = snd name ++ ", " ++ fst name

-- getLocation :: String -> Name -> String -- the next version is better
-- getLocation location name  = case location  of 
--                 "wit" -> witOffice name
--                 "itc" -> itcOffice name
--                 "oth" -> othOffice name
--                 _     -> fst name ++ "  " ++ snd name ++ ": Address unknown"

getLocation :: String -> Name -> String
getLocation location = case location  of 
                "wit" -> witOffice 
                "itc" -> itcOffice 
                "oth" -> othOffice 
                "tcd" -> tcdOffice
                "ucd" -> ucdOffice
                _     -> (\name -> fst name ++ "  " ++ snd name ++ ": Address unknown" ) 

addressLetter :: Name -> String -> String
addressLetter name location = getLocation location name