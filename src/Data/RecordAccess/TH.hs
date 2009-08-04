{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies #-}
 
module Data.RecordAccess.TH (
    record,
    subrecord
) where

import Prelude hiding ((.))
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Control.Monad (forM)
import Data.Maybe (catMaybes)
import Data.Char (toUpper, toLower)

import Data.RecordAccess

record :: Q [Dec] -> Q [Dec]
record qds = do
    ds <- qds
    case ds of
      [DataD ctx recName [] [RecC recConName fields] derives] -> concatSequence 
        [ genLabels fields
        , genRecType recName recConName fields derives
        , genLabelInstances recName fields
        , genRecClassAndInstance recName fields
        ]
      _ -> report True "Record declaration expected" >> return []
      
subrecord :: Q [Dec] -> Q [Dec]
subrecord qds = do
    ds <- qds
    case ds of
      [DataD ctx recName [] [RecC recConName fields] []] -> genRecClassAndInstance recName fields
      _ -> report True "Record declaration expected" >> return []
      
genLabels :: [VarStrictType] -> Q [Dec]
genLabels fields = 
    fmap catMaybes $ forM fields $ \(n,_,_) -> 
      (reify n >> return Nothing) `onFailure` (genLabel n >>= return . Just)
      
genLabel :: Name -> DecQ
genLabel n = 
    -- class Foo s a | s -> a where { foo :: (Ref r) => r s a; }
    return $ ClassD [] (update nameString capitalize n) [s,a] [FunDep [s] [a]] [SigD n
      (ForallT [r] [AppT (ConT ''Ref) (VarT r)] (AppT (AppT (VarT r) (VarT s)) (VarT a)))]
    where
      s = mkName "s"
      a = mkName "a"
      r = mkName "r"

genRecType :: Name -> Name -> [VarStrictType] -> [Name] -> Q [Dec]
genRecType recClassName recConName fields derives = 
    return [DataD [] recTypeName [] [RecC recConName fields'] derives]
    where
      recTypeName               = suffixRecordName recClassName
      fields'                   = map prefixNameInField fields
      prefixNameInField (n,s,t) = (prefixFieldName recClassName n, s, t)
      
genLabelInstances :: Name -> [VarStrictType] -> Q [Dec]
genLabelInstances recClassName fields = 
    forM fields $ \(n,_,t) -> genLabelInstance recClassName n t

genLabelInstance :: Name -> Name -> Type -> DecQ
genLabelInstance recClassName n t =
    -- instance Label Rec ValType where
    --   label = ref rec_label (\a s -> s { rec_label = a })
    return $ InstanceD [] instanceT [methodD]
    where
      instanceT      = AppT (AppT (ConT labelClassName) (ConT recTypeName)) t
      methodD        = ValD (VarP n) (NormalB body) []
      body           = AppE (AppE (VarE 'ref) getE) putE
      getE           = VarE fieldName
      putE           = LamE [VarP a, VarP s] (RecUpdE (VarE s) [(fieldName,VarE a)])
      fieldName      = prefixFieldName recClassName n
      labelClassName = update nameString capitalize n
      recTypeName    = suffixRecordName recClassName
      s              = mkName "s"
      a              = mkName "a"
      
genRecClassAndInstance :: Name -> [VarStrictType] -> Q [Dec]
genRecClassAndInstance recClassName fields = 
    -- class    (Field1 s VT1, Field2 s VT2) => RecClass s
    -- instance (Field1 s VT1, Field2 s VT2) => RecClass s
    return 
      [ ClassD    cxt recClassName [s] [] []
      , InstanceD cxt instanceT           []
      ]
    where
      instanceT              = AppT (ConT recClassName) (VarT s)
      cxt                    = map fieldToContext fields
      fieldToContext (n,_,t) = AppT (AppT (ConT (update nameString capitalize n)) (VarT s)) t
      s                      = mkName "s"


suffixRecordName = update nameString (++ "Rec")

prefixFieldName recName = update nameString $ ((uncapitalize . nameBase) recName ++) . ('_':)

nameString = ref nameBase (const .mkName)
      
      
concatSequence = fmap concat . sequence

capitalize = update listHead toUpper

uncapitalize = update listHead toLower

listHead = ref head (\h (_:t) -> h:t)

onFailure = flip recover 
