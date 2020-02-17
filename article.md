# Records, pattern matching et control flow en clojure

## Le problème 

Peut être pas vraiment un problème
Absence de pattern matching dans clojure.core
Absence de support pour le matching des records dans core.match
Boilerplate code lors de la définition de records 
Pas de mecanisme de validation pour les champs des records 

Le controle flow basé sur des prédicats amène à du code verbeux
L'accès et la transformation de valeurs nichées peut être verbeux et/ou peinible

## Solutions existantes 

validation: spec, plumatic/schema
pattern matching: core.match
control flow: _
accès, transformation: specter

## Proposition

extension de core.match 
- supporter le matching et la destructuration des records utilisateur
- utiliser des specs pour valider 
- syntax sugar pour matcher via prédicat  

nouvelle forme de definition de record 
- permettant la validation des champs via clojure.spec
- generant une spec permettant la validation et la génération 
- definition de fonctions d'instanciation (validées) et d'un prédicat 

nouvelle forme pour les fonctions mattern-matchées

mecanisme d'accès et transformation via une implementation de Lens

mecanisme de control flow