module
  Diagramas
  ( rectánguloImagen
  , Orientación(Horizontal, Vertical)
  , dividir
  , caminar
  , sustituir
  , posible
  )
  where

import Graphics.Mosaico.Diagrama (Diagrama((:-:), (:|:), Hoja), Paso(Primero, Segundo), Rectángulo(Rectángulo, color, imagen))
import Graphics.Mosaico.Imagen   (Imagen(Imagen, altura, anchura, datos))

import Imagen (colorPromedio, hSplit, vSplit)

rectánguloImagen :: Imagen -> Rectángulo
rectánguloImagen img = Rectángulo (colorPromedio img) img 

data Orientación
  = Horizontal
  | Vertical
  deriving Show

dividir :: Orientación -> Rectángulo -> Maybe Diagrama
dividir o (Rectángulo _ img@(Imagen x y _) ) 
  = case o of Horizontal -> if y < 2 then Nothing
                            else  Just((Hoja( rectánguloImagen (fst corte) )):-:(Hoja( rectánguloImagen (snd corte) )) )
                              where corte = hSplit img 
              Vertical ->   if x < 2 then Nothing
                            else  Just ((Hoja( rectánguloImagen (fst corte) )):|:(Hoja( rectánguloImagen (snd corte) )) )
                              where corte = vSplit img 


caminar :: [Paso] -> Diagrama -> Maybe Diagrama
caminar [] diagram = Just diagram
caminar (x:xs) (primero :-: segundo ) = case x of Primero -> caminar xs primero
                                                  Segundo -> caminar xs segundo
caminar (x:xs) (primero :|: segundo ) = case x of Primero -> caminar xs primero
                                                  Segundo -> caminar xs segundo
caminar (_:_) (Hoja _ ) = Nothing                                                           


sustituir :: Diagrama -> [Paso] -> Diagrama -> Maybe Diagrama
sustituir diagramNuev paso diagramOrig = 
  if posible paso diagramOrig then Just ( sustituir' diagramNuev paso diagramOrig) 
  else Nothing
    where

      sustituir' diagramNuev [] diagramOrig = diagramNuev
      sustituir' diagramNuev (p:ps) diagramOrig@(primero :-: segundo) = case p of Primero -> ((sustituir' diagramNuev ps primero) :-: segundo) 
                                                                                  Segundo -> (primero :-: (sustituir' diagramNuev ps segundo))
      sustituir' diagramNuev (p:ps) diagramOrig@(primero :|: segundo) = case p of Primero -> ((sustituir' diagramNuev ps primero) :|: segundo) 
                                                                                  Segundo -> (primero :|: (sustituir' diagramNuev ps segundo))
      sustituir' diagramNuev  _ (Hoja _) = diagramNuev


posible :: [Paso] -> Diagrama -> Bool
posible [] _ = True
posible (x:xs) (primero :-: segundo ) = case x of Primero -> posible xs primero
                                                  Segundo -> posible xs segundo
posible (x:xs) (primero :|: segundo ) = case x of Primero -> posible xs primero
                                                  Segundo -> posible xs segundo
posible (_:_) (Hoja _ ) = False 