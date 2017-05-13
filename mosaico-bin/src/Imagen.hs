module Imagen
  ( hSplit, vSplit
  , colorPromedio
  )
  where

import Graphics.Mosaico.Imagen (Color(Color, rojo, verde, azul), Imagen(Imagen, altura, anchura, datos))



subImagen
  :: Integer -> Integer
  -> Integer -> Integer
  -> Imagen -> Imagen

subImagen x y x' y' (Imagen a h lista) =  Imagen x' y' (map (take (fromIntegral x')) (map (drop (fromIntegral x)) (take (fromIntegral y') (drop (fromIntegral y) lista))))


hSplit :: Imagen -> (Imagen, Imagen)
hSplit img@(Imagen x y lista) = ((subImagen 0 0 x (y `div` 2) img),  (subImagen 0 (y `div` 2) x (y-(y `div` 2)) img) )

vSplit :: Imagen -> (Imagen, Imagen)
vSplit img@(Imagen x y lista) = ((subImagen 0 0 (x `div` 2) y img),  (subImagen (x `div` 2) 0 (x-(x `div` 2)) y img) )

colorPromedio :: Imagen -> Color
colorPromedio (Imagen anchura altura f) = Color (fromIntegral (promedioRojo f) ) (fromIntegral (promedioVerde f)) (fromIntegral (promedioAzul f))
	where 
		promedioRojo z  = ((sumatoriaLineas z "rojo") `div` (anchura*altura))
		promedioAzul z  = ((sumatoriaLineas z "verde") `div` (anchura*altura))
		promedioVerde z = ((sumatoriaLineas z "azul") `div` (anchura*altura))

sumatoriaLineas :: [[Color]]-> String -> Integer
sumatoriaLineas [] _ = 0
sumatoriaLineas (x:xs) "rojo" = (sumatoriaRojo x) + (sumatoriaLineas xs "rojo")
sumatoriaLineas (x:xs) "verde" = (sumatoriaVerde x) + (sumatoriaLineas xs "verde")
sumatoriaLineas (x:xs) "azul" = (sumatoriaAzul x) + (sumatoriaLineas xs "azul")
sumatoriaLineas _ _ = 0



sumatoriaRojo :: [Color] -> Integer 
sumatoriaRojo (Color r _ _:cs) = (toInteger r) + sumatoriaRojo cs
sumatoriaRojo [] = 0;

sumatoriaVerde :: [Color] -> Integer 
sumatoriaVerde (Color _ _ v:cs) = (toInteger v) + sumatoriaVerde cs
sumatoriaVerde [] = 0;

sumatoriaAzul :: [Color] -> Integer 
sumatoriaAzul (Color _ a _:cs) = (toInteger a) + sumatoriaAzul cs
sumatoriaAzul [] = 0;
