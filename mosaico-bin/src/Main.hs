module Main (main) where

import Graphics.Mosaico.Diagrama (Diagrama((:-:), (:|:), Hoja), Paso(Primero, Segundo))
import Graphics.Mosaico.Imagen   (Imagen(Imagen, altura, anchura, datos), leerImagen)
import Graphics.Mosaico.Ventana  (Ventana, cerrar, crearVentana, leerTecla, mostrar)
import Diagramas (Orientación(Horizontal, Vertical), caminar, dividir, rectánguloImagen, sustituir)
import System.Environment
import Data.Maybe (fromJust,isNothing)


ciclo :: Ventana -> Diagrama -> [Paso] -> IO ()
ciclo vent diag paso = do 
						mostrar vent paso diag
						tecla <- leerTecla vent
						if tecla == Nothing then ciclo vent diag paso
						else case (fromJust tecla) of 
							"Down" -> do
										if (isNothing destino) then cerrar vent
										else
											case (fromJust destino) of 
												(Hoja rec) -> do    
													if (isNothing diagTempHori) then ciclo vent diag paso
													else
														if (isNothing diagSustituidoH)  then ciclo vent diag paso
														else ciclo vent  (fromJust diagSustituidoH) (paso++[Segundo])
													where
														diagTempHori  = dividir Horizontal rec 
														diagSustituidoH  =  sustituir (fromJust diagTempHori) paso diag
												(_ :-: _) -> ciclo vent diag (paso++[Segundo])
												(_ :|: _) -> ciclo vent diag paso
							"Up" -> do
										if (isNothing destino) then cerrar vent
										else
											case (fromJust destino) of 
												(Hoja rec) -> do    
													if (isNothing diagTempHori) then ciclo vent diag paso
													else
														if (isNothing diagSustituidoH)  then ciclo vent diag paso
														else ciclo vent  (fromJust diagSustituidoH) (paso++[Primero])
													where
														diagTempHori  = dividir Horizontal rec 
														diagSustituidoH  =  sustituir (fromJust diagTempHori) paso diag
												(_ :-: _) -> ciclo vent diag (paso++[Primero])
												(_ :|: _) -> ciclo vent diag paso
							"Left" -> do
										if (isNothing destino) then cerrar vent
										else
											case (fromJust destino) of 
												(Hoja rec) -> do    
													if (isNothing diagTempVert) then ciclo vent diag paso
													else
														if (isNothing diagSustituidoV)  then ciclo vent diag paso
														else ciclo vent  (fromJust diagSustituidoV) (paso++[Primero])
													where
														diagTempVert = dividir Vertical rec
														diagSustituidoV  =  sustituir (fromJust diagTempVert) paso diag
												(_ :|: _) -> ciclo vent diag (paso++[Primero])
												(_ :-: _) -> ciclo vent diag paso
							"Right" -> do
										if (isNothing destino) then cerrar vent
										else
											case (fromJust destino) of 
												(Hoja rec) -> do    
													if (isNothing diagTempVert) then ciclo vent diag paso
													else
														if (isNothing diagSustituidoV)  then ciclo vent diag paso
														else ciclo vent  (fromJust diagSustituidoV) (paso++[Segundo])
													where
														diagTempVert = dividir Vertical rec 
														diagSustituidoV  =  sustituir (fromJust diagTempVert) paso diag
												(_ :|: _) -> ciclo vent diag (paso++[Segundo])
												(_ :-: _) -> ciclo vent diag paso

							"BackSpace" -> if (null paso) then ciclo vent diag paso
										   else ciclo vent diag (init paso) 
							"q" -> cerrar vent
							_ -> ciclo vent diag paso
							where
								destino = caminar paso diag


main :: IO ()
main = do
	args <- getArgs
	case args of 
		(archivo:[]) -> do
			imagen <- leerImagen archivo
			case imagen of 
				Right img@(Imagen x y _)  -> do
							vent <- crearVentana (x) (y);
							(ciclo vent (Hoja (rectánguloImagen img)) []);
				Left mensaje -> putStrLn mensaje
		_ -> do
			nombre <- getProgName
			putStrLn $ "Uso: " ++ nombre ++ " path_imagen"



