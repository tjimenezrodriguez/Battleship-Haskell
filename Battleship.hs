-- Realizado por Teófilo Jiménez Rodríguez y Álex Carrillo Delgado

-- Importamos el módulo que genera aleatoriedad

import System.Random

-- Definimos tipos de datos
-- Primero definimos los datos asociados a "estados"

data Pos = A | B | Hit | Miss  --A y B son los estados de agua y barco respectivamente. Hit para cuando aciertas a un barco y Miss para cuando falla al agua.
   deriving (Eq, Show, Read)
   
type Tab = [[Pos]] -- Un tablero es una matriz de estados

-- Ahora los demás datos 

type Coord = (Int, Int)  -- Una coordenada está formada por una tupla de dos enteros.
type Barco = [Coord]       -- OBS: no es lo mismo que el "estado" B
type Jug = (Tab, [Barco]) 
-- IMPORTANTE : El tablero es aquel donde el jugador dispara y la lista de barcos es la de aquellos colocados en él. En un jugador está la información que "le hace ganar" si lo hace antes que el otro jugador. Es "autocontenido".
type Juego = (Jug, Jug)  -- El tipo Juego es una tupla formada por la información de los dos jugadores de la partida.
type Tam = Int -- Definimos también tamaño para legibilidad posterior

data Orient = Vert | Hor -- Este tipo de datos servirá para la generación de barcos. Indica si el barco está en vertical u horizontal.
   deriving (Eq, Show, Read)
    
-- Coloca una lista de barcos en un tablero (actualiza su estado).
colBarcos :: [Barco] -> Tab -> Tab
colBarcos barcos tab = foldr (\coord nuevoTab -> actTab B coord nuevoTab) tab (concat barcos)

-- Genera un tablero vacío con el tamaño que deseemos.
tabVacio :: Tam -> Tab
tabVacio n = replicate n (replicate n A)

-- Actualizaciones de tablero:

-- Función principal para la actualización.
disparo :: Coord -> Jug -> Jug   
disparo (x, y) (tab, barcos) =
  case tab !! (y - 1) !! (x - 1) of
    A -> (actTab Miss (x, y) tab, barcos)
    B -> (actTab Hit (x, y) tab, actBarco (x, y) barcos)
    _ -> (tab, barcos)  -- Ya fue disparado en esta posición, no ocurre nada

-- Auxiliar de disparo para actualizar el tablero en cada disparo.
actTab :: Pos -> Coord -> Tab -> Tab
actTab nEstado (x, y) tab =
  take (y - 1) tab ++ [nFila] ++ drop y tab
  where
    fila = tab !! (y - 1)
    nFila = take (x - 1) fila ++ [nEstado] ++ drop x fila

-- Auxiliar de disparo para actualizar el barco en cada disparo.
actBarco :: Coord -> [Barco] -> [Barco]
actBarco (x, y) barcos =
  map (\b -> guardaElem b) barcos
  where
    guardaElem b
      | (x, y) `elem` b = quitCoord (x, y) b
      | otherwise = b

-- Auxiliar de actBarco para quitar a un barco la coordenada específica.
quitCoord :: Coord -> Barco -> Barco   
quitCoord _ [] = []
quitCoord coord (c:cs)
  | coord == c = cs
  | otherwise = c : quitCoord coord cs
  
-- Actualiza la lista de barcos eliminando aquellos que estén hundidos.
actlBarcos :: Jug -> Jug
actlBarcos (tab, barcos) = (tab, filter (\barco -> not (bHundido barco tab)) barcos)

-- Auxiliar de actlBarcos para verifica si un barco está completamente hundido.
bHundido :: Barco -> Tab -> Bool  
bHundido barco tab = all (\(x, y) -> coordVal (x, y) tab && tab !! (y - 1) !! (x - 1) == Hit) barco 

-- Visualización del tablero: 

-- El parámetro de entrada Int, se debe a que necesitamos dos funciones de visualización, para el jugador (i=1) y para la máquina (i=2). Se arrastra sin efecto hasta visPos.
visTab :: Tab -> Int -> IO ()
visTab tab i = do
  putStrLn $ ['-' | _ <- [1 .. 4 * tam + 1]] -- Para generar una línea superior del tablero
  mapM_ (\fila -> visF fila i) tab             -- Lo aplicamos a cada fila
  putStrLn $ ['-' | _ <- [1 .. 4 * tam + 1]] -- Para la línea inferior del tablero
     where tam = length tab

-- Auxiliar de visTab para crear cada fila.
visF :: [Pos] -> Int -> IO ()
visF fila i = do
  putChar '|'
  mapM_ (\pos -> visPos pos i) fila
  putStrLn "|" -- Fin de la fila

-- i=1 es donde el jugador dispara, i=2 donde lo hace la máquina.

-- Auxiliar de visF para indicar el estado de cada casilla.
visPos :: Pos -> Int -> IO ()
visPos A _ = putStr " . "
visPos Hit _ = putStr " X "
visPos Miss _ = putStr " O "
visPos B 1 = putStr " . "
visPos B 2 = putStr " B "


-- Generación (aleatoria) de barcos:

-- Esta es la función principal, que devuelve una lista de barcos (cuya composición arbitrariamente hemos considerado adecuada) según el tamaño del tablero.
lBarcos :: Tam -> IO [Barco]
lBarcos tTab = sequence [creaBarco t tTab | t <- tams]
  where
    tams
      | tTab == 5 = [2, 2, 3]
      | tTab >= 6 && tTab <= 8 = [2, 3, 4, 5]
      | tTab >= 9 && tTab <= 10 = [2, 3, 3, 4, 5]
      | tTab >= 11 && tTab <= 14 = [2, 3, 3, 3, 4, 4, 5]
      | tTab == 15 = [2, 3, 3, 3, 4, 4, 4, 5, 5]
      | otherwise = error "Tamaño de tablero incorrecto"

-- Función auxiliar que crea un barco válido dado un tamaño para el barco y otro para el tablero.
creaBarco :: Tam -> Tam -> IO Barco
creaBarco tBarco tTab = do
  gen1 <- newStdGen  -- Toma un generador de números aleatorios
  let (x, gen2) = randomR (1, tTab - tBarco + 1) gen1   -- randomR toma un número al azar del rango que se le indique
      (y, gen3) = randomR (1, tTab - tBarco + 1) gen2
      (orientNum, gen4) = randomR (1 :: Int, 2 :: Int) gen3
      orient
        | orientNum == 1 = Vert
        | otherwise = Hor
      barco = case orient of
        Vert -> zip [x..(x + tBarco - 1)] (repeat y)
        Hor  -> zip (repeat x) [y..(y + tBarco - 1)]
  return barco

-- Auxiliar que verifica que si lista de barcos es correcta (es decir, no hay superposiciones de coordenadas; ninguna dupla igual).
eslbVal :: [Barco] -> Bool
eslbVal matriz = sinDup (concat matriz) == concat matriz

-- Auxiliar de eslbVal para filtrar de una lista y quitar los elementos duplicados.
sinDup :: Eq a => [(a, a)] -> [(a, a)]
sinDup [] = []
sinDup (x:xs) = x : sinDup (filter (/= x) xs)

-- Lógica máquina 
--Siempre devuelve un Agua(A) o Barco(B)
  --Si no hay Hit, devuelve con A. Esto lo llamamos coordenada tipo 1 (función tcoord1)
  --Si hay un sólo Hit, devuelve adyacente al Hit. Esto lo llamamos coordenada tipo 2 (función tcoord2)
  --Si hay dos o más Hits consecutivos, devuelve una de las coordenadas siguientes de un barco. Esto lo llamamos coordenada tipo 3 (función tcoord3)
  -- Como cada vez son más restrictivos, si estando en tcoord3 no se cumple, y por tanto no se devolvería nada, al final de la recursión se llama a tcoord2. Si en tcoord2 tampoco resultase haber candidatos, vamos al caso tcoord1. Es una "desescalada" progresiva.
  -- OBS: por la generación de barcos (se coge un punto posible, que tiende a "nacer" en el centro y luego se extiende), los barcos suelen apelotonarse. Debido a esto, la IA hace mejores jugadas implementada de esta forma (con la "desescalada progresiva"), que pasando del caso 3 al 1 directamente.

-- En esta función entra toda esa lógica para ver qué coordenada devolvemos.
maq :: Tab -> IO Coord
maq tab
  | lh == 0 = tcoord1 tab
  | lh == 1 = tcoord2 (head hits) tab
  | otherwise = tcoord3 hits tab
  where
    lh = length hits
    hits = obtHits tab

-- Definimos varias funciones auxiliares:

-- Verifica que una posición es agua o barco.
esAB :: Coord -> Tab -> Bool
esAB (x, y) tab 
  | pos == A = True
  | pos == B = True
  | otherwise = False
  where
    pos = tab !! (y - 1) !! (x - 1)

-- Dado un tablero devuelve su lista de Hits.
obtHits :: Tab -> [Coord]
obtHits tab =
  [(x, y) | x <- [1..tam], y <- [1..tam], tab !! (y - 1) !! (x - 1) == Hit]
  where
    tam = length tab

-- Función que comprueba si una coordenada está dentro de los límites del tablero.
coordVal :: Coord -> Tab -> Bool
coordVal (x, y) tab = x >= 1 && x <= length tab && y >= 1 && y <= length (head tab)

-- TIPO 1-- Genera de forma aleatoria una coordenada en el tablero. Luego, verifica que la coordenada sea válida. Si la coordenada generada no cumple con estas condiciones, se vuelve a llamar recursivamente a la función hasta obtener una coordenada válida. 
tcoord1 :: Tab -> IO Coord
tcoord1 tab = do
  x <- randomRIO (1, tam)  
  y <- randomRIO (1, tam)
  let c = (x, y)
  case (coordVal c tab, esAB c tab) of
    (True, True) -> return c
    _            -> tcoord1 tab
  where
    tam = length tab

-- TIPO 2-- Dada una coord hit, cojo las adyacentes, y de las que sean agua cojo una al azar (siempre va a haber por cómo están los casos definidos).
tcoord2 :: Coord -> Tab -> IO Coord
tcoord2 (x, y) tab = do
  let ady = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)] -- Cogemos las adyacentes
      cand = filter (\c -> coordVal c tab && esAB c tab) ady  -- Filtramos y sacamos candidatas
  if null cand
    then tcoord1 tab
    else do
      i <- randomRIO (0, length cand - 1) -- Cogemos un número aleatorio
      return $ cand !! i

-- TIPO 3-- Dada una lista de coord de hits, devuelve el candidato que verifica las condiciones (suponemos que en la lista hay más de un elemento, pues ha tenido que pasar los filtros anteriores).
tcoord3 :: [Coord] -> Tab -> IO Coord
tcoord3 [cd] tab = tcoord2 cd tab
tcoord3 (c1:c2:cs) tab
  | coordVal (fst c) tab && esAB (fst c) tab  = return (fst c)
  | coordVal (snd c) tab && esAB (snd c) tab  = return (snd c)               
  | otherwise = tcoord3 (c2:cs) tab                         
     where c = coordSig c1 c2
    
-- Auxiliar que verifica si dos coordenadas son consecutivas
sonCons :: Coord -> Coord -> Bool
sonCons (x1, y1) (x2, y2) = abs (x1 - x2) == 1 && abs (y1 - y2) == 0 || abs (y1 - y2) == 1 && abs (x1 - x2) == 0

-- Auxiliar que calcula las "siguientes" (anterior y posterior) coordenadas dadas dos.
coordSig :: Coord -> Coord -> (Coord, Coord)
coordSig (x1, y1) (x2, y2)
  | x1 < x2 = ((x2 + 1, y2), (x1 - 1, y1))
  | x1 > x2 = ((x1 + 1, y1), (x2 - 1, y2))
  | y1 < y2 = ((x2, y2 + 1), (x1, y1 - 1))
  | y1 > y2 = ((x1, y1 + 1), (x2, y2 - 1))
  | otherwise = error "Las coordenadas no son consecutivas."

-- Funciones para ensamblar todas las lógicas anteriores e interactuar con el jugador:
  
-- Muestra un mensaje indicando cuántos barcos quedan en el juego
barcRest :: [Barco] -> IO ()
barcRest barcos = do
  putStrLn "Barcos restantes:"
  mapM_ (\barco -> putStrLn $ "  - Un barco con " ++ show (length barco) ++ " casillas sin hundir.") barcos 

-- Verifica si todos los barcos que tenía que hundir un jugador para ganar lo están.
jugGana :: Tab -> Bool
jugGana tab = not (any esB (concat tab))
  where
    esB :: Pos -> Bool
    esB B = True
    esB _ = False

-- Maneja el turno del jugador.
turnoJ :: Jug -> IO Jug
turnoJ j = do
  putStrLn "Tu tablero de disparos:"
  visTab (fst j) 1
  putStrLn "Ingresa las coordenadas a disparar (por ejemplo: 3 4): "
  input <- getLine
  let [x, y] = map read (words input)
  let coord = (x, y)
  if coordVal coord (fst j)
    then do
      let nuevoJug = disparo coord j
      let jugAct = actlBarcos nuevoJug
      putStrLn "------------------------------------------------------------¡TURNO DE LOS DISPAROS!------------------------------------------------------------"

      -- Verificar si algún barco está hundido. Para ello veo si se ha actualizado la lista de barcos o no.
      if (snd jugAct) /= (snd nuevoJug)
        then do
          putStrLn "¡Has hundido un barco!"
          barcRest (snd jugAct)  -- Muestra los barcos restantes del oponente
        else return ()
      return jugAct
    else do
      putStrLn "Coordenadas no válidas. Inténtalo de nuevo."
      turnoJ j

-- Maneja el turno de la máquina   -- En esta función no hay casi mensajes para dejar la pantalla limpia.
turnoM :: Jug -> IO Jug
turnoM j= do
  coord <- maq (fst j)
  putStrLn $ "La máquina disparó a las coordenadas: " ++ show coord
  let nJ = disparo coord j
  let jAct = actlBarcos nJ
  visTab (fst nJ) 2
  if (snd jAct) /= (snd nJ)
        then do
          putStrLn "¡Ciao barco!"
        else return ()
  return jAct

-- Maneja la lógica del juego llamando al jugador y a la máquina.
turnos :: Juego -> IO ()
turnos juego@(j1, j2) = do
  nj1 <- turnoJ j1
  if jugGana (fst nj1)
    then putStrLn "¡Has ganado!"
    else do
      putStrLn "Pulsa g para guardar (y cerrar) la partida"
      respuestaGuardar <- getLine
      if respuestaGuardar == "g"
        then guardarPartida (nj1, j2) 
        else do
          nj2 <- turnoM j2
          if jugGana (fst nj2)
            then putStrLn "¡La máquina ha ganado!"
            else turnos (nj1, nj2)
    
-- Crea un juego con dos jugadores.
creaJuego :: Tam -> IO Juego
creaJuego tam = do
  usuario <- creaJug tam
  maquina <- creaJug tam
  return (usuario, maquina)

-- Crea el tablero de cada jugador.
creaJug :: Tam -> IO Jug
creaJug tam = do
  barcos <- lBarcos tam
  let tabInic = colBarcos barcos (tabVacio tam)
  case eslbVal barcos of
    True  -> return (tabInic, barcos)
    False -> creaJug tam

-- Guarda una partida.
guardarPartida :: Juego -> IO ()
guardarPartida juego = do
  putStrLn "Escribe el nombre del archivo que guardará tu partida:"
  nombreArchivo <- getLine
  writeFile nombreArchivo (show juego)
  putStrLn $ "Partida guardada en el archivo '" ++ nombreArchivo ++ "'."

-- Carga una partida.
cargarPartida :: IO Juego
cargarPartida = do
  putStrLn "Escribe el nombre del archivo donde está la partida que deseas guardar:"
  nombreArchivo <- getLine
  contenido <- readFile nombreArchivo
  let juego = read contenido :: Juego
  putStrLn $ "Partida cargada desde el archivo '" ++ nombreArchivo ++ "'."
  return juego

-- Función principal del juego.
play :: IO ()
play = do
      putStrLn "¡Bienvenido al juego Hundir la Flota!"
      putStrLn "¿Quieres cargar una partida ya existente? (c)"
      respuesta <- getLine
      if respuesta == "c"
        then cargarPartida >>= turnos
        else do
          putStrLn "Elige el tamaño del tablero (entre 5 y 15):"
          tamStr <- getLine
          let tam = read tamStr :: Int
          if tam >= 5 && tam <= 15
            then do
              njuego <- creaJuego tam
              let j1 = fst njuego
                  j2 = snd njuego
                  b1 = snd j2
              putStrLn "Tu flota estará compuesta por los siguientes barcos:"
              mapM_ (\barco -> putStrLn $ "  - Barco de tamaño " ++ show (length barco) ++ " en " ++ show barco) b1
              putStrLn "Este es el tablero donde se han colocado tus barcos:"
              visTab (fst j2) 2
              turnos njuego
            else do
              putStrLn "¡Lo sentimos, ese tamaño no es válido!"
              play