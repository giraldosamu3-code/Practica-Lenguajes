import System.IO
import Data.List (find)
import System.Directory (doesFileExist)

-- =========================================================
-- 1. ESTRUCTURA DE DATOS
-- =========================================================
-- Representa a un estudiante en el sistema
-- idEstudiante  -> identificador del estudiante
-- horaEntrada   -> minutos desde 00:00
-- horaSalida    -> minutos desde 00:00
-- estaPresente  -> indica si está dentro de la universidad
data Estudiante = Estudiante {
    idEstudiante :: String,
    horaEntrada  :: Int,
    horaSalida   :: Int,
    estaPresente :: Bool
} deriving (Show, Read, Eq)

-- =========================================================
-- 2. LÓGICA DE TIEMPO
-- =========================================================
-- Convierte minutos a formato legible (horas y minutos)
formatearDuracion :: Int -> String
formatearDuracion totalMinutos =
    let horas = div totalMinutos 60
        minutos = mod totalMinutos 60
    in show horas ++ " horas y " ++ show minutos ++ " minutos"

-- Calcula y muestra el tiempo que el estudiante estuvo dentro
calcularYMostrarTiempo :: Int -> Int -> IO ()
calcularYMostrarTiempo entrada salida = do
    let total = salida - entrada
    putStrLn ("Tiempo en la universidad: " ++ formatearDuracion total)

-- =========================================================
-- 3. GESTIÓN DE ARCHIVOS (PERSISTENCIA)
-- =========================================================

-- Guarda todos los estudiantes en University.txt
guardarDatos :: [Estudiante] -> IO ()
guardarDatos lista = do
    writeFile "University.txt" (show lista)

-- Carga los estudiantes desde el archivo
cargarDatos :: IO [Estudiante]
cargarDatos = do
    existe <- doesFileExist "University.txt"
    if existe
        then do
            contenido <- readFile "University.txt"
            let lista = if null contenido then [] else read contenido
            return lista
        else return []

-- =========================================================
-- 4. MENÚ PRINCIPAL
-- =========================================================
ejecutarMenu :: [Estudiante] -> IO ()
ejecutarMenu listaActual = do

    -- Encabezado con color (similar a Prolog)
    putStrLn "\ESC[36m"
    putStrLn "==#=   ==============  =============   =#=="
    putStrLn "=#== SISTEMA DE REGISTRO UNIVERSITARIO ==#="
    putStrLn "==#=   ==============  =============   =#=="
    putStrLn "\ESC[0m"

    putStrLn "1. | Registrar Entrada        |"
    putStrLn "2. | Registrar Salida         |"
    putStrLn "3. | Buscar Estudiante por ID |"
    putStrLn "4. | Listar Estudiantes       |"
    putStrLn "5. | Salir                    |"

    putStr "Seleccione una opción: "
    hFlush stdout
    opcion <- getLine

    case opcion of

        -- =================================================
        -- OPCIÓN 1: REGISTRAR ENTRADA
        -- =================================================
        "1" -> do
            putStr "Ingrese ID del estudiante: "
            hFlush stdout
            idIn <- getLine

            case find (\e -> idEstudiante e == idIn && estaPresente e) listaActual of
                Just _ -> do
                    putStrLn "Error: El estudiante ya se encuentra en la universidad."
                    ejecutarMenu listaActual
                Nothing -> do
                    putStr "Ingrese hora de entrada (minutos desde 00:00): "
                    hFlush stdout
                    hInStr <- getLine
                    let hIn = read hInStr :: Int

                    let nuevo = Estudiante idIn hIn 0 True
                    let nuevaLista = nuevo : listaActual

                    guardarDatos nuevaLista
                    putStrLn "Entrada registrada exitosamente."

                    ejecutarMenu nuevaLista

        -- =================================================
        -- OPCIÓN 2: REGISTRAR SALIDA
        -- =================================================
        "2" -> do
            putStr "Ingrese ID del estudiante: "
            hFlush stdout
            idSal <- getLine

            case find (\e -> idEstudiante e == idSal && estaPresente e) listaActual of
                Nothing -> do
                    putStrLn "Error: El estudiante no está en la universidad o el ID no existe."
                    ejecutarMenu listaActual

                Just estudiante -> do
                    putStr "Ingrese hora de salida (minutos desde 00:00): "
                    hFlush stdout
                    hOutStr <- getLine
                    let hOut = read hOutStr :: Int

                    let nuevaLista =
                            map (\e ->
                                if idEstudiante e == idSal
                                then e {horaSalida = hOut, estaPresente = False}
                                else e
                            ) listaActual

                    calcularYMostrarTiempo (horaEntrada estudiante) hOut

                    guardarDatos nuevaLista
                    putStrLn "Salida registrada exitosamente."

                    ejecutarMenu nuevaLista

        -- =================================================
        -- OPCIÓN 3: BUSCAR ESTUDIANTE
        -- =================================================
        "3" -> do
            putStr "Ingrese ID a buscar: "
            hFlush stdout
            idBusq <- getLine

            case find (\e -> idEstudiante e == idBusq) listaActual of
                Just e -> do
                    putStrLn ("Estudiante ID: " ++ idEstudiante e
                        ++ " | Estado: "
                        ++ (if estaPresente e then "adentro" else "afuera")
                        ++ " | Entrada: " ++ show (horaEntrada e)
                        ++ " | Salida: " ++ show (horaSalida e))
                Nothing -> do
                    putStrLn "\ESC[31mEstudiante no encontrado.\ESC[0m"

            ejecutarMenu listaActual

        -- =================================================
        -- OPCIÓN 4: LISTAR ESTUDIANTES
        -- =================================================
        "4" -> do
            datosArchivo <- cargarDatos

            putStrLn "\ESC[34m"
            putStrLn "=============   =============="
            putStrLn "==== LISTA DE ESTUDIANTES ===="
            putStrLn "=============   =============="
            putStrLn "\ESC[0m"

            mapM_ print datosArchivo

            ejecutarMenu datosArchivo

        -- =================================================
        -- OPCIÓN 5: SALIR
        -- =================================================
        "5" -> do
            putStrLn "Saliendo del sistema..."

        -- =================================================
        -- OPCIÓN INVÁLIDA
        -- =================================================
        _ -> do
            putStrLn "Opción no válida."
            ejecutarMenu listaActual


-- =========================================================
-- 5. FUNCIÓN PRINCIPAL
-- =========================================================
main :: IO ()
main = do
    putStrLn "Iniciando sistema..."
    listaInicial <- cargarDatos
    ejecutarMenu listaInicial