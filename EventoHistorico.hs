
-- Módulo EventoHistorico

module EventoHistorico
  (
    EventoHistorico(..),
    fecha,
    eventos,
    desdeLinea
  ) where

type Fecha = Int -- Año
type Evento = String
data EventoHistorico = EventoHistorico Fecha [Evento]
  deriving Show

fecha :: EventoHistorico -> Fecha
fecha (EventoHistorico x _) = x

eventos :: EventoHistorico -> [Evento]
eventos (EventoHistorico _ x) = x

encuentraAlmohadilla :: String -> Int
encuentraAlmohadilla = length . takeWhile (/= '#') -- length t where t = takeWhile (/= '#') s

separaEventosDesdeLinea :: String -> [Evento]
separaEventosDesdeLinea [] = []
separaEventosDesdeLinea s = t:separaEventosDesdeLinea (drop 1 z)  where (t,z) = break (== '#') s


separaFechaDesdeLinea :: String -> (Int, [Evento])
separaFechaDesdeLinea s = (read (take t s), separaEventosDesdeLinea (drop (t+1) s) ) where t = encuentraAlmohadilla s

desdeLinea :: String -> EventoHistorico
desdeLinea s = EventoHistorico f e where (f,e) = separaFechaDesdeLinea s


