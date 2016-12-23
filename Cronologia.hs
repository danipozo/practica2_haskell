-- Módulo Cronología

module Cronologia
  (
    Cronologia,
    
  ) where

import EventoHistorico

data Cronologia = Cronologia [EventoHistorico]
                  deriving Show

eventos :: Cronologia -> [EventoHistorico]
eventos (Cronologia es) = es

agregarEvento :: Cronologia -> EventoHistorico -> Cronologia
agregarEvento (Cronologia es) e = Cronologia (eless++[e]++egr) where (eless, egr) = break (\x -> fecha x > fecha e) es

reducirIguales :: [EventoHistorico] -> [EventoHistorico]
reducirIguales [] = []
reducirIguales (e1:e2:es) 
                        |  fecha e1 /= fecha e2 = e1:reducirIguales (e2:es)
                        |  fecha e1 == fecha e2 = (EventoHistorico (fecha e1) ((EventoHistorico.eventos e1) ++ (EventoHistorico.eventos e2))):(reducirIguales es)
-- Solo queda un elemento en la lista
reducirIguales (e:es) = [e]

unionCronologias :: Cronologia -> Cronologia -> Cronologia
unionCronologias c1 (Cronologia es2) = Cronologia (reducirIguales (aux c1 es2)) where
                                                     aux (Cronologia e1) [] = e1
                                                     aux (Cronologia e1) (e:e2) = aux (agregarEvento (Cronologia e1) e) e2

desdeTexto :: String -> Cronologia
desdeTexto s = Cronologia (map EventoHistorico.desdeLinea (lines s))
