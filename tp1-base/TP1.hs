module TP1 where

data Caja = Bombilla Bool | Nada
              deriving Eq
instance Show Caja where
    show = showDeCaja

showDeCaja :: Caja -> String 
showDeCaja (Bombilla True) = "💡"
showDeCaja (Bombilla False) = "⚪️"
showDeCaja (Nada) = "🛑"

data Circuito = Caja     Caja
              | Serie    Circuito Circuito
              | Paralelo Caja Circuito Circuito Caja
                  deriving Eq
instance Show Circuito where
    show = showDeCircuitoConEstructura

showDeCircuito :: Circuito -> String
showDeCircuito (Caja caja) = showDeCaja caja
showDeCircuito (Serie circuitoInicial circuitoFinal) =
  (showDeCircuito circuitoInicial) ++ "-" ++ (showDeCircuito circuitoFinal)
showDeCircuito (Paralelo cajaEntrada circuitoIzquierdo circuitoDerecho cajaSalida) =
  (showDeCaja cajaEntrada) ++
  "{" ++ (showDeCircuito circuitoIzquierdo) ++ "}" ++
  "{" ++ (showDeCircuito circuitoDerecho) ++ "}" ++
  (showDeCaja cajaSalida)

showDeCircuitoConEstructura :: Circuito -> String
showDeCircuitoConEstructura (Caja caja) = showDeCaja caja
showDeCircuitoConEstructura (Serie circuitoInicial circuitoFinal) = "(" ++
  (showDeCircuitoConEstructura circuitoInicial) ++
    "-" ++
  (showDeCircuitoConEstructura circuitoFinal) ++ ")"
showDeCircuitoConEstructura (Paralelo cajaEntrada circuitoIzquierdo circuitoDerecho cajaSalida) =
  (showDeCaja cajaEntrada) ++
  "{" ++ (showDeCircuitoConEstructura circuitoIzquierdo) ++ "}" ++
  "{" ++ (showDeCircuitoConEstructura circuitoDerecho) ++ "}" ++
  (showDeCaja cajaSalida)

on  = Bombilla True
off = Bombilla False

cajaOn   = Caja on
cajaOff  = Caja off
cajaNada = Caja Nada

-- agregado para testear el ejemplo
ejemplo = Serie 
  (Paralelo 
    on  
    (Paralelo 
      off
      cajaNada
      cajaOn
      on)
    (Paralelo 
      Nada 
      cajaOn
      cajaOff
      Nada)
    on)
  cajaOn

ejemplo2 = Serie
  (Paralelo 
    on  
    (Paralelo 
      off
      cajaNada
      cajaOn
      on)
    (Paralelo 
      Nada 
      cajaOn
      cajaOff
      Nada)
    on)
  (Serie
    (Serie cajaOn cajaOff)
    (Serie cajaOff cajaNada))


ejemplo3 = Serie cajaOn (Serie (Serie cajaOn cajaOff) (Serie cajaOff cajaNada))
ejemplo4 = Serie (Serie (Serie (Serie cajaOn cajaOn) cajaOff) cajaOff) cajaNada
-- 1: recCircuito

recCircuito :: (Caja -> b) -> (b -> b -> Circuito -> Circuito -> b) -> (Caja -> b -> b -> Caja -> Circuito -> Circuito -> b) -> Circuito -> b
recCircuito fCaja fSerie fParalelo circuito = case circuito of 
  Caja caja -> fCaja caja
  Serie ci1 ci2 -> fSerie (rec ci1) (rec ci2) ci1 ci2
  Paralelo ca1 ci1 ci2 ca2 -> fParalelo ca1 (rec ci1) (rec ci2) ca2 ci1 ci2
  where rec = recCircuito fCaja fSerie fParalelo

-- 2: foldCircuito

foldCircuito :: (Caja -> b) -> (b -> b -> b) -> (Caja -> b -> b -> Caja -> b) -> Circuito -> b
foldCircuito fCaja fSerie fParalelo = recCircuito fCaja (\ci1rec ci2rec _ _ -> fSerie ci1rec ci2rec) (\ca1 ci1rec ci2rec ca2 _ _ -> fParalelo ca1 ci1rec ci2rec ca2)

-- 3 invertido

invertido :: Circuito -> Circuito
invertido = foldCircuito Caja (flip Serie) (\ca1 ci1rec ci2rec ca2 -> Paralelo ca2 ci2rec ci1rec ca1)

-- 4: hayCaminoIluminado

hayCaminoIluminado :: Circuito -> Bool
hayCaminoIluminado = foldCircuito (== on) (&&) (\ca1 ci1rec ci2rec ca2 -> ca1 == on && (ci1rec || ci2rec) && ca2 == on) 

-- 5: cantidadPrendidas

cantidadPrendidas :: Circuito -> Int
cantidadPrendidas = foldCircuito (\ca -> sumarPrendida ca) (+) (\ca1 ci1rec ci2rec ca2 -> sumarPrendida ca1 + ci1rec + ci2rec + sumarPrendida ca2)
  where sumarPrendida ca = if ca == on then 1 else 0

-- 6: cajasDeCircuito

cajasDeCircuito :: Circuito -> [Caja]
cajasDeCircuito = foldCircuito (: []) (++) (\ca1 ci1rec ci2rec ca2 -> [ca1] ++ ci1rec ++ ci2rec ++ [ca2])

-- 7: esCircuitoProlijo

esSerie :: Circuito -> Bool
esSerie = foldCircuito (const False) (\_ _ -> True) (\_ _ _ _ -> False)

esCircuitoProlijo :: Circuito -> Bool
esCircuitoProlijo = recCircuito (const True) (\ci1rec ci2rec _ ci2 -> (not $ esSerie ci2) && ci1rec && ci2rec) (\_ ci1rec ci2rec _ _ _ -> ci1rec && ci2rec)

-- 8: circuitoEmprolijado

circuitoEmprolijado :: Circuito -> Circuito
circuitoEmprolijado = undefined

-- 9: tienenLaMismaEstructura 

tienenLaMismaEstructura :: Circuito -> Circuito -> Bool
tienenLaMismaEstructura = foldCircuito (\_ -> \circuito -> case circuito of
                                                            Caja _ -> True
                                                            Serie _ _ -> False
                                                            Paralelo _ _ _ _ -> False)
                                       (\c1rec c2rec -> \circuito -> case circuito of
                                                                      Caja _ -> False
                                                                      Serie c1 c2 -> c1rec c1 && c2rec c2
                                                                      Paralelo _ _ _ _ -> False)
                                       (\_ c1rec c2rec _ -> \circuito -> case circuito of
                                                                          Caja _ -> False
                                                                          Serie _ _ -> False
                                                                          Paralelo _ c1 c2 _ -> c1rec c1 && c2rec c2)

-- 10: subCircuitoMásResistente

subCircuitoMásResistente = undefined -- TODO: COMPLETAR

{-- 11: Demostrar: alternado . alternado = id

alternado :: Circuito -> Circuito
{AC} alternado (Caja caja) = Caja (cajaAlternada caja)
{AS} alternado (Serie ci cf) = Serie (alternado ci) (alternado cf)
{AP} alternado (Paralelo ce ci cd cs) =
       Paralelo (cajaAlternada ce) (alternado ci) (alternado cd) (cajaAlternada cs)

cajaAlternada :: Caja -> Caja
{CAN} cajaAlternada Nada = Nada
{CAB} cajaAlternada Bombilla booleano = Bombilla not booleano

(.) :: (b -> c) -> (a -> b) -> a -> c
{C} (f . f) x = f (f x)

id :: a -> a
{I} id x = x

not :: Bool -> Bool
{NT} not True = False
{NF} not False = True

-- TODO: COMPLETAR

--}
