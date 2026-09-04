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

subCircuitoMasResistente :: Circuito -> Circuito
subCircuitoMasResistente = recCircuito (\caja -> Caja caja) 
                                       (\c1rec c2rec c1 c2 -> elMasResistente c1rec (elMasResistente c2rec (Serie c1 c2)))
                                       (\ca1 c1rec c2rec ca2 c1 c2 -> elMasResistente c1rec (elMasResistente c2rec (Paralelo ca1 c1 c2 ca2)))

elMasResistente :: Circuito -> Circuito -> Circuito
elMasResistente c1 c2 = if (resistencia c1 >= resistencia c2) then c1 else c2

resistencia :: Circuito -> Float
resistencia (Caja Nada) = 0.0
resistencia (Caja (Bombilla _)) = 5.0
resistencia (Serie c1 c2) = resistencia c1 + resistencia c2
resistencia (Paralelo cj1 c1 c2 cj2) = resistencia (Caja cj1) + resistenciaCentral + resistencia (Caja cj2)
  where
    resistenciaCentral = if (resistencia c1 == 0.0 || resistencia c2 == 0.0) then 0.0 else (1 / ((1 / resistencia c1) + (1 / resistencia c2)))

{-- 11: 

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

Demostrar que: alternado . alternado = id

Por extensionalidad funcional, si f x = g x => f = g. Luego basta ver que:

∀c :: Circuito . (alternado . alternado) c = id c

Predicado unario:
P(c) ≡ (alternado . alternado) c = id c

Esquema:
Por induccion estructural en c, basta ver que si:
1) ∀caja :: Caja . P(Caja caja)
2) ∀c1 :: Circuito . ∀c2 :: Circuito . (P(c1) ∧ P(c2)) => P(Serie c1 c2)
3) ∀cj1 :: Caja . ∀c1 :: Circuito . ∀c2 :: Circuito . ∀cj2 :: Caja . (P(c1) ∧ P(c2)) => P(Paralelo cj1 c1 c2 cj2)
Entonces ∀c :: Circuito . P(c)

1) Caso Caja: 
Quiero ver que: P(Caja caja) ≡ (alternado . alternado) (Caja caja) = id (Caja caja)
                         {C} ≡ alternado (alternado (Caja caja)) = id (Caja caja)
                        {AC} ≡ alternado (Caja (cajaAlternada caja)) = id (Caja caja)
                        {AC} ≡ Caja (cajaAlternada (cajaAlternada caja)) = id (Caja caja)
                      {LEMA} ≡ Caja (id caja) = id (Caja caja)
                         {I} ≡ Caja caja = id (Caja caja)
                         {I} ≡ Caja caja = Caja caja
Luego queda probado que ∀caja :: Caja . P(Caja caja).

2) Caso Serie:
Asumo valida mi Hipotesis Inductiva (P(c1) ∧ P(c2)) siendo:
P(c1) ≡ (alternado . alternado) c1 = id c1
P(c2) ≡ (alternado . alternado) c2 = id c2

Quiero ver que: P(Serie c1 c2) ≡ (alternado . alternado) (Serie c1 c2) = id (Serie c1 c2)
                           {C} ≡ alternado (alternado (Serie c1 c2)) = id (Serie c1 c2)
                          {AS} ≡ alternado (Serie (alternado c1) (alternado c2)) = id (Serie c1 c2)
                          {AS} ≡ Serie (alternado (alternado c1) (alternado (alternado c2))) = id (Serie c1 c2)
                           {C} ≡ Serie ((alternado . alternado) c1) (alternado (alternado c2)) = id (Serie c1 c2)
                           {C} ≡ Serie ((alternado . alternado) c1) ((alternado . alternado) c2) = id (Serie c1 c2)
                          {HI} ≡ Serie (id c1) ((alternado . alternado) c2) = id (Serie c1 c2)
                          {HI} ≡ Serie (id c1) (id c2) = id (Serie c1 c2)
                           {I} ≡ Serie c1 (id c2) = id (Serie c1 c2)
                           {I} ≡ Serie c1 c2 = id (Serie c1 c2)
                           {I} ≡ Serie c1 c2 = Serie c1 c2
Luego queda probado que ∀c1 :: Circuito . ∀c2 :: Circuito . P(Serie c1 c2) asumiendo valida la Hipotesis Inductiva.

3) Caso Paralelo:
Asumo valida mi Hipotesis Inductiva (P(c1) ∧ P(c2)) siendo:
P(c1) ≡ (alternado . alternado) c1 = id c1
P(c2) ≡ (alternado . alternado) c2 = id c2

Quiero ver que: P(Paralelo cj1 c1 c2 cj2) ≡ (alternado . alternado) (Paralelo cj1 c1 c2 cj2) = id (Paralelo cj1 c1 c2 cj2)
                                      {C} ≡ alternado (alternado (Paralelo cj1 c1 c2 cj2)) = id (Paralelo cj1 c1 c2 cj2)
                                     {AP} ≡ alternado (Paralelo (cajaAlternanda cj1) (alternado c1) (alternado c2) (cajaAlternada cj2)) = id (Paralelo cj1 c1 c2 cj2)
                                     {AP} ≡ Paralelo (cajaAlternada (cajaAlternada cj1)) (alternado (alternado c1)) (alternado (alternado c2)) (cajaAlternada (cajaAlternada cj2)) = id (Paralelo cj1 c1 c2 cj2)
                                   {LEMA} ≡ Paralelo (id cj1) (alternado (alternado c1)) (alternado (alternado c2)) (cajaAlternada (cajaAlternada cj2)) = id (Paralelo cj1 c1 c2 cj2)
                                   {LEMA} ≡ Paralelo (id cj1) (alternado (alternado c1)) (alternado (alternado c2)) (id cj2) = id (Paralelo cj1 c1 c2 cj2)
                                      {C} ≡ Paralelo (id cj1) ((alternado . alternado) c1) (alternado (alternado c2)) (id cj2) = id (Paralelo cj1 c1 c2 cj2)
                                      {C} ≡ Paralelo (id cj1) ((alternado . alternado) c1) ((alternado . alternado) c2) (id cj2) = id (Paralelo cj1 c1 c2 cj2)
                                     {HI} ≡ Paralelo (id cj1) ((alternado . alternado) c1) (id c2) (id cj2) = id (Paralelo cj1 c1 c2 cj2)
                                     {HI} ≡ Paralelo (id cj1) (id c1) (id c2) (id cj2) = id (Paralelo cj1 c1 c2 cj2)
                                      {I} ≡ Paralelo cj1 (id c1) (id c2) (id cj2) = id (Paralelo cj1 c1 c2 cj2)
                                      {I} ≡ Paralelo cj1 c1 (id c2) (id cj2) = id (Paralelo cj1 c1 c2 cj2)
                                      {I} ≡ Paralelo cj1 c1 c2 (id cj2) = id (Paralelo cj1 c1 c2 cj2)
                                      {I} ≡ Paralelo cj1 c1 c2 cj2 = id (Paralelo cj1 c1 c2 cj2)
                                      {I} ≡ Paralelo cj1 c1 c2 cj2 = Paralelo cj1 c1 c2 cj2
Luego queda probado que ∀cj1 :: Caja . ∀c1 :: Circuito . ∀c2 :: Circuito . ∀cj2 :: Caja . P(Paralelo cj1 c1 c2 cj2) asumiendo valida la Hipotesis Inductiva.
Finalmente queda probado que ∀c :: Circuito . P(c) al haber demostrado la propiedad para cada constructor del tipo Circuito.

Demostracion del LEMA:
Quiero demostrar que: ∀caja :: Caja . cajaAlternada (cajaAlternada caja) = id caja
Predicado unario:
Q(caja) ≡ cajaAlternada (cajaAlternada caja) = id caja

Por Lema de generacion del tipo Caja, basta ver que si:
1) Q(Nada)
2) ∀b :: Bool . Q(Bomilla b)
Entonces ∀caja :: Caja . Q(Caja)

1) Caso Nada:
Quiero ver que: Q(Nada) ≡ cajaAlternada (cajaAlternada Nada) = id Nada
                  {CAN} ≡ cajaAlternada Nada = id Nada
                  {CAN} ≡ Nada = id Nada
                    {I} ≡ Nada = Nada

2) Caso (Bomilla b):
Quiero ver que: Q(Bombilla b) ≡ cajaAlternada (cajaAlternada (Bombilla b)) = id (Bombilla b)
                        {CAB} ≡ cajaAlternada (Bombilla (not b)) id (Bombilla b)
                        {CAB} ≡ Bombilla (not (not b)) = id (Bombilla b)
Por Lema de generacion de Bool, analizo los casos:
A) b = True
B) b = False

A) Caso (b = True):
Quiero ver que: Bombilla (not (not True)) = id (Bombilla True)
         {NT} ≡ Bombilla (not False) = id (Bombilla True)
         {NF} ≡ Bombilla True = id (Bombilla True)
          {I} ≡ Bombilla True = Bombilla True

B) Caso (b = False):
Quiero ver que: Bombilla (not (not False)) = id (Bombilla False)
         {NT} ≡ Bombilla (not True) = id (Bombilla False)
         {NF} ≡ Bombilla False = id (Bombilla False)
          {I} ≡ Bombilla False = Bombilla False
Entonces ∀b :: Bool . Q(Bombilla b)
Luego queda probado que ∀caja :: Caja . Q(caja) al haber demostrado el LEMA para cada constructor del tipo Caja. 
--}