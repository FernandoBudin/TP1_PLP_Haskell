import Test.HUnit
import TP1

-- TESTS

testsInvertido :: Test
testsInvertido = TestList -- TODO: AGREGAR
  [ "Caja invertida (1)"
    ~: invertido cajaOn
    ~?= cajaOn
  , "Caja invertida (2)"
    ~: invertido cajaOff
    ~?= cajaOff
  , "Caja invertida (3)"
    ~: invertido cajaNada
    ~?= cajaNada
  ]

testsHayCaminoIluminado :: Test
testsHayCaminoIluminado = TestList -- TODO: AGREGAR
  [ "En una caja con bombilla encendida hay camino iluminado"
    ~: hayCaminoIluminado cajaOn
    ~?= True
  ]

testsCantidadPrendidas :: Test
testsCantidadPrendidas = TestList -- TODO: AGREGAR
  [ "Cantidad prendidas en caja prendida es 1"
    ~: cantidadPrendidas cajaOn
    ~?= 1
  ]

testsCajasDeCircuito :: Test
testsCajasDeCircuito = TestList -- TODO: AGREGAR
  [ "La lista de cajas de un circuito con una única caja es la lista con esa caja"
    ~: cajasDeCircuito cajaOn
    ~?= [on]
  ]

testsEsCircuitoProlijo :: Test
testsEsCircuitoProlijo = TestList -- TODO: AGREGAR
  [ "Una caja es prolija"
    ~: esCircuitoProlijo cajaOn
    ~?= True
  ]

-- NOTA: para correr este test, cambiar la línea 18 del archivo tp1.hs de "show = showDeCircuito" a
  -- "show = showDeCircuitoConEstructura".
  -- De esa forma, podrán distinguir la estructura de los circuitos en serie.
testsCircuitoEmprolijado :: Test
testsCircuitoEmprolijado = TestList -- TODO: AGREGAR
  [ "La versión emprolijada de una caja es la misma caja"
    ~: circuitoEmprolijado cajaOn
    ~?= cajaOn
  ]

testsTienenLaMismaEstructura :: Test
testsTienenLaMismaEstructura = TestList -- TODO: AGREGAR
  [
    
  ]

testsSubCircuitoMásResistente :: Test
testsSubCircuitoMásResistente = TestList -- TODO: AGREGAR
  [ "Caja simple" ~: 
        subCircuitoMasResistente cajaOn ~?= cajaOn,

    -- 2. Caso Serie (El total suma más que las partes)
    "Serie gana contra subcircuitos" ~: 
        subCircuitoMasResistente (Serie cajaOn cajaOff) ~?= Serie cajaOn cajaOff,

    -- 3. Caso Paralelo (La división reduce la resistencia total)
    "Rama interna le gana al Paralelo completo" ~: 
        subCircuitoMasResistente (Paralelo Nada cajaOn cajaOff Nada) ~?= cajaOn,

    -- 4. Caso Complejo (Un paralelo anidado pierde contra una de sus ramas)
    "Serie profunda gana en circuito mayor" ~:
        subCircuitoMasResistente (Serie cajaNada (Serie cajaOn cajaOff)) ~?= Serie cajaOn cajaOff,

    -- 5. Caso Paralelo Pesado (Los extremos suman suficiente para que gane el total)
    "Paralelo complejo gana absoluto" ~:
        subCircuitoMasResistente (Paralelo on (Serie cajaOn cajaOn) cajaOff off) ~?= Paralelo on (Serie cajaOn cajaOn) cajaOff off
  ]

tests :: Test
tests = TestList
  [ TestLabel "invertido"                testsInvertido
  , TestLabel "hayCaminoIluminado"       testsHayCaminoIluminado
  , TestLabel "cantidadPrendidas"        testsCantidadPrendidas
  , TestLabel "cajasDeCircuito"          testsCajasDeCircuito
  , TestLabel "esCircuitoProlijo"        testsEsCircuitoProlijo
  , TestLabel "circuitoEmprolijado"      testsCircuitoEmprolijado
  , TestLabel "tienenLaMismaEstructura"  testsTienenLaMismaEstructura
  , TestLabel "subCircuitoMásResistente" testsSubCircuitoMásResistente
  ]

main :: IO ()
main = runTestTT tests >>= print