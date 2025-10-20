import Data.List (sortBy)

type Intervalo = (Int, Int)

compararIntervalo :: Intervalo -> Intervalo -> Ordering
compararIntervalo (_, b1) (_, b2) = compare b1 b2

seleccionarIntervalos :: [Intervalo] -> [Intervalo]
seleccionarIntervalos [] = []
seleccionarIntervalos intervalos =
    reverse (go (tail intervalosOrdenados) [head intervalosOrdenados])
  where
    intervalosOrdenados = sortBy compararIntervalo intervalos

    go :: [Intervalo] -> [Intervalo] -> [Intervalo]
    go [] acumulador = acumulador
    go (intervaloActual:resto) (ultimoSeleccionado:acumuladorResto) =
      let inicioActual     = fst intervaloActual
          finUltimo        = snd ultimoSeleccionado
      in
        if inicioActual >= finUltimo then
          go resto (intervaloActual : ultimoSeleccionado : acumuladorResto)
        else
          go resto (ultimoSeleccionado : acumuladorResto)
