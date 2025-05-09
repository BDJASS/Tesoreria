/*
  Programa : cxcc1495.i
  Funcion  : Calculo de IVA, asumiendo actualizacion a nuevo iva 
  Autor    : FLC
  Fecha    : 17 FEB 2010
*/

ASSIGN l-sub = 0
       l-ivasub = 0.
IF {&Id-MC_Fact} = 1 THEN DO:
   FIND Factura WHERE Factura.Id-Factura = w-MovCliente.RefSaldo NO-LOCK NO-ERROR.
   IF AVAILABLE Factura AND Factura.RetIva > 0 THEN DO:
      ASSIGN l-monto = (w-MovCliente.Importe * -1)
              l-sub   = l-sub + l-monto
              l-ivaimp = 0
              l-ivasub = l-ivasub + l-ivaimp.
      IF w-MovCliente.RefSaldo BEGINS "1" THEN DO:
          {cxcc1496.i &PorcIVA = 0 &Suc = 1}
      END. /* es una factura de frontera */              
      ELSE IF w-MovCliente.RefSaldo BEGINS "2" THEN DO:
          {cxcc1496.i &PorcIVA = 0 &Suc = 2}
      END. /* es una factura de cedis */              
      ELSE IF w-MovCliente.RefSaldo BEGINS "3" THEN DO:
          {cxcc1496.i &PorcIVA = 0 &Suc = 3}
      END. /* es una factura de fug */              
      ELSE IF w-MovCliente.RefSaldo BEGINS "4" THEN DO:
          {cxcc1496.i &PorcIVA = 0 &Suc = 4}
      END. /* es una factura de saltillo */              
      ELSE IF w-MovCliente.RefSaldo BEGINS "5" THEN DO:
          {cxcc1496.i &PorcIVA = 0 &Suc = 5}
      END.  /* es una factura de chihuahua */              
      ELSE IF w-MovCliente.RefSaldo BEGINS "6" THEN DO:
          {cxcc1496.i &PorcIVA = 0 &Suc = 6}
      END.  /* es una factura de pablo livas */              
      ELSE IF w-MovCliente.RefSaldo BEGINS "7" THEN DO:
          {cxcc1496.i &PorcIVA = 0 &Suc = 7}
      END.  /* es una factura de ruiz cortines */              
      ELSE IF w-MovCliente.RefSaldo BEGINS "8" THEN DO:
          {cxcc1496.i &PorcIVA = 0 &Suc = 8}
      END.  /* es una factura de ruiz cortines */              
      ELSE IF w-MovCliente.RefSaldo BEGINS "9" THEN DO:
          {cxcc1496.i &PorcIVA = 0 &Suc = 9}
      END.  /* es una factura de diego diaz */              
      ELSE IF w-MovCliente.RefSaldo BEGINS "N" THEN DO:
          {cxcc1496.i &PorcIVA = 0 &Suc = 10}
      END.  /* es una factura de cerradas de anahuac */              
      ELSE DO:
          {cxcc1496.i &PorcIVA = 0 &Suc = 0}
      END. /* es ya factura de matriz */
   END.
   ELSE DO:
       FIND FIRST DistIVA WHERE DistIVA.Id-Factura = w-movcliente.refsaldo AND
                                DistIVa.TipoVenta = 3 NO-LOCK 
                                                                    NO-ERROR.
       IF NOT AVAILABLE DistIVA THEN DO:
          IF AVAILABLE Factura AND Factura.FecReg >= 01/01/2010 THEN DO:
              ASSIGN l-monto = ((w-MovCliente.Importe * -1) * 1) / 1.16
                     l-sub   = l-sub + l-monto
                     l-ivaimp = (w-MovCliente.Importe * -1) - l-monto
                     l-ivasub = l-ivasub + l-ivaimp.

              IF w-MovCliente.RefSaldo BEGINS "1" THEN DO:
                  {cxcc1496.i &PorcIVA = 16 &Suc = 1}
              END. /* es una factura de frontera */              
              ELSE IF w-MovCliente.RefSaldo BEGINS "2" THEN DO:
                  {cxcc1496.i &PorcIVA = 16 &Suc = 2}
              END. /* es una factura de cedis */              
              ELSE IF w-MovCliente.RefSaldo BEGINS "3" THEN DO:
                  {cxcc1496.i &PorcIVA = 16 &Suc = 3}
              END. /* es una factura de fug */              
              ELSE IF w-MovCliente.RefSaldo BEGINS "4" THEN DO:
                  {cxcc1496.i &PorcIVA = 16 &Suc = 4}
              END. /* es una factura de saltillo */              
              ELSE IF w-MovCliente.RefSaldo BEGINS "5" THEN DO:
                  {cxcc1496.i &PorcIVA = 16 &Suc = 5}
              END.  /* es una factura de chihuahua */              
              ELSE IF w-MovCliente.RefSaldo BEGINS "6" THEN DO:
                  {cxcc1496.i &PorcIVA = 16 &Suc = 6}
              END.  /* es una factura de pablo livas */              
              ELSE IF w-MovCliente.RefSaldo BEGINS "7" THEN DO:
                  {cxcc1496.i &PorcIVA = 16 &Suc = 7}
              END.  /* es una factura de ruiz cortines */              
              ELSE IF w-MovCliente.RefSaldo BEGINS "8" THEN DO:
                  {cxcc1496.i &PorcIVA = 16 &Suc = 8}
              END.  /* es una factura de cumbres */              
              ELSE IF w-MovCliente.RefSaldo BEGINS "9" THEN DO:
                  {cxcc1496.i &PorcIVA = 16 &Suc = 9}
              END.  /* es una factura de diego diaz */              
              ELSE IF w-MovCliente.RefSaldo BEGINS "N" THEN DO:
                  {cxcc1496.i &PorcIVA = 16 &Suc = 10}
              END.  /* es una factura de cerradas de anahuac */              
              ELSE DO:
                  {cxcc1496.i &PorcIVA = 16 &Suc = 0}
              END. /* es ya factura de matriz */
          END.
          ELSE DO:
              IF w-MovCliente.FecReg < 01/11/2010 THEN DO:
                  ASSIGN l-monto = ((w-MovCliente.Importe * -1) * 1) / 1.15
                         l-sub   = l-sub + l-monto
                         l-ivaimp = l-monto * 0.15
                         l-ivasub = l-ivasub + l-ivaimp.

                  IF w-MovCliente.RefSaldo BEGINS "1" THEN DO:
                      {cxcc1496.i &PorcIVA = 15 &Suc = 1}
                  END. /* es una factura de frontera */              
                  ELSE IF w-MovCliente.RefSaldo BEGINS "2" THEN DO:
                      {cxcc1496.i &PorcIVA = 15 &Suc = 2}
                  END. /* es una factura de cedis */              
                  ELSE IF w-MovCliente.RefSaldo BEGINS "3" THEN DO:
                      {cxcc1496.i &PorcIVA = 15 &Suc = 3}
                  END. /* es una factura de fug */              
                  ELSE IF w-MovCliente.RefSaldo BEGINS "4" THEN DO:
                      {cxcc1496.i &PorcIVA = 15 &Suc = 4}
                  END. /* es una factura de saltillo */              
                  ELSE IF w-MovCliente.RefSaldo BEGINS "5" THEN DO:
                      {cxcc1496.i &PorcIVA = 15 &Suc = 5}
                  END.  /* es una factura de chihuahua */              
                  ELSE IF w-MovCliente.RefSaldo BEGINS "6" THEN DO:
                      {cxcc1496.i &PorcIVA = 15 &Suc = 6}
                  END.  /* es una factura de pablo livas */              
                  ELSE IF w-MovCliente.RefSaldo BEGINS "7" THEN DO:
                      {cxcc1496.i &PorcIVA = 15 &Suc = 7}
                  END.  /* es una factura de ruiz cortines */              
                  ELSE IF w-MovCliente.RefSaldo BEGINS "8" THEN DO:
                      {cxcc1496.i &PorcIVA = 15 &Suc = 8}
                  END.  /* es una factura de cumbres */              
                  ELSE IF w-MovCliente.RefSaldo BEGINS "9" THEN DO:
                      {cxcc1496.i &PorcIVA = 15 &Suc = 9}
                  END.  /* es una factura de diago diaz */              
                  ELSE IF w-MovCliente.RefSaldo BEGINS "N" THEN DO:
                      {cxcc1496.i &PorcIVA = 15 &Suc = 10}
                  END.  /* es una factura de cerradas de anahuac */              
                  ELSE DO:
                      {cxcc1496.i &PorcIVA = 15 &Suc = 0}
                  END. /* es ya factura de matriz */
              END.
              ELSE DO:
                  ASSIGN l-monto = ((w-MovCliente.Importe * -1) * 1) / 1.15
                         l-sub   = l-sub + l-monto
                         l-ivaimp = l-monto * 0.16
                         l-ivasub = l-ivasub + l-ivaimp.
                  IF w-MovCliente.RefSaldo BEGINS "1" THEN DO:
                      {cxcc1496.i &PorcIVA = 16 &Suc = 1}
                  END. /* es una factura de frontera */              
                  ELSE IF w-MovCliente.RefSaldo BEGINS "2" THEN DO:
                      {cxcc1496.i &PorcIVA = 16 &Suc = 2}
                  END. /* es una factura de cedis */              
                  ELSE IF w-MovCliente.RefSaldo BEGINS "3" THEN DO:
                      {cxcc1496.i &PorcIVA = 16 &Suc = 3}
                  END. /* es una factura de fug */              
                  ELSE IF w-MovCliente.RefSaldo BEGINS "4" THEN DO:
                      {cxcc1496.i &PorcIVA = 16 &Suc = 4}
                  END. /* es una factura de saltillo */              
                  ELSE IF w-MovCliente.RefSaldo BEGINS "5" THEN DO:
                      {cxcc1496.i &PorcIVA = 16 &Suc = 5}
                  END.  /* es una factura de chihuahua */              
                  ELSE IF w-MovCliente.RefSaldo BEGINS "6" THEN DO:
                      {cxcc1496.i &PorcIVA = 16 &Suc = 6}
                  END.  /* es una factura de pablo livas */              
                  ELSE IF w-MovCliente.RefSaldo BEGINS "7" THEN DO:
                      {cxcc1496.i &PorcIVA = 16 &Suc = 7}
                  END.  /* es una factura de ruiz cortines */              
                  ELSE IF w-MovCliente.RefSaldo BEGINS "8" THEN DO:
                      {cxcc1496.i &PorcIVA = 16 &Suc = 8}
                  END.  /* es una factura de cumbres */              
                  ELSE IF w-MovCliente.RefSaldo BEGINS "9" THEN DO:
                      {cxcc1496.i &PorcIVA = 16 &Suc = 9}
                  END.  /* es una factura de diego diaz */              
                  ELSE IF w-MovCliente.RefSaldo BEGINS "N" THEN DO:
                      {cxcc1496.i &PorcIVA = 16 &Suc = 10}
                  END.  /* es una factura de cerradas de anahuac */              
                  ELSE DO:
                      {cxcc1496.i &PorcIVA = 16 &Suc = 0}
                  END. /* es ya factura de matriz */
              END.
          END.
       END.
       FOR EACH DistIVA WHERE DistIVA.Id-Factura = w-movcliente.RefSaldo AND
                              DistIVA.TipoVenta  = 3 NO-LOCK :
            FOR EACH b-distiva WHERE 
                                b-distiva.Id-Factura = w-movcliente.refsaldo AND
                                b-distiva.TipoVenta  = 3 NO-LOCK:
                ACCUMULATE b-distiva.id-factura (COUNT).
                IF (ACCUM COUNT b-distiva.id-factura) > 1 THEN LEAVE.
            END.
            ASSIGN l-count = (IF (ACCUM COUNT b-distiva.id-factura) > 1 
                                                          THEN TRUE ELSE FALSE).
            l-PorcSust = Distiva.PorcIva.
            IF w-MovCliente.FecReg >= 01/11/2010 THEN DO:
                IF l-PorcSust = 15 THEN l-PorcSust = 16.
                ELSE IF l-PorcSust = 10 THEN l-PorcSust = 11.
            END.
            
            ASSIGN l-monto = ((w-MovCliente.Importe * -1) * 
                          (IF (DistIVA.Participacion > 100) OR NOT l-count THEN 1 
                                  ELSE (DistIVA.Participacion / 100))    /
                              (1 + (DistIVA.PorcIVA / 100))).
            ASSIGN 
                   l-sub   = l-sub + l-monto
                   l-ivaimp = l-monto * (l-PorcSust / 100)
                   l-ivasub = l-ivasub + l-ivaimp.

            IF w-MovCliente.RefSaldo BEGINS "1" THEN DO:
                {cxcc1496.i &PorcIVA = l-PorcSust &Suc = 1}
            END. /* es una factura de frontera */              
            ELSE IF w-MovCliente.RefSaldo BEGINS "2" THEN DO:
                {cxcc1496.i &PorcIVA = l-PorcSust &Suc = 2}
            END. /* es una factura de cedis */              
            ELSE IF w-MovCliente.RefSaldo BEGINS "3" THEN DO:
                {cxcc1496.i &PorcIVA = l-PorcSust &Suc = 3}
            END. /* es una factura de fug */              
            ELSE IF w-MovCliente.RefSaldo BEGINS "4" THEN DO:
                {cxcc1496.i &PorcIVA = l-PorcSust &Suc = 4}
            END. /* es una factura de saltillo */              
            ELSE IF w-MovCliente.RefSaldo BEGINS "5" THEN DO:
                {cxcc1496.i &PorcIVA = l-PorcSust &Suc = 5}
            END.  /* es una factura de chihuahua */              
            ELSE IF w-MovCliente.RefSaldo BEGINS "6" THEN DO:
                {cxcc1496.i &PorcIVA = l-PorcSust &Suc = 6}
            END.  /* es una factura de pablo livas */              
            ELSE IF w-MovCliente.RefSaldo BEGINS "7" THEN DO:
                {cxcc1496.i &PorcIVA = l-PorcSust &Suc = 7}
            END.  /* es una factura de ruiz cortines */              
            ELSE IF w-MovCliente.RefSaldo BEGINS "8" THEN DO:
                {cxcc1496.i &PorcIVA = l-PorcSust &Suc = 8}
            END.  /* es una factura de cumbres */              
            ELSE IF w-MovCliente.RefSaldo BEGINS "9" THEN DO:
                {cxcc1496.i &PorcIVA = l-PorcSust &Suc = 9}
            END.  /* es una factura de diego diaz */              
            ELSE IF w-MovCliente.RefSaldo BEGINS "N" THEN DO:
                {cxcc1496.i &PorcIVA = l-PorcSust &Suc = 10}
            END.  /* es una factura de cerradas de anahuac */              
            ELSE DO:
                {cxcc1496.i &PorcIVA = l-PorcSust &Suc = 0}
            END. /* es ya factura de matriz */
       END.
   END.
END.
ELSE IF {&Id-MC_NCO} = 2 THEN DO:   
END.
ELSE IF {&Id-MC_Che} = 3 THEN DO:   
                /* Cheque Devuelto */
   ASSIGN l-monto = (w-MovCliente.Importe * -1)
          l-sub   = l-sub + l-monto
          l-ivaimp = 0
          l-ivasub = l-ivasub + l-ivaimp.
   {cxcc1496.i &porciva = 0 &Suc = 0}

/*
 Se cambio esta programacion, porque estaba desglosando iva a cheques
 devueltos que ya pagamos el iva en la cobranza original, y por lo tanto
 no debe de acumular iva de nuevo, visto por GEE y FRANC el 14/ABR/2004

   FIND Zona OF Cliente NO-LOCK NO-ERROR.
   ASSIGN l-monto = ((w-MovCliente.Importe * -1) / (1 + Zona.Porc-IVA / 100))
          l-sub   = l-sub + l-monto.
   {cxcc1496.i &porciva = Zona.Porc-Iva}

    IF Zona.Porc-IVA = 15 THEN DO:
     {cxcc1496.i &PorcIVA = 13}
    END.
*/
END.
