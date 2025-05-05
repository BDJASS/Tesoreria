/*
  Empresa  : Consultoria en Informatica Ejecutiva, S.A. de C.V.
  Modulo   : Cuentas por Cobrar
  Programa : cxcc0490.i
  Llamador : cxcc0490.p
  Funcion  : Calculo de IVA 
  Autor    : LUIS
  Fecha    : 15/04/97
  Modificado: RNPC - 2019-10-31 - Ajuste para mostrar NCRs en dolares a pesos 
*/

ASSIGN l-sub = 0
       l-iva = 0.
IF {&Id-MC_Fact} = 1 THEN DO:
   FIND Factura WHERE Factura.Id-Factura = b-Mov.RefSaldo NO-LOCK NO-ERROR.
   IF AVAILABLE Factura AND Factura.RetIva > 0 THEN DO:
      ASSIGN l-monto = (MovCliente.Importe * -1)
              l-sub   = l-sub + l-monto.
      IF b-Mov.RefSaldo BEGINS "1" THEN DO:
          {cxcc0491.i &PorcIVA = 0 &Suc = 1}
      END. /* es una factura de frontera */              
      ELSE IF b-Mov.RefSaldo BEGINS "2" THEN DO:
          {cxcc0491.i &PorcIVA = 0 &Suc = 2}
      END. /* es una factura de cedis */              
      ELSE IF b-Mov.RefSaldo BEGINS "3" THEN DO:
          {cxcc0491.i &PorcIVA = 0 &Suc = 3} 
      END. /* es una factura de fug */              
      ELSE IF b-Mov.RefSaldo BEGINS "4" THEN DO:
          {cxcc0491.i &PorcIVA = 0 &Suc = 4}
      END. /* es una factura de saltillo */              
      ELSE IF b-Mov.RefSaldo BEGINS "5" THEN DO:
          {cxcc0491.i &PorcIVA = 0 &Suc = 5}
      END.  /* es una factura de chihuahua */              
      ELSE IF b-Mov.RefSaldo BEGINS "6" THEN DO:
          {cxcc0491.i &PorcIVA = 0 &Suc = 6}
      END.  /* es una factura de pablo livas */              
      ELSE IF b-Mov.RefSaldo BEGINS "7" THEN DO:
          {cxcc0491.i &PorcIVA = 0 &Suc = 7}
      END.  /* es una factura de ruiz cortines */              
      ELSE IF b-Mov.RefSaldo BEGINS "8" THEN DO:
          {cxcc0491.i &PorcIVA = 0 &Suc = 8}
      END.  /* es una factura de cumbres */              
      ELSE IF b-Mov.RefSaldo BEGINS "9" THEN DO:
          {cxcc0491.i &PorcIVA = 0 &Suc = 9}
      END.  /* es una factura de diego diaz */              
      ELSE IF b-Mov.RefSaldo BEGINS "N" THEN DO:
          {cxcc0491.i &PorcIVA = 0 &Suc = 10}
      END.  /* es una factura de cerradas de anahuac */              
      ELSE DO:
          {cxcc0491.i &PorcIVA = 0 &Suc = 0}
      END. /* es ya factura de matriz */
   END.
   ELSE DO:
       FIND FIRST DistIVA WHERE DistIVA.Id-Factura = b-mov.refsaldo AND
                                DistIVa.TipoVenta = 3 NO-LOCK 
                                NO-ERROR.
       IF NOT AVAILABLE DistIVA THEN DO:
              IF AVAILABLE Factura AND Factura.FecReg >= 01/01/2010 THEN DO:
                  ASSIGN l-monto = ((MovCliente.Importe * -1) * 1) / 1.16
                          l-sub   = l-sub + l-monto.
                  IF b-Mov.RefSaldo BEGINS "1" THEN DO:
                      {cxcc0491.i &PorcIVA = 16 &Suc = 1}
                  END. /* es una factura de frontera */              
                  ELSE IF b-Mov.RefSaldo BEGINS "2" THEN DO:
                      {cxcc0491.i &PorcIVA = 16 &Suc = 2}
                  END. /* es una factura de cedis */              
                  ELSE IF b-Mov.RefSaldo BEGINS "3" THEN DO:
                      {cxcc0491.i &PorcIVA = 16 &Suc = 3}
                  END. /* es una factura de fug */              
                  ELSE IF b-Mov.RefSaldo BEGINS "4" THEN DO:
                      {cxcc0491.i &PorcIVA = 16 &Suc = 4}
                  END. /* es una factura de saltillo */              
                  ELSE IF b-Mov.RefSaldo BEGINS "5" THEN DO:
                      {cxcc0491.i &PorcIVA = 16 &Suc = 5}
                  END.  /* es una factura de chihuahua */              
                  ELSE IF b-Mov.RefSaldo BEGINS "6" THEN DO:
                      {cxcc0491.i &PorcIVA = 16 &Suc = 6}
                  END.  /* es una factura de pablo livas */              
                  ELSE IF b-Mov.RefSaldo BEGINS "7" THEN DO:
                      {cxcc0491.i &PorcIVA = 16 &Suc = 7}
                  END.  /* es una factura de ruiz cortines */              
                  ELSE IF b-Mov.RefSaldo BEGINS "8" THEN DO:
                      {cxcc0491.i &PorcIVA = 16 &Suc = 8}
                  END.  /* es una factura de cumbres */              
                  ELSE IF b-Mov.RefSaldo BEGINS "9" THEN DO:
                      {cxcc0491.i &PorcIVA = 16 &Suc = 9}
                  END.  /* es una factura de diego diaz */              
                  ELSE IF b-Mov.RefSaldo BEGINS "N" THEN DO:
                      {cxcc0491.i &PorcIVA = 16 &Suc = 10}
                  END.  /* es una factura de cerradas de anahuac */              
                  ELSE DO:
                      {cxcc0491.i &PorcIVA = 16 &Suc = 0}
                  END. /* es ya factura de matriz */
              END.
              ELSE DO:
                  ASSIGN l-monto = ((MovCliente.Importe * -1) * 1) / 1.15
                          l-sub   = l-sub + l-monto.
                  IF b-Mov.RefSaldo BEGINS "1" THEN DO:
                      {cxcc0491.i &PorcIVA = 15 &Suc = 1}
                  END. /* es una factura de frontera */              
                  ELSE IF b-Mov.RefSaldo BEGINS "2" THEN DO:
                      {cxcc0491.i &PorcIVA = 15 &Suc = 2}
                  END. /* es una factura de cedis */              
                  ELSE IF b-Mov.RefSaldo BEGINS "3" THEN DO:
                      {cxcc0491.i &PorcIVA = 15 &Suc = 3}
                  END. /* es una factura de fug */              
                  ELSE IF b-Mov.RefSaldo BEGINS "4" THEN DO:
                      {cxcc0491.i &PorcIVA = 15 &Suc = 4}
                  END. /* es una factura de saltillo */              
                  ELSE IF b-Mov.RefSaldo BEGINS "5" THEN DO:
                      {cxcc0491.i &PorcIVA = 15 &Suc = 5}
                  END.  /* es una factura de chihuahua */              
                  ELSE IF b-Mov.RefSaldo BEGINS "6" THEN DO:
                      {cxcc0491.i &PorcIVA = 15 &Suc = 6}
                  END.  /* es una factura de pablo livas */              
                  ELSE IF b-Mov.RefSaldo BEGINS "7" THEN DO:
                      {cxcc0491.i &PorcIVA = 15 &Suc = 7}
                  END.  /* es una factura de ruiz cortines */              
                  ELSE IF b-Mov.RefSaldo BEGINS "8" THEN DO:
                      {cxcc0491.i &PorcIVA = 15 &Suc = 8}
                  END.  /* es una factura de cumbres */              
                  ELSE IF b-Mov.RefSaldo BEGINS "9" THEN DO:
                      {cxcc0491.i &PorcIVA = 15 &Suc = 9}
                  END.  /* es una factura de diego diaz */              
                  ELSE IF b-Mov.RefSaldo BEGINS "N" THEN DO:
                      {cxcc0491.i &PorcIVA = 15 &Suc = 10}
                  END.  /* es una factura de cerradas de anahuac */              
                  ELSE DO:
                      {cxcc0491.i &PorcIVA = 15 &Suc = 0}
                  END. /* es ya factura de matriz */
              END.
       END.
       FOR EACH DistIVA WHERE DistIVA.Id-Factura = b-Mov.RefSaldo AND
                              DistIVA.TipoVenta  = 3 NO-LOCK :
            FOR EACH b-distiva WHERE b-distiva.Id-Factura = b-mov.refsaldo AND
                                     b-distiva.TipoVenta  = 3 NO-LOCK:
                ACCUMULATE b-distiva.id-factura (COUNT).
                IF (ACCUM COUNT b-distiva.id-factura) > 1 THEN LEAVE.
            END.
            ASSIGN l-count = (IF (ACCUM COUNT b-distiva.id-factura) > 1 
                                                          THEN TRUE ELSE FALSE).
            l-PorcSust = Distiva.PorcIva.
            IF MovCliente.FecReg >= 01/11/2010 AND MovCliente.Id-MC <> 67 AND MovCliente.Id-MC <> 65 THEN DO:
                IF l-PorcSust = 15 THEN l-PorcSust = 16.
                ELSE IF l-PorcSust = 10 THEN l-PorcSust = 11.
            END.
            
            ASSIGN l-monto = ROUND(((MovCliente.Importe * -1) * 
                             (IF (DistIVA.Participacion > 100) OR NOT l-count THEN 1 
                             ELSE (DistIVA.Participacion / 100)) )    /
                             (1 + (l-PorcSust / 100)),2). 
                   //l-sub   = l-sub + l-monto.
            
            // RNPC - 2019-10-31 - Valido si el movimiento esta en dolares y busco datos de NCR
            IF MovCliente.Id-Moneda > 1 THEN DO:
                FIND FIRST NCR WHERE NCR.Id-NCR = MovCliente.Id-Ncr AND NCR.Id-Moneda > 1 NO-LOCK NO-ERROR.
                IF AVAILABLE NCR THEN DO:
                    ASSIGN l-monto = 0
                           l-iva = 0.
                    FOR EACH DetNCR WHERE DetNCR.Id-NCR = NCR.Id-NCR AND (IF AVAILABLE Factura THEN DetNCR.Documento = Factura.Id-Factura ELSE TRUE) NO-LOCK: 
                        ASSIGN l-monto = l-monto + (Factura.TipoCambio * DetNCR.Importe) // SUBTOTAL
                               l-iva   = l-iva + (Factura.TipoCambio * ROUND((DetNCR.Importe * (l-PorcSust / 100)),2)).
                    END.
                END.
             
                // RNPC - 2019-10-31 - Valido si NO hay NCR, pero el Movimiento esta en dolares
                IF MovCliente.Id-Moneda > 1 AND MovCliente.Id-Ncr = '' THEN 
                    ASSIGN l-monto = IF AVAILABLE Factura THEN (Factura.TipoCambio * l-monto) ELSE (l-monto) // SUBTOTAL
                        l-iva   = l-monto * (l-PorcSust / 100).
            END.
                       
             l-sub   = l-sub + l-monto.
              
            /*IF inte(DistIVA.PorcIVA) >= 15 THEN DO:*/
              IF b-Mov.RefSaldo BEGINS "1" THEN DO:
                  {cxcc0491.i &PorcIVA = l-PorcSust &DistIVA = TRUE &Suc = 1}
              END. /* es una factura de frontera */              
              ELSE IF b-Mov.RefSaldo BEGINS "2" THEN DO:
                  {cxcc0491.i &PorcIVA = l-PorcSust &DistIVA = TRUE &Suc = 2}
              END. /* es una factura de cedis */              
              ELSE IF b-Mov.RefSaldo BEGINS "3" THEN DO:
                  {cxcc0491.i &PorcIVA = l-PorcSust &DistIVA = TRUE &Suc = 3}
              END. /* es una factura de fug */              
              ELSE IF b-Mov.RefSaldo BEGINS "4" THEN DO:
                  {cxcc0491.i &PorcIVA = l-PorcSust &DistIVA = TRUE &Suc = 4}
              END. /* es una factura de saltillo */              
              ELSE IF b-Mov.RefSaldo BEGINS "5" THEN DO:
                  {cxcc0491.i &PorcIVA = l-PorcSust &DistIVA = TRUE &Suc = 5}
              END.  /* es una factura de chihuahua */              
              ELSE IF b-Mov.RefSaldo BEGINS "6" THEN DO:
                  {cxcc0491.i &PorcIVA = l-PorcSust &DistIVA = TRUE &Suc = 6}
              END.  /* es una factura de pablo livas */              
              ELSE IF b-Mov.RefSaldo BEGINS "7" THEN DO:
                  {cxcc0491.i &PorcIVA = l-PorcSust &DistIVA = TRUE &Suc = 7}
              END.  /* es una factura de ruiz cortines */              
              ELSE IF b-Mov.RefSaldo BEGINS "8" THEN DO:
                  {cxcc0491.i &PorcIVA = l-PorcSust &DistIVA = TRUE &Suc = 8}
              END.  /* es una factura de cumbres */              
              ELSE IF b-Mov.RefSaldo BEGINS "9" THEN DO:
                  {cxcc0491.i &PorcIVA = l-PorcSust &DistIVA = TRUE &Suc = 9}
              END.  /* es una factura de diego diaz */              
              ELSE IF b-Mov.RefSaldo BEGINS "N" THEN DO:
                  {cxcc0491.i &PorcIVA = l-PorcSust &DistIVA = TRUE &Suc = 10}
              END.  /* es una factura de diego diaz */              
              ELSE DO:
                  {cxcc0491.i &PorcIVA = l-PorcSust &DistIVA = TRUE &Suc = 0}
              END. /* es ya factura de matriz */
            /*END.*/
       END.
   END.
END.
ELSE IF {&Id-MC_NCO} = 2 THEN DO:   
                /* Nota de Cargo */
   FIND Nco WHERE Nco.Id-nco = b-Mov.RefSaldo NO-LOCK NO-ERROR.
   /* IF NOT AVAILABLE NCO THEN NEXT. */
   IF AVAILABLE NCO THEN DO:
       IF Nco.Tipo = 3 AND NCO.SubTotal = 0 AND NCO.Iva > 0 THEN DO:
           ASSIGN l-monto = 0
                  l-sub   = l-sub + l-monto.
           {cxcc0491.i &porciva = Nco.PorcIva &Suc = 100}
       END.
       ELSE DO:
           ASSIGN l-monto = (MovCliente.Importe * -1 / (1 + Nco.PorcIVA / 100))
                  l-sub   = l-sub + l-monto.
            IF NCO.PorcIva >= 15 THEN DO: 
              FIND FIRST DetNCO WHERE DetNCO.Id-NCO = MovCliente.RefSaldo 
                                                                NO-LOCK NO-ERROR.
              IF DetNCO.Documento BEGINS "1" THEN DO:
                  {cxcc0491.i &PorcIVA = Nco.PorcIva &Suc = 1}
              END.
              ELSE IF DetNCO.Documento BEGINS "2" THEN DO:
                  {cxcc0491.i &PorcIVA = Nco.PorcIva &Suc = 2}
              END.
              ELSE IF DetNCO.Documento BEGINS "3" THEN DO:
                  {cxcc0491.i &PorcIVA = Nco.PorcIva &Suc = 3}
              END.
              ELSE IF DetNCO.Documento BEGINS "4" THEN DO:
                  {cxcc0491.i &PorcIVA = Nco.PorcIva &Suc = 4}
              END.
              ELSE IF DetNCO.Documento BEGINS "5" THEN DO:
                  {cxcc0491.i &PorcIVA = Nco.PorcIva &Suc = 5}
              END.
              ELSE IF DetNCO.Documento BEGINS "6" THEN DO:
                  {cxcc0491.i &PorcIVA = Nco.PorcIva &Suc = 6}
              END.
              ELSE IF DetNCO.Documento BEGINS "7" THEN DO:
                  {cxcc0491.i &PorcIVA = Nco.PorcIva &Suc = 7}
              END.
              ELSE IF DetNCO.Documento BEGINS "8" THEN DO:
                  {cxcc0491.i &PorcIVA = Nco.PorcIva &Suc = 8}
              END.
              ELSE IF DetNCO.Documento BEGINS "9" THEN DO:
                  {cxcc0491.i &PorcIVA = Nco.PorcIva &Suc = 9}
              END.
              ELSE IF DetNCO.Documento BEGINS "N" THEN DO:
                  {cxcc0491.i &PorcIVA = Nco.PorcIva &Suc = 10}
              END.
              ELSE DO:
                  {cxcc0491.i &PorcIVA = Nco.PorcIva &Suc = 0}
              END.
            END.
       END.
   END.
   ELSE DO:
       ASSIGN l-monto = (MovCliente.Importe * -1 / (1 + 15 / 100))
              l-sub   = l-sub + l-monto.
       /* {cxcc0491.i &porciva = 15} */
        FIND FIRST DetNCO WHERE DetNCO.Id-NCO = MovCliente.RefSaldo 
                                                          NO-LOCK NO-ERROR.
        IF DetNCO.Documento BEGINS "4" THEN DO:
            {cxcc0491.i &PorcIVA = 11}
        END. /* es una factura de saltillo */
        ELSE DO:
            {cxcc0491.i &PorcIVA = 13}
        END. /* es ya factura de matriz */
   END.
END.

ELSE IF {&Id-MC_Che} = 3 THEN DO:   
                /* Cheque Devuelto */
   ASSIGN l-monto = (MovCliente.Importe * -1)
          l-sub   = l-sub + l-monto.
   {cxcc0491.i &porciva = 0 &Suc = 0}

/*
   FIND Zona OF Cliente NO-LOCK NO-ERROR.
   ASSIGN l-monto = ((MovCliente.Importe * -1) / (1 + Zona.Porc-IVA / 100))
          l-sub   = l-sub + l-monto.
   {cxcc0491.i &porciva = Zona.Porc-Iva}

    IF Zona.Porc-IVA = 15 THEN DO:
     {cxcc0491.i &PorcIVA = 13}
    END.
*/
END.
