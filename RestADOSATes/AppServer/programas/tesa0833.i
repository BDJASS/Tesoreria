
/*
  Empresa    : Consultoria en Informatica Ejecutiva S.A. de C.V.
  Modulo     : tesoreria
  Programa   : tesa0833.i
  Funcion    : Impresion de algunas opciones cuando exista remision
  Usado por  : tesa0831.p
  Autor      : LUIS
  Fecha      : 25/03/97
*/

   IF l-encontro THEN DO:
       DISPLAY STREAM s-salida
           SKIP(1)
           l-tot - l-iva FORMAT "-zzzz,zz9.99" LABEL "SUBTOTAL"  SKIP
           l-iva         FORMAT "-zzzz,zz9.99" LABEL "     IVA"  SKIP
           l-tot         FORMAT "-zzzz,zz9.99" LABEL "   TOTAL" SKIP(1)
           l-redo        FORMAT "-zzzz,zz9.99" LABEL "DONATIVO" SKIP(1)
           (ACCUM COUNT MovCaja.Folio)
                        LABEL "CANT NOTAS"
       WITH FRAME {&Frame} OVERLAY SIDE-LABEL COLUMN 5 WIDTH 38.
       RELEASE Remision.
       {&Aparte}
   END.
   ELSE DO:
       VIEW STREAM s-salida FRAME {&Framepri}.
       DISPLAY STREAM s-salida
             "SUBTOTAL:       0.00" AT 2   SKIP
             "     IVA:       0.00" AT 2    SKIP
             "   TOTAL:       0.00" AT 2   SKIP
             "DONATIVO:       0.00" AT 2   SKIP
       WITH FRAME {&Frame}2 OVERLAY SIDE-LABEL COLUMN 5 WIDTH 38.
       {&Aparte2}
   END.
