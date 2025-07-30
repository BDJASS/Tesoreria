/*
  Empresa    : Consultoria en Informatica Ejecutiva S.A. de C.V.
  Modulo     : tesoreria
  Programa   : tesa0832.p
  Funcion    : Incluido de Impresion de Remisiones
  Usado por  : tesa0831.p
  Autor      : LUIS
  Fecha      : 25/03/97
*/

ASSIGN l-Hora = ''.
IF MovCaja.TipoVenta <> 3 THEN DO:
    FIND Remision WHERE Remision.Id-Remision = MovCaja.Refer NO-LOCK NO-ERROR.
    IF AVAILABLE Remision THEN 
         ASSIGN l-hora = STRING(Remision.HorReg,"HH:MM").
END.
ELSE DO:
    FIND Factura WHERE Factura.Id-Factura = MovCaja.Refer NO-LOCK NO-ERROR.
    IF AVAILABLE Factura THEN      
         ASSIGN l-hora = STRING(Factura.HorReg,"HH:MM").
END.

IF MovCaja.TipoVenta = 6 THEN DO:
    ASSIGN l-hora = STRING(MovCaja.HorReg,"HH:MM").
END.    

DISPLAY STREAM s-salida
    MovCaja.Folio
    /*
    IF MovCaja.TipoVenta = 1
    THEN 'TIC'
    ELSE IF MovCaja.TipoVenta = 2
         THEN 'VTA'
         ELSE IF MovCaja.TipoVenta = 3
              THEN 'FAC'
              ELSE 'NCR'  @ l-eti
    */
    l-hora
    IF (MovCaja.TipoVenta < 3 OR MovCaja.TipoVenta = 6) THEN MovCaja.Referencia ELSE Ncr.Id-ncr @ MovCaja.Referencia
    IF AVAILABLE Remision
    THEN Remision.Id-Vendedor
    ELSE IF MovCaja.TipoVenta = 6
         THEN "Conc."
         ELSE ""  @ Remision.Id-Vendedor
    IF ((MovCaja.Canc AND NOT l-SiVale) OR MovCaja.TipoVenta = 6)
    THEN 0
    ELSE IF MovCaja.TipoVenta < 3
         THEN (IF AVAILABLE Remision
               THEN Remision.Tot
               ELSE IF AVAILABLE Factura
                    THEN Factura.Tot
                    ELSE MovCaja.TotVenta)
         ELSE (NCR.Tot) * -1 @ MovCaja.TotVenta
WITH FRAME {&Frame}.

DOWN STREAM s-salida WITH FRAME {&Frame}.
ACCUMULATE MovCaja.Folio (COUNT).

ASSIGN l-tot = l-tot + IF ((MovCaja.Canc AND NOT l-SiVale) OR MovCaja.TipoVenta = 6)
                       THEN 0
                       ELSE IF MovCaja.TipoVenta < 3
                            THEN Remision.Tot
                            ELSE NCR.Tot * -1
       l-iva = l-iva + IF ((MovCaja.Canc AND NOT l-SiVale) OR MovCaja.TipoVenta = 6)
                       THEN 0
                       ELSE IF MovCaja.TipoVenta < 3
                            THEN Remision.IVA
                            ELSE NCR.IVA * -1
       l-redo = l-redo + IF MovCaja.TipoVenta < 3 AND (NOT MovCaja.Canc OR l-SiVale)
                         THEN Remision.Redo
                         ELSE 0.                     
