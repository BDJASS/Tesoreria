@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*------------------------------------------------------------------------
    File        : teserp015.p
    Purpose     : HU04 Resumen de Cobranza
                  basado en cxcc0530
                  Reporte 
                  
    Author(s)   : sis10
    Created     : Fecha actual   
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW. /* Manejo de errores global */

/* **********************  Internal Procedures  *********************** */



/* ***************************  Main Procedure *************************** */
DEFINE BUFFER b-distiva FOR DistIVA.
DEFINE VARIABLE l-reporte       AS CHARACTER                                 NO-UNDO.
DEFINE VARIABLE l-meses         AS CHARACTER                                 NO-UNDO.
DEFINE VARIABLE l-IdMC          AS CHARACTER
 INITIAL "98,99,57,58,59,60,61,62,63,64,65,66,67,68,69,77,78,87,73,83,90,95,96,97"
 NO-UNDO.
DEFINE VARIABLE l-i             AS INTEGER                                 NO-UNDO.
DEFINE VARIABLE l-tot15         AS DECIMAL FORMAT "-z,zzz,zz9.99"          NO-UNDO.
DEFINE VARIABLE l-tot10         AS DECIMAL FORMAT "-z,zzz,zz9.99"          NO-UNDO.
DEFINE VARIABLE l-tot0          AS DECIMAL FORMAT "-z,zzz,zz9.99"          NO-UNDO.
DEFINE VARIABLE l-ivadesc       AS DECIMAL FORMAT "zz,zzz,zz9.99"          NO-UNDO.
DEFINE VARIABLE l-pagosant      AS DECIMAL FORMAT 'ZZZ,ZZ9.99'             NO-UNDO.
DEFINE VARIABLE l-count                AS LOGI                                        NO-UNDO.
DEFINE VARIABLE l-pagosnor      LIKE MovCliente.importe                 NO-UNDO.
DEFINE VARIABLE l-pagosche      LIKE MovCliente.Importe                 NO-UNDO.
/* DEF VAR nada as deci format ">>>,>>>,>>9.99".  */

DEFINE WORKFILE w-MC
  FIELD Id-MC   LIKE MovCliente.Id-Mc
  FIELD Tot15   LIKE MovCliente.Importe FORMAT "-zzzzz,zz9.99"
  FIELD Tot10   LIKE MovCliente.Importe FORMAT "-zzzzz,zz9.99"
  FIELD Tot0    LIKE MovCliente.Importe FORMAT "-zzzzz,zz9.99".
DEFINE BUFFER b-mov FOR MovCliente.


  

DEFINE TEMP-TABLE ttRepCob NO-UNDO 
    FIELD Id          AS INTEGER          
    FIELD FecReg      LIKE MovCliente.FecReg        
    FIELD PagosNor    LIKE l-Pagosnor                    
    FIELD PagosCheDev LIKE l-Pagosche
    FIELD AntPend     LIKE l-PagosAnt
    FIELD TotDes15    LIKE l-tot15
    FIELD TotDes10    LIKE l-tot15
    FIELD TotDes0     LIKE l-tot0.
    
 DEFINE TEMP-TABLE ttRepPago NO-UNDO
    FIELD Id               AS INTEGER 
    FIELD TotalPagos       LIKE l-pagosnor FORMAT "zzz,zzz,zz9.99"
    FIELD TotalPagosChedev LIKE l-pagosche FORMAT "zzz,zzz,zz9.99"        
    FIELD TotalPagosAnt    LIKE l-pagosant FORMAT "zzz,zzz,zz9.99"
    FIELD TotalDescuentos  LIKE l-tot15    FORMAT "zzz,zzz,zz9.99"
    FIELD TotalDescuentos15 LIKE l-tot15    FORMAT "zzz,zzz,zz9.99"
    FIELD TotalDescuentos10 LIKE l-tot15    FORMAT "zzz,zzz,zz9.99"
    FIELD TotalDescuentos0  LIKE l-tot15    FORMAT "zzz,zzz,zz9.99"
    FIELD TotalGeneral     LIKE l-tot15    FORMAT "zzz,zzz,zz9.99".

DEFINE TEMP-TABLE ttDetPago NO-UNDO
    FIELD Id          AS INTEGER 
    FIELD Descripcion LIKE TabMC.Descr 
    FIELD TotDes15    LIKE l-tot15
    FIELD TotDes10    LIKE l-tot15
    FIELD TotDes0     LIKE l-tot0.
DEFINE DATASET dsResCob FOR ttRepCob, ttRepPago,ttDetPago
    DATA-RELATION RelAcuse FOR ttRepCob, ttRepPago 
    RELATION-FIELDS (Id, Id)
    DATA-RELATION RelAcuse2 FOR ttRepPago , ttDetPago 
    RELATION-FIELDS (Id, Id).

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetRepCob:
    DEFINE INPUT PARAMETER l-fecini AS DATE.
    DEFINE INPUT PARAMETER l-fecfin AS DATE.
    DEFINE OUTPUT PARAMETER DATASET FOR dsResCob.


ASSIGN l-meses   = "Enero,Febrero,Marzo,Abril,Mayo,Junio,Julio,Agosto," +
                   "Septiembre,Octubre,Noviembre,Diciembre".
                       
FOR EACH w-mc EXCLUSIVE-LOCK:
  DELETE w-mc.
END.

DO l-i = 1 TO NUM-ENTRIES(l-idMC):
    CREATE w-mc.
    ASSIGN w-mc.Id-MC = INTEGER(ENTRY(l-i,l-IdMC))
           w-mc.tot15 = 0
           w-mc.tot10 = 0
           w-mc.tot0  = 0.
END.
CREATE w-mc.
ASSIGN w-mc.Id-MC = 9999
       w-mc.tot15 = 0
       w-mc.tot10 = 0
       w-mc.tot0  = 0.
       
ASSIGN    l-tot15  = 0
          l-tot10  = 0
          l-pagosant = 0
          l-tot0   = 0
         /* l-fecini = g-today - DAY(g-today - 1)
          l-fecfin = g-today  */
          l-ivadesc = 0.
                     
FOR EACH MovCliente WHERE MovCliente.FecReg >= l-fecini AND
                            MovCliente.FecReg <= l-fecfin AND
                            MovCliente.Id-MC > 3 NO-LOCK
          BREAK BY MovCliente.FecReg:
    
      IF FIRST-OF(MovCliente.FecReg) THEN DO:
         FOR EACH Acuse WHERE Acuse.Tipo = "A" AND Acuse.Estatus = 4 AND
                              Acuse.FecDep = MovCliente.FecReg NO-LOCK :
             FIND Anticipo WHERE Anticipo.Id-Acuse = Acuse.Id-Acuse
                              NO-LOCK NO-ERROR.
             IF AVAILABLE Anticipo THEN DO:
                IF Anticipo.ImpAnticipo - Anticipo.ImpAplicado -
                   Anticipo.ImpDevuelto > 0 THEN
                   ASSIGN l-pagosant = l-pagosant + (
                                            Anticipo.ImpAnticipo -
                                            Anticipo.ImpAplicado -
                                            Anticipo.ImpDevuelto).
             END.
         END.
      END.
/* assign nada = 0.  */
      FIND Acuse WHERE Acuse.Id-Acuse = MovCliente.Documento
                    NO-LOCK NO-ERROR.
      IF NOT AVAILABLE Acuse OR
             AVAILABLE Acuse AND Acuse.Estatus = 4 THEN DO:
         FIND TipoPago WHERE TipoPago.Id-TP = MovCliente.Id-MC NO-LOCK NO-ERROR.

         IF MovCLiente.Id-mc <> 90 AND movcliente.id-mc > 62 THEN DO:
            FIND FIRST DistIVA WHERE DistIVA.Id-Factura = MovCliente.RefSaldo
                                AND DistIVa.TipoVenta = 3
                                              NO-LOCK NO-ERROR.
            IF NOT AVAILABLE DistIva THEN DO:
               FIND FIRST w-mc WHERE w-mc.Id-MC = 9999 EXCLUSIVE-LOCK NO-ERROR.
               FIND FIRST b-mov WHERE b-mov.RefSaldo = MovCliente.Refsaldo AND
                          b-mov.Id-MC    = 3 NO-LOCK NO-ERROR.
               IF AVAILABLE b-mov THEN DO:
                  ASSIGN l-tot0     = l-tot0  + (MovCliente.Importe * -1)
                     w-mc.tot0  = w-mc.Tot0 + (MovCliente.Importe * -1).
                  FIND FIRST w-mc WHERE w-mc.Id-MC = MovCliente.Id-MC
                                     EXCLUSIVE-LOCK NO-ERROR.
                  ASSIGN w-mc.tot0  = w-mc.tot0 + (MovCliente.Importe * -1).
               END. 
               ELSE DO:
                  ASSIGN l-tot15    = l-tot15 + (MovCliente.Importe * -1)
                     w-mc.Tot15 = w-mc.Tot15 +
                         ((MovCliente.Importe * -1) / 1.15).
        
                  FIND FIRST w-mc WHERE w-mc.Id-MC = MovCliente.Id-MC
                                     EXCLUSIVE-LOCK NO-ERROR.
                  ASSIGN w-mc.Tot15 = w-mc.Tot15 +
                         ((MovCliente.Importe * -1) / 1.15).
               END.
            END.
            FOR EACH DistIVA WHERE DistIVA.Id-Factura = MovCliente.RefSaldo
                               AND DistIVa.TipoVenta = 3     NO-LOCK:
                ACCUMULATE DISTIVA.PARTICIPACION (TOTAL).
                FOR EACH b-distiva WHERE b-distiva.Id-Factura = 
                                              MovCliente.RefSaldo NO-LOCK:
                    ACCUMULATE b-distiva.id-factura (COUNT).
                    IF (ACCUM COUNT b-distiva.id-factura) > 1 THEN LEAVE.
                END.
                ASSIGN l-count = (IF (ACCUM COUNT b-distiva.id-factura) > 1 
                                                        THEN TRUE ELSE FALSE).

                /* assign nada = nada + ((MovCliente.Importe * -1) *
                 ( IF (DistIVA.Participacion > 100) OR NOT l-count THEN 1 
                                  ELSE (DistIva.Participacion / 100)) ).  */

                FIND FIRST w-mc WHERE w-mc.Id-MC = 9999 EXCLUSIVE-LOCK NO-ERROR.
                IF DistIVA.PorcIVA = 10 THEN
                   ASSIGN l-tot10    = l-tot10 + ((MovCliente.Importe * -1) *
                     ( IF (DistIVA.Participacion > 100) OR NOT l-count THEN 1 
                              ELSE (DistIva.Participacion / 100)) )
                          w-mc.Tot10 = w-mc.Tot10 +
                                             ((MovCliente.Importe * -1) / 1.10).
                ELSE IF DistIVA.PorcIVA = 0 THEN
                   ASSIGN l-tot0     = l-tot0  + ((MovCliente.Importe * -1) *
                     ( IF (DistIVA.Participacion > 100) OR NOT l-count THEN 1 
                                  ELSE (DistIva.Participacion / 100)) )
                          w-mc.tot0  = w-mc.Tot0 +
                                              ((MovCliente.Importe * -1) / 1.0).
                ELSE
                   ASSIGN l-tot15 = l-tot15    + ((MovCliente.Importe * -1) *
                     ( IF (DistIVA.Participacion > 100) OR NOT l-count THEN 1 
                                  ELSE (DistIva.Participacion / 100)) )
                           w-mc.Tot15 = w-mc.Tot15 +
                                             ((MovCliente.Importe * -1) / 1.15).
                FIND FIRST w-mc WHERE w-mc.Id-Mc = MovCliente.Id-MC
                                                  EXCLUSIVE-LOCK NO-ERROR.
                IF DistIVA.PorcIVA = 10 THEN
                    ASSIGN w-mc.Tot10 = w-mc.tot10 +
                                    ((MovCliente.Importe * -1) / 1.10).
                ELSE IF DistIVA.PorcIVA = 0 THEN
                    ASSIGN w-mc.tot0  = w-mc.tot0 +
                                    ((MovCliente.Importe * -1) / 1.0).
                ELSE DO:
                    /* IF DistIVA.PorcIVA = 15 THEN  */
                    ASSIGN w-mc.Tot15 = w-mc.Tot15 +
                                       ((MovCliente.Importe * -1) / 1.15).
                END.
            END.
/* if nada <> (movcliente.importe * -1) then do:
   disp movcliente.refsaldo (importe * -1) nada
        ((movcliente.importe * -1) - nada) label "dif" (TOTAL).
   nada = 0.
end. */

        END.
        ELSE DO:
            FIND FIRST PagoAcuse WHERE PagoAcuse.Id-Acuse = MovCliente.Documento
                    NO-LOCK NO-ERROR.
            
            FIND FIRST b-mov WHERE b-mov.RefSaldo = MovCliente.Refsaldo AND
                                   b-mov.Id-MC    = 3 NO-LOCK NO-ERROR.
            IF AVAILABLE b-mov THEN
               ASSIGN l-pagosche = l-pagosche + (movCliente.Importe * -1).
            ELSE
               IF (AVAILABLE Acuse AND Acuse.Estatus = 4) OR
                                       NOT AVAILABLE Acuse THEN
                   ASSIGN l-pagosnor = l-pagosnor + (MovCliente.Importe * -1 *
                       (IF MovCliente.Id-Moneda > 1 AND AVAILABLE PagoAcuse AND PagoAcuse.Id-Moneda = 1 
                        THEN MovCliente.TipoCambio ELSE 1)).
        END.
    END.
    IF LAST-OF(MovCliente.FecReg) THEN DO:
        CREATE ttRepCob.
        ASSIGN
           ttRepCob.FecReg      = MovCliente.FecReg
           ttRepCob.PagosNor    =  l-pagosnor
           ttRepCob.PagosCheDev = l-pagosche
           ttRepCob.AntPend     = l-pagosant
           ttRepCob.TotDes15    =  l-tot15
           ttRepCob.TotDes10    =  l-tot10
           ttRepCob.TotDes0     =  l-tot0.

      //  DOWN STREAM s-salida WITH FRAME f-cobranza.
        ACCUMULATE l-tot15 (TOTAL).
        ACCUMULATE l-tot10 (TOTAL).
        ACCUMULATE l-tot0  (TOTAL).
        ACCUMULATE l-pagosnor (TOTAL).
        ACCUMULATE l-pagosche (TOTAL).
        ACCUMULATE l-pagosant (TOTAL).
        ASSIGN  l-tot15    = 0 l-tot10    = 0 l-tot0   = 0
                l-pagosnor = 0 l-pagosche = 0
                l-pagosant = 0.
    END. /* IF LAST-OF(MovCliente.FecReg) */ 
  END. /* del for each a movcliente */    
  FIND FIRST w-mc WHERE w-mc.Id-MC = 9999 NO-LOCK NO-ERROR.
  FIND FIRST w-mc WHERE w-mc.Id-MC = 9999 EXCLUSIVE-LOCK NO-ERROR.
  ASSIGN w-mc.Tot15 = (ACCUM TOTAL l-tot15) - ((ACCUM TOTAL l-tot15) / 1.15)
         w-mc.Tot10 = (ACCUM TOTAL l-tot10) - ((ACCUM TOTAL l-tot10) / 1.10)
         w-mc.Tot0  = (ACCUM TOTAL l-tot0) * 0.0.
  // DISPLAY STREAM s-salida
    CREATE  ttRepPago.
    ASSIGN 
           ttRepPago.TotalPagos = (ACCUM TOTAL l-pagosnor)
           ttRepPago.TotalPagosChedev      = (ACCUM TOTAL l-pagosche)
           ttRepPago.TotalPagosAnt         = (ACCUM TOTAL l-pagosant) 
           ttRepPago.TotalDescuentos      =  ((ACCUM TOTAL l-tot15) + (ACCUM TOTAL l-tot10) +
                                    (ACCUM TOTAL l-tot0)) 
           ttRepPago.TotalDescuentos15 =  (ACCUM TOTAL l-tot15)
           ttRepPago.TotalDescuentos10 = (ACCUM TOTAL l-tot10)
           ttRepPago.TotalDescuentos0  = (ACCUM TOTAL l-tot0)
           
           ttRepPago.TotalGeneral      = ((ACCUM TOTAL l-pagosnor) +
                             (ACCUM TOTAL l-pagosche) +
                             (ACCUM TOTAL l-pagosant)) +
                            ((ACCUM TOTAL l-tot15) + (ACCUM TOTAL l-tot10) +
                           (ACCUM TOTAL l-tot0)) .
           
          CREATE  ttDetPago.
          ASSIGN ttDetPago.Descripcion = "Neto de Descuentos"
                 ttDetPago.TotDes15    =  ((ACCUM TOTAL l-tot15) - w-mc.Tot15)
                 ttDetPago.TotDes10    =  ((ACCUM TOTAL l-tot10) - w-mc.Tot10)
                 ttDetPago.TotDes0     =  ((ACCUM TOTAL l-tot0)  - w-mc.Tot0).
          
         CREATE  ttDetPago.
          ASSIGN ttDetPago.Descripcion = "IVA de Descuentos"
                 ttDetPago.TotDes15    =  w-mc.tot15
                 ttDetPago.TotDes10    =  w-mc.tot10
                 ttDetPago.TotDes0     =  w-mc.tot0.
  FOR EACH w-mc WHERE w-mc.Id-Mc <> 9999 NO-LOCK:
      FIND TabMC WHERE TabMc.Id-MC = w-mc.Id-MC NO-LOCK NO-ERROR.
      CREATE  ttDetPago.
          ASSIGN ttDetPago.Descripcion = TabMC.Descr WHEN AVAILABLE TabMC
                 ttDetPago.TotDes15    =  w-mc.tot15
                 ttDetPago.TotDes10    =  w-mc.tot10
                 ttDetPago.TotDes0     =  w-mc.tot0.
  END.
   
END PROCEDURE.
