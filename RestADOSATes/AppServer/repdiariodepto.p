@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : repdiariodepto.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : sis6
    Created     : Mon May 26 17:30:05 CST 2025
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

DEFINE TEMP-TABLE ttTotalDepDiario
    FIELD IdReg  AS INTEGER 
    FIELD TotalDep AS DECIMAL
    FIELD TotalME AS DECIMAL
    FIELD TotalGral AS DECIMAL.

DEFINE TEMP-TABLE ttDepDiario
    FIELD IdReg        AS INTEGER
    FIELD IdAcuse      AS CHARACTER    
    FIELD IdCliente    AS INTEGER
    FIELD RazonSocial  AS CHARACTER    
    FIELD TotalPago    AS DECIMAL    
    FIELD TipoPago     AS CHARACTER
    FIELD IdBanco      AS INTEGER.
    
    DEFINE DATASET dsDepDiario FOR 
    ttTotalDepdiario,
    ttDepDiario
    DATA-RELATION DepDiario FOR ttTotalDepdiario, ttDepDiario
    RELATION-FIELDS (IdReg, IdReg).
    
    DEFINE VARIABLE cFechaISOIni AS CHARACTER NO-UNDO.
DEFINE VARIABLE dFechaIni    AS DATE      NO-UNDO.

DEF VAR l-Reng          AS DECIMAL NO-UNDO.
DEF VAR l-total         AS DECI FORMAT "zzz,zzz,zz9.99" LABEL "Total" NO-UNDO.
DEF VAR l-totalME       AS DECI FORMAT "zzz,zzz,zz9.99" LABEL "Total" NO-UNDO INITIAL 0.
DEF VAR l-totalGral     AS DECI FORMAT "zzz,zzz,zz9.99" LABEL "Total" NO-UNDO INITIAL 0.
DEF VAR l-HayMonedaEX   AS LOGICAL INITIAL FALSE                  NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetDiarioDepositos:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER l-FechaIni AS CHARACTER NO-UNDO. 
DEFINE OUTPUT PARAMETER DATASET FOR dsDepDiario.  

cFechaISOIni = SUBSTRING(l-FechaIni, 1, 10). 

    cFechaISOIni = SUBSTRING(cFechaISOIni, 9, 2) + "/" +  /* DD */ 
        SUBSTRING(cFechaISOIni, 6, 2) + "/" +  /* MM */            
        SUBSTRING(cFechaISOIni, 1, 4).         /* YYYY */

    dFechaIni = DATE(cFechaISOIni).


FOR EACH Acuse WHERE Acuse.FecDep  = dFechaIni AND
                      Acuse.Estatus = 4 NO-LOCK
                           BY Acuse.Id-Cliente
                           BY Acuse.Id-Acuse:
   
   IF Acuse.Tipo = "C" THEN NEXT.
   l-Reng = 0.
   FOR EACH PagoAcuse OF Acuse NO-LOCK:
      l-Reng = l-Reng + 1.
      FIND TipoPago OF PagoAcuse NO-LOCK NO-ERROR.
      
      CREATE ttDepDiario.
      ASSIGN ttDepDiario.IdReg = 1.
      
      IF AVAILABLE TipoPago AND PagoAcuse.Id-Tp <> 50 THEN DO:
         
         ASSIGN l-total = PagoAcuse.Importe * PagoAcuse.TC.         
    
         FIND Cliente OF Acuse NO-LOCK NO-ERROR.
         IF PagoAcuse.Id-Moneda > 1 THEN DO:       // RNPC - 2019-08-15
             FIND FIRST Moneda WHERE Moneda.Id-Moneda = PagoAcuse.Id-Moneda NO-LOCK NO-ERROR.
             ASSIGN
               l-totalME = l-totalME + PagoAcuse.Importe * PagoAcuse.TC
               l-HayMonedaEX = TRUE.
         END.
         ELSE ACCUMULATE l-total (TOTAL).
         
         ASSIGN ttDepDiario.IdAcuse = Acuse.Id-Acuse
                ttDepDiario.IdCliente = Acuse.Id-Cliente
                ttDepDiario.RazonSocial = Cliente.RazonSocial
                ttDepDiario.TotalPago = l-total
                ttDepDiario.TipoPago = TipoPago.Descr
                ttDepDiario.IdBanco = PagoAcuse.Id-Banco.
                
      END.
      ELSE IF PagoAcuse.Id-Tp = 65 THEN DO:
         FIND Cliente OF Acuse NO-LOCK NO-ERROR.
         
         ASSIGN ttDepDiario.IdAcuse = Acuse.Id-Acuse
                ttDepDiario.IdCliente = Acuse.Id-Cliente
                ttDepDiario.RazonSocial = Cliente.RazonSocial
                ttDepDiario.TotalPago = 0
                ttDepDiario.TipoPago = "DEVOLUCION".
                
         
      END.
      
      RELEASE ttDepDiario.
   END.   
 END.
 
IF l-HayMonedaEX THEN
    ASSIGN
        l-totalGral = (ACCUM TOTAL l-total) + l-totalME.

 
 CREATE ttTotalDepDiario.
 
 ASSIGN ttTotalDepDiario.IdReg = 1
        ttTotalDepDiario.TotalDep = (ACCUM TOTAL l-total)
        ttTotalDepDiario.TotalME = l-totalME
        ttTotalDepDiario.TotalGral = l-totalGral.

RELEASE ttTotalDepDiario.

END PROCEDURE.

