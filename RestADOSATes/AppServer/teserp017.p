@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*------------------------------------------------------------------------
    File        : teserp017.p
    Purpose     : HU07   Reporte Relacion Cheque para Deposito

    Author(s)   : sis10
    Created     : Fecha actual   
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW. /* Manejo de errores global */

/* **********************  Internal Procedures  *********************** */



/* ***************************  Main Procedure *************************** */



DEF TEMP-TABLE ttRepRelaCheque NO-UNDO
    FIELD NoCheque LIKE Saldo.Cargo
    FIELD Banco    LIKE Saldo.Dias
    FIELD NomBanco AS CHAR
    FIELD Importe  LIKE Saldo.Saldo.
       
DEF VAR l-tot           AS DECI FORMAT ">>>,>>>,>>9.99"         NO-UNDO.
DEF VAR l-cf            AS INTE                                 NO-UNDO.
DEF VAR l-sec           AS INTE.
DEFINE VARIABLE cFechaISOIni AS CHARACTER NO-UNDO.
DEFINE VARIABLE dFechaIni    AS DATE      NO-UNDO.

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetRepRelacionCheque:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipFecha AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttRepRelaCheque.
    
    
    cFechaISOIni = SUBSTRING(ipFecha, 1, 10). 

    cFechaISOIni = SUBSTRING(cFechaISOIni, 9, 2) + "/" +  /* DD */ 
        SUBSTRING(cFechaISOIni, 6, 2) + "/" +  /* MM */            
        SUBSTRING(cFechaISOIni, 1, 4).         /* YYYY */

    dFechaIni = DATE(cFechaISOIni).     

    
    
    /* Variables locales */
    DEFINE VARIABLE lcTipoCheque AS CHARACTER NO-UNDO INIT "RelacCheque".
    DEFINE VARIABLE lcLocal      AS CHARACTER NO-UNDO INIT "Local".
    DEFINE VARIABLE lcForaneo    AS CHARACTER NO-UNDO INIT "Foraneo".
    
    /* Limpiar tabla temporal */
    EMPTY TEMP-TABLE ttRepRelaCheque.
    
    /* Buscar registros en Saldo que coincidan con la fecha y tipo de cheque */
    FOR EACH Saldo NO-LOCK
        WHERE Saldo.FecReg = dFechaIni
          AND Saldo.Doc = lcTipoCheque
          AND Saldo.Tty =  "Local" :    
        
        FIND FIRST Banco WHERE Banco.Id-Banco = Saldo.Dias NO-LOCK NO-ERROR.
        CREATE ttRepRelaCheque.
        ASSIGN
            ttRepRelaCheque.NoCheque   = Saldo.Cargo
            ttRepRelaCheque.Banco      = Saldo.Dias
            ttRepRelaCheque.NomBanco   = IF AVAILABLE Banco THEN Banco.Nombre ELSE ""
            ttRepRelaCheque.Importe    = Saldo.Saldo.  
    END.
    

END PROCEDURE.