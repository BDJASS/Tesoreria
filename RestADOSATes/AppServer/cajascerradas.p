@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : cajascerradas.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : sis6
    Created     : Thu Apr 10 11:31:13 CST 2025
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

DEFINE TEMP-TABLE ttCajas
    FIELD IdCaja AS INTEGER
    FIELD Descr  AS CHARACTER
    FIELD Turno AS INTEGER.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetCajasCerradas:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER TABLE FOR ttCajas.


    FOR EACH Caja NO-LOCK:
    
        FIND FIRST CtlCaja WHERE CtlCaja.Id-caja = Caja.Id-caja
            AND CtlCaja.FecCierre = ? NO-LOCK NO-ERROR.
    
        IF AVAILABLE CtlCaja THEN NEXT.
        
        FIND LAST CtlCaja WHERE CtlCaja.Id-caja = Caja.Id-caja
                             AND CtlCaja.FecOper = TODAY
                             AND CtlCaja.FecCierre <> ? NO-LOCK NO-ERROR. 
        
        DO:
        
            CREATE ttCajas.
            ASSIGN 
                ttCajas.IdCaja = Caja.Id-caja
                ttCajas.Descr  = Caja.Descr
                ttCajas.Turno  = IF AVAILABLE CtlCaja THEN CtlCaja.Turno + 1 ELSE 1.
            
            RELEASE ttCajas.
        
        END.    
    
    END.



END PROCEDURE.

