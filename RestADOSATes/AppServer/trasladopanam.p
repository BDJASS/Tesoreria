@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : trasladopanam.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : sis6
    Created     : Sat May 17 11:23:56 CST 2025
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

DEFINE TEMP-TABLE ttPanamCorte
    FIELD IdPanam      AS INTEGER    
    FIELD Monto        AS DECIMAL
    FIELD FecCorte     AS DATE
    FIELD FecReg       AS DATE
    FIELD Miles        AS INTEGER
    FIELD Traslado     AS DECIMAL    
    FIELD Verificacion AS DECIMAL
    FIELD IdDepto      AS CHARACTER
    FIELD TotalPanam   AS DECIMAL.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetTrasladoPanam:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER FechaCorte AS DATE FORMAT "99/99/9999" NO-UNDO.
    DEFINE INPUT PARAMETER IdDepto AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttPanamCorte.
    
    DEFINE VARIABLE TotalPanam AS DECIMAL NO-UNDO.
    
    FOR EACH Panamericano WHERE Panamericano.FecCorte = FechaCorte NO-LOCK:
        TotalPanam = TotalPanam + Panamericano.Monto.
    END.
    

    FIND FIRST Panamericano WHERE Panamericano.Id-Depto = IdDepto 
                              AND Panamericano.FecCorte = FechaCorte
        NO-LOCK NO-ERROR.
                          
                          
    IF AVAILABLE Panamericano THEN 
    DO:
        CREATE ttPanamCorte.
        ASSIGN 
            ttPanamCorte.IdPanam      = Panamericano.Id-Panam
            ttPanamCorte.Monto        = Panamericano.Monto
            ttPanamCorte.FecCorte     = Panamericano.FecCorte
            ttPanamCorte.FecReg       = Panamericano.FecReg
            ttPanamCorte.Miles        = Panamericano.Miles
            ttPanamCorte.Traslado     = Panamericano.Traslado
            ttPanamCorte.Verificacion = Panamericano.Verificacion
            ttPanamCorte.IdDepto      = Panamericano.Id-Depto
            ttPanamCorte.TotalPanam   = TotalPanam.
               
        RELEASE ttPanamCorte.
    END.

END PROCEDURE.

