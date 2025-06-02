@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : ctlcajas.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : sis6
    Created     : Thu Apr 10 09:05:52 CST 2025
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

DEFINE TEMP-TABLE ttCtlCajas
    FIELD IdCaja       AS INTEGER
    FIELD Descr        AS CHARACTER
    FIELD Turno        AS INTEGER
    FIELD FecOper      AS DATE
    FIELD FecApertura  AS DATE
    FIELD HoraApertura AS CHARACTER
    FIELD FolioIni     AS INTEGER
    FIELD FolioFin     AS INTEGER.
    
DEFINE VAR l-tiempo-c AS CHARACTER NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetCtlCajas:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER TABLE FOR ttCtlCajas.


    FOR EACH CtlCaja WHERE CtlCaja.FecCierre = ? AND CtlCaja.Id-caja > 0 NO-LOCK:

        ASSIGN 
            l-tiempo-c = SUBSTRING(STRING(CtlCaja.HoraApertura,'999999'),1,2) + ':' +
                    SUBSTRING(STRING(CtlCaja.HoraApertura,'999999'),3,2).
        FIND Caja WHERE Caja.Id-caja = CtlCaja.Id-caja NO-LOCK NO-ERROR.                 
        CREATE ttCtlCajas.
        ASSIGN 
            ttCtlCajas.IdCaja       = CtlCaja.Id-caja
            ttCtlCajas.Descr        = Caja.Descr
            ttCtlCajas.Turno        = CtlCaja.Turno
            ttCtlCajas.FecOper      = CtlCaja.FecOper
            ttCtlCajas.FecApertura  = CtlCaja.FecApertura
            ttCtlCajas.HoraApertura = l-tiempo-c
            ttCtlCajas.FolioIni     = CtlCaja.FolioIni
            ttCtlCajas.FolioFin     = CtlCaja.FolioFin.   
            
        RELEASE ttCtlCajas.                 

    END.
    
    

END PROCEDURE.

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE PostCtlCajas:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER IdCaja AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER Turno AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER oMensaje AS CHARACTER NO-UNDO.

    DEFINE BUFFER b-CtlCaja FOR CtlCaja.
    DO TRANSACTION:
        
        FIND LAST CtlCaja WHERE CtlCaja.Id-Caja = IdCaja
            AND CtlCaja.FecOper = TODAY
            AND CtlCaja.Turno = Turno
            AND CtlCaja.FecCierre = ? NO-LOCK NO-ERROR.
        IF NOT AVAILABLE CtlCaja THEN 
        DO:        
            
            FIND LAST b-CtlCaja WHERE b-CtlCaja.id-caja = IdCaja NO-LOCK NO-ERROR.
            CREATE CtlCaja.
            ASSIGN
                CtlCaja.id-caja      = IdCaja
                CtlCaja.Turno        = Turno            
                CtlCaja.FecOper      = TODAY
                CtlCaja.FecApertura  = TODAY
                l-tiempo-c           = STRING(TIME,'HH:MM:SS')
                CtlCaja.HoraApertura = INTEGER(SUBSTRING(l-tiempo-c,1,2) + SUBSTRING(l-tiempo-c,4,2) + SUBSTRING(l-tiempo-c,7,2))
                CtlCaja.FolioIni     = IF AVAILABLE b-CtlCaja THEN b-CtlCaja.FolioFin ELSE 0
                CtlCaja.FolioFin     = CtlCaja.FolioIni
                CtlCaja.Dotacion     = 0.

        END.
    END.
END PROCEDURE.

