@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : repcajachica.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : sis6
    Created     : Thu May 15 09:17:42 CST 2025
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

DEFINE TEMP-TABLE ttCajaChica
    FIELD IdCajaChica AS INTEGER
    FIELD FecReg      AS DATE
    FIELD FecSol      AS DATE
    FIELD Documento   AS CHARACTER
    FIELD Proveedor   AS CHARACTER    
    FIELD Monto       AS DECIMAL
    FIELD SolUser     AS CHARACTER
    FIELD Reembolsado AS LOGICAL.
    
    DEFINE VARIABLE cFechaISOIni AS CHARACTER NO-UNDO.
DEFINE VARIABLE dFechaIni    AS DATE      NO-UNDO.
DEFINE VARIABLE cFechaISOFin AS CHARACTER NO-UNDO.
DEFINE VARIABLE dFechaFin    AS DATE      NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetCajaChicaRep:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER FecIni    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER FecFin    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER Estatus   AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER PorFecha  AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttCajaChica.
    
    
    cFechaISOIni = SUBSTRING(FecIni, 1, 10). 

    cFechaISOIni = SUBSTRING(cFechaISOIni, 9, 2) + "/" +  /* DD */ 
        SUBSTRING(cFechaISOIni, 6, 2) + "/" +  /* MM */            
        SUBSTRING(cFechaISOIni, 1, 4).         /* YYYY */

    dFechaIni = DATE(cFechaISOIni).

    cFechaISOFin = SUBSTRING(FecFin, 1, 10). 

    cFechaISOFin = SUBSTRING(cFechaISOFin, 9, 2) + "/" +  /* DD */ 
        SUBSTRING(cFechaISOFin, 6, 2) + "/" +  /* MM */            
        SUBSTRING(cFechaISOFin, 1, 4).         /* YYYY */

    dFechaFin = DATE(cFechaISOFin).

    FOR EACH CajaChica NO-LOCK
        WHERE (
        (Estatus = 1 AND CajaChica.Reembolsado = TRUE) OR
        (Estatus = 2 AND CajaChica.Reembolsado = FALSE) OR
        (Estatus = 0)
        )
        AND (
        PorFecha AND 
        (CajaChica.FecSol >= dFechaIni AND CajaChica.FecSol <= dFechaFin)
        OR
        NOT PorFecha AND 
        (CajaChica.FecReg >= dFechaIni AND CajaChica.FecReg <= dFechaFin)
        ):

        CREATE ttCajaChica.
        ASSIGN 
            ttCajaChica.IdCajaChica = CajaChica.Id-CajaChica
            ttCajaChica.FecReg      = CajaChica.FecReg
            ttCajaChica.FecSol      = CajaChica.FecSol
            ttCajaChica.Documento   = CajaChica.Documento
            ttCajaChica.Proveedor   = CajaChica.Proveedor
            ttCajaChica.Monto       = CajaChica.Monto
            ttCajaChica.SolUser     = CajaChica.SolUser
            ttCajaChica.Reembolsado = CajaChica.Reembolsado.
        RELEASE ttCajaChica.
    END.

END PROCEDURE.

