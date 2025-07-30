@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : teserp024.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : sis10
    Created     : Thu Jul 10 15:08:22 CST 2025
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE TEMP-TABLE ttFolPolDep NO-UNDO
    FIELD FecReg     AS DATE
    FIELD IdCia     AS INTEGER
    FIELD IdPoliza  AS INTEGER
    FIELD Serie      AS CHARACTER
    FIELD anio       AS INTEGER.



/* **********************  Internal Procedures  *********************** */


@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetFechasFolPolDep:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER l-anio AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER l-idcia AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR ttFolPolDep.

IF l-anio  = ? THEN 0.
IF l-idcia = ? THEN 0.
FOR EACH folpoldep 
   WHERE (l-anio  = ? OR folpoldep.anio   = l-anio)
      AND (l-idcia = ? OR folpoldep.Id-Cia = l-idcia) NO-LOCK:
          
     CREATE ttFolPolDep.
        ASSIGN
            ttFolPolDep.FecReg     = folpoldep.FecReg
            ttFolPolDep.IdCia     = folpoldep.Id-Cia
            ttFolPolDep.IdPoliza  = folpoldep.Id-Poliza
            ttFolPolDep.Serie      = folpoldep.Serie
            ttFolPolDep.anio       = folpoldep.anio.     

END.

END PROCEDURE.




