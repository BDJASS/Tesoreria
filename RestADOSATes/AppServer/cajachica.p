@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : cajachica.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : sis6
    Created     : Tue May 06 09:53:18 CST 2025
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

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE DeleteCajaChica:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER IdCajaChica AS INTEGER NO-UNDO.

    DO TRANSACTION:
        FIND FIRST CajaChica WHERE CajaChica.Id-CajaChica = IdCajaChica EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
        IF AVAILABLE CajaChica THEN 
        DO:
            DELETE CajaChica.
        END.
    END.

END PROCEDURE.

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetCajaChica:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER FecRegIni AS DATE FORMAT "99/99/9999" NO-UNDO.
    DEFINE INPUT PARAMETER FecRegFin AS DATE FORMAT "99/99/9999" NO-UNDO.
    DEFINE INPUT PARAMETER FecSolR AS DATE FORMAT "99/99/9999" NO-UNDO.
    DEFINE INPUT PARAMETER Estatus AS LOGICAL NO-UNDO.    
    DEFINE OUTPUT PARAMETER TABLE FOR ttCajaChica.
    
    
    FOR EACH CajaChica WHERE CajaChica.Reembolsado = Estatus 
        AND (IF FecRegIni <> ? THEN CajaChica.FecReg >= FecRegIni ELSE TRUE)
        AND (IF FecRegFin <> ? THEN CajaChica.FecReg >= FecRegFin ELSE TRUE)
        AND (IF FecSolR <> ? THEN CajaChica.FecSol >= FecSolR ELSE TRUE) NO-LOCK: 
     
     
        CREATE ttCajaChica.
     
        ASSIGN 
            ttCajaChica.IdCajaChica = CajaChica.Id-CajaChica
            ttCajaChica.FecReg      = CajaChica.FecReg
            ttCajaChica.FecSol      = CajaChica.FecSol
            ttCajaChica.Documento   = CajaChica.Documento
            ttCajaChica.Proveedor   = CajaChica.Proveedor
            ttCajaChica.Monto       = CajaChica.Monto
            ttCajaChica.SolUser     = CajaChica.SolUSer
            ttCajaChica.Reembolsado = CajaChica.Reembolsado.
     
     
        RELEASE ttCajaChica.
    END.
    
    

END PROCEDURE.

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE PostCajaChica:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER TABLE FOR ttCajaChica.
    DEFINE OUTPUT PARAMETER Mensaje AS CHARACTER NO-UNDO.

    DEF VAR i AS INTEGER NO-UNDO INITIAL 0.    
    
    DEFINE BUFFER bfCajaChica FOR CajaChica.
    FIND FIRST ttCajaChica EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE ttCajaChica THEN RETURN.
    blk1:
    DO WHILE TRUE:
        i = i + 1.
        FIND FIRST bfCajaChica WHERE bfCajaChica.Id-CajaChica = i NO-LOCK NO-ERROR.
        IF AVAILABLE bfCajaChica THEN NEXT.
        ELSE LEAVE blk1.
    END.
        
    DO TRANSACTION:
            
        CREATE CajaChica.
        BUFFER-COPY ttCajaChica TO CajaChica.
        ASSIGN 
            CajaChica.Id-CajaChica = i
            CajaChica.FecReg       = TODAY
            CajaChica.Reembolsado  = FALSE.
                
    END.

    RELEASE CajaChica.
    
END PROCEDURE.

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE PutCajaChica:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER TABLE FOR ttCajaChica.
    
    
    DO TRANSACTION:
        

        FIND FIRST ttCajaChica NO-LOCK NO-ERROR.

        IF AVAILABLE ttCajaChica THEN 
        DO:
            FIND CajaChica WHERE CajaChica.Id-CajaChica = ttCajaChica.IdCajaChica EXCLUSIVE-LOCK NO-ERROR.
        
            ASSIGN 
                CajaChica.Documento = ttCajaChica.Documento
                CajaChica.Proveedor = ttCajaChica.Proveedor
                CajaChica.Monto     = ttCajaChica.Monto
                CajaChica.SolUser   = ttCajaChica.SolUser.
     
        END.
    
        RELEASE CajaChica. 
          
    END.

END PROCEDURE.

