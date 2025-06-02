@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : catmovbancario.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : sis6
    Created     : Mon Apr 07 09:38:39 CST 2025
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.
DEFINE TEMP-TABLE ttTipoMB BEFORE-TABLE bttTipoMB
    FIELD IdTipoMB  AS INTEGER
    FIELD Descr     AS CHARACTER
    FIELD TipoCC    AS LOGICAL
    FIELD NomCuenta AS CHARACTER
    FIELD IdCta     AS INTEGER
    FIELD IdSCta    AS INTEGER
    FIELD IdSSCta   AS INTEGER
    FIELD IdSSSCta  AS INTEGER.

/* ********************  Preprocessor Definitions  ******************** */



/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetTipoMB:
    DEFINE INPUT PARAMETER IdTipoMB AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttTipoMB.

    
    EMPTY TEMP-TABLE ttTipoMB.

    FOR EACH TipoMB 
        WHERE (IF IdTipoMB <> 0 AND IdTipoMB <> ? THEN TipoMB.Id-TipoMB = IdTipoMB ELSE TRUE) 
        NO-LOCK:

        CREATE ttTipoMB.
        ASSIGN 
            ttTipoMB.IdTipoMB = TipoMB.Id-TipoMB
            ttTipoMB.Descr    = TipoMB.Descr
            ttTipoMB.TipoCC   = TipoMB.TipoCC.
            
        FIND FIRST Empresa NO-LOCK NO-ERROR.           
            FIND Cuenta WHERE Cuenta.Id-Cia     = Empresa.Id-Cia  AND
                Cuenta.Id-Cta     = TipoMB.id-cta   AND
                Cuenta.Id-SCta    = TipoMB.id-scta  AND
                Cuenta.Id-SSCta   = TipoMB.id-sscta AND
                Cuenta.Id-SSSCta  = TipoMB.id-ssscta
                NO-LOCK NO-ERROR.
            IF AVAILABLE cuenta THEN
                ttTipoMB.NomCuenta = cuenta.descr.
            ELSE
                ttTipoMB.NomCuenta = 'NO DISP.'.

            ASSIGN                
                ttTipoMB.IdCta    = tipomb.id-cta
                ttTipoMB.IdSCta   = tipomb.id-scta
                ttTipoMB.IdSSCta  = tipomb.id-sscta
                ttTipoMB.IdSSSCta = tipomb.id-ssscta.    

        RELEASE ttTipoMB.
    END.
        
END PROCEDURE.
