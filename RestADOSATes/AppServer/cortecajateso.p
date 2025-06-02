@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : cortecajateso.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : sis6
    Created     : Tue May 20 09:56:56 CST 2025
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.
    
DEFINE TEMP-TABLE ttCorteCaja
    FIELD IdCaja        AS INTEGER
    FIELD Turno         AS INTEGER
    FIELD FecOper       AS DATE
    FIELD TotalTDebito  AS DECIMAL
    FIELD TotalTCredito AS DECIMAL
    FIELD Cheques       AS DECIMAL
    FIELD Efectivo      AS DECIMAL
    FIELD Devoluciones  AS DECIMAL
    FIELD Depositos     AS DECIMAL
    FIELD Anticipos     AS DECIMAL
    FIELD Vales         AS DECIMAL
    FIELD Retiros       AS DECIMAL
    FIELD Faltantes     AS DECIMAL
    FIELD TarjetaDebitoM AS DECIMAL
    FIELD TarjetaCreditoM AS DECIMAL
    FIELD ChequesM       AS DECIMAL
    FIELD EfectivoM      AS DECIMAL
    FIELD DevolucionesM  AS DECIMAL
    FIELD DepositosM     AS DECIMAL
    FIELD AnticiposM     AS DECIMAL
    FIELD ValesM         AS DECIMAL
    FIELD RetirosM       AS DECIMAL
    FIELD FaltantesM     AS DECIMAL
    FIELD IdDepto       AS CHARACTER.
    

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetCorteTesoreria:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER l-Fecha AS DATE FORMAT "99/99/9999" NO-UNDO.
    DEFINE INPUT PARAMETER idSucursal AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttCorteCaja.
    
    FOR EACH ModifCorteCaja WHERE ModifCorteCaja.Id-Depto = idSucursal AND ModifCorteCaja.FecOper = l-Fecha NO-LOCK:
        
        CREATE ttCorteCaja.
        BUFFER-COPY ModifCorteCaja TO ttCorteCaja.
        ASSIGN ttCorteCaja.IdCaja = ModifCorteCaja.Id-caja
               ttCorteCaja.IdDepto = ModifCorteCaja.Id-Depto.
               
               
               
    END.    
    
    
    RELEASE ttCorteCaja.
    

END PROCEDURE.

