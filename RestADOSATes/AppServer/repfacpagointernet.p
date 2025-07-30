@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : repfacpagointernet.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : sis6
    Created     : Fri May 23 15:44:08 CST 2025
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

DEFINE TEMP-TABLE ttFacturasInt
    FIELD IdRemision   AS CHARACTER         
    FIELD IdPedido     AS CHARACTER
    FIELD IdReq        AS CHARACTER
    FIELD FecReg       AS DATE
    FIELD FecPed       AS DATE
    FIELD IdCliente    AS INTEGER    
    FIELD RazonSocial  AS CHARACTER
    FIELD IdEntrega    AS INTEGER
    FIELD EntregaDscr  AS CHARACTER
    FIELD TotalFactura AS DECIMAL
    FIELD TotalPedido  AS DECIMAL
    FIELD Sobrante     AS DECIMAL
    FIELD PagInfo      AS CHARACTER.
    
DEFINE VARIABLE cFechaISOIni AS CHARACTER NO-UNDO.
DEFINE VARIABLE dFechaIni    AS DATE      NO-UNDO.
DEFINE VARIABLE cFechaISOFin AS CHARACTER NO-UNDO.
DEFINE VARIABLE dFechaFin    AS DATE      NO-UNDO.
    
DEFINE VARIABLE l-Pedidos    AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-ReqCte     AS CHARACTER FORMAT "X(6)" LABEL "ReqCte". 
DEFINE VARIABLE l-PagInfo    LIKE Pedido.PagInfo NO-UNDO.
DEFINE VARIABLE l-FecPed     AS DATE      NO-UNDO.
DEFINE VARIABLE l-TotPed     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE l-Sobrante   AS DECIMAL   NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetRepFacPagoInt:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER l-FechaIni AS CHARACTER NO-UNDO.   
    DEFINE INPUT PARAMETER l-FechaFin AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttFacturasInt.


    cFechaISOIni = SUBSTRING(l-FechaIni, 1, 10). 

    cFechaISOIni = SUBSTRING(cFechaISOIni, 9, 2) + "/" +  /* DD */ 
        SUBSTRING(cFechaISOIni, 6, 2) + "/" +  /* MM */            
        SUBSTRING(cFechaISOIni, 1, 4).         /* YYYY */

    dFechaIni = DATE(cFechaISOIni).

    cFechaISOFin = SUBSTRING(l-FechaFin, 1, 10). 

    cFechaISOFin = SUBSTRING(cFechaISOFin, 9, 2) + "/" +  /* DD */ 
        SUBSTRING(cFechaISOFin, 6, 2) + "/" +  /* MM */            
        SUBSTRING(cFechaISOFin, 1, 4).         /* YYYY */

    dFechaFin = DATE(cFechaISOFin).


    FOR EACH Remision WHERE Remision.Pagada = FALSE 
        AND Remision.TipoVenta = 2 AND Remision.FecReg >= dFechaIni
        AND Remision.FecReg <= dFechaFin
        AND Remision.Id-Vendedor = "0100"
        NO-LOCK BY Remision.Id-Remision:

        IF Remision.Feccanc <> ? THEN NEXT.
        IF remision.tipoventa = 1 AND remision.id-Remis BEGINS "@" THEN NEXT.
        IF remision.tipoventa = 2 AND CAN-DO("0,1,2,3,4,5,6,7,8,9",SUBSTRING(remision.id-remis,7,1)) THEN NEXT.
    
        FIND Entrega OF Remision NO-LOCK NO-ERROR.

        IF NUM-ENTRIES(Remision.Pedidos) > 1 THEN
            ASSIGN l-Pedidos = SUBSTRING(Remision.Pedidos,1,7) + "*".
        ELSE
            ASSIGN l-Pedidos = Remision.Pedidos.

        FIND FIRST Pedido WHERE Pedido.Id-Pedido = SUBSTRING(l-Pedidos,1,7) NO-LOCK NO-ERROR.
        ASSIGN 
            l-PagInfo = "".
        IF AVAILABLE Pedido THEN 
        DO: 
            ASSIGN 
                l-PagInfo = REPLACE(Pedido.PagInfo,"+"," ").
            ASSIGN 
                l-PagInfo = REPLACE(l-PagInfo,"$"," ").
        END.
    
        ASSIGN 
            l-FecPed   = ?
            l-TotPed   = 0
            l-Sobrante = 0
            l-ReqCte   = "".
        FOR EACH Pedido WHERE Pedido.Id-Factura = Remision.Id-Remision
            AND CAN-DO(Pedido.Id-Pedido, Remision.Pedidos) NO-LOCK:
            IF l-FecPed = ? THEN 
                ASSIGN l-FecPed = Pedido.FecReg.
            ASSIGN 
                l-TotPed = l-TotPed + Pedido.Tot
                l-ReqCte = l-ReqCte + (IF l-ReqCte = "" THEN "" ELSE " ") + TRIM(Pedido.ReqCte).                 
        END.
        ASSIGN 
            l-Sobrante = l-TotPed - Remision.Tot.
                
        IF l-PagInfo MATCHES "*OPENPAY*" THEN l-PagInfo = "OPENPAY".
        ELSE IF l-PagInfo MATCHES "*TARJETA*" THEN l-PagInfo = "BANAMEX".
            ELSE IF l-PagInfo MATCHES "*PAYPAL*" THEN l-PagInfo = "PAYPAL".
                ELSE l-PagInfo = "SANTANDER".
                  

        CREATE ttFacturasInt.
            
        ASSIGN 
            ttFacturasInt.IdRemision   = Remision.Id-Remision
            ttFacturasInt.IdPedido     = l-Pedidos
            ttFacturasInt.IdReq        = l-ReqCte
            ttFacturasInt.FecReg       = Remision.FecReg
            ttFacturasInt.FecPed       = l-FecPed
            ttFacturasInt.IdCliente    = Remision.Id-Cliente
            ttFacturasInt.RazonSocial  = Remision.RazonSocial
            ttFacturasInt.IdEntrega    = Remision.Id-Entrega
            ttFacturasInt.EntregaDscr  = IF AVAILABLE Entrega THEN Entrega.Descr ELSE ""
            ttFacturasInt.TotalFactura = Remision.Tot
            ttFacturasInt.TotalPedido  = l-TotPed
            ttFacturasInt.Sobrante     = l-Sobrante
            ttFacturasInt.PagInfo      = l-PagInfo.
            
        RELEASE ttFacturasInt.
              
    END.

END PROCEDURE.

