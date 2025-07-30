@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*------------------------------------------------------------------------
    File        : teserp025.p
    Purpose     : REPORTE DE FACTURAS DE CONTADO FALTANTES DE PAGO NORMALES-- REMISION.
                  tesc0970
    Author(s)   : sis10
    Created     : Fecha actual   
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW. /* Manejo de errores global */

/* **********************  Internal Procedures  *********************** */



/* ***************************  Main Procedure *************************** */

/*
  Empresa  : Consultoria en Informatica Ejecutiva, S.A. de C.V.
  Modulo   : Tesoreria
  Programa : tesc0971.p
  Funcion  : Reporte de Facturas Faltantes de Pago
*/
// {sia00000.var}

DEF STREAM s-Salida.
DEF    VAR      l-reporte  AS CHAR      NO-UNDO.

DEF    VAR      l-veces    AS INT       NO-UNDO INITIAL 0.
DEF    VAR      l-linea    AS CHAR.
DEF    VAR      l-Camion   AS INTEGER   FORMAT "zz9" NO-UNDO.
DEF    VAR      l-FolEmb   LIKE Embarque.Id-Embarque NO-UNDO.
DEF    VAR      l-FecEmb   AS DATE      FORMAT '99/99/9999' NO-UNDO.
DEF    VAR      l-HorEmb   AS CHAR      FORMAT "x(5)" NO-UNDO.
DEF    VAR      l-CantEmb  AS INTEGER   NO-UNDO.
DEF    VAR      l-Estatus  LIKE DetEmbarque.Estatus NO-UNDO.  
DEF    VAR      l-SVeces   AS CHAR      NO-UNDO FORMAT "x(2)".
DEF    VAR      l-Aster    AS CHAR      NO-UNDO FORMAT "x(1)".
DEF    VAR      l-Prim     AS LOGICAL   NO-UNDO.
DEF    VAR      l-NOExis   AS LOGICAL   NO-UNDO.
DEF    VAR      l-ListDev  AS CHAR      NO-UNDO.
DEF    VAR      l-TotDev   AS DECIMAL   NO-UNDO.
DEF    VAR      l-opcion   AS CHAR      EXTENT 2 FORMAT "x(10)" 
    INITIAL ["Normal", "Condensado"] NO-UNDO.
DEFINE VARIABLE l-opcion2  AS CHARACTER EXTENT 2 FORMAT "x(10)" INITIAL ["Remisiones", "Facturas"] NO-UNDO.
DEFINE VARIABLE l-PagInfo  LIKE Pedido.PagInfo NO-UNDO.
DEFINE VARIABLE l-FecPed   AS DATE      NO-UNDO.
DEFINE VARIABLE l-TotPed   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE l-Sobrante AS DECIMAL   NO-UNDO.
DEFINE VARIABLE l-Pedidos  AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-ReqCte   AS CHARACTER FORMAT "X(6)" LABEL "ReqCte".   

DEF TEMP-TABLE ttFacNormalRemision  
    FIELD IdRemision         LIKE Remision.Id-Remision
    FIELD Pedido             LIKE l-Pedidos
    FIELD FecReg             LIKE Remision.FecReg
    FIELD IdCliente          LIKE Remision.Id-Cliente
    FIELD RazonSocial        LIKE Remision.RazonSocial FORMAT 'x(27)'
    FIELD IdVendedor         LIKE Remision.Id-Vendedor
    FIELD Vendedor           LIKE empleado.Nombre
    FIELD IdEntrega          LIKE Remision.Id-Entrega
    FIELD Descr              LIKE Entrega.Descr
    FIELD Tot                LIKE Remision.Tot
    FIELD FolEmb             LIKE l-FolEmb
    FIELD Camion             LIKE l-Camion
    FIELD FecEmb             LIKE l-FecEmb
    FIELD HorEmb             LIKE l-HorEmb
    FIELD Estatus            AS CHAR
    FIELD InformacionDelPago AS CHARACTER FORMAT 'x(27)'
    INDEX Idx-Def IdRemision.


DEF TEMP-TABLE w-Datos
    FIELD Id-Remision LIKE Remision.Id-Remision
    FIELD Pedido      LIKE l-Pedidos
    FIELD FecReg      LIKE Remision.FecReg
    FIELD Id-Cliente  LIKE Remision.Id-Cliente
    FIELD RazonSocial LIKE Remision.RazonSocial FORMAT 'x(27)'
    FIELD Id-Vendedor LIKE Remision.Id-Vendedor
    FIELD Id-Entrega  LIKE Remision.Id-Entrega
    FIELD Descr       LIKE Entrega.Descr
    FIELD Tot         LIKE Remision.Tot
    FIELD Camion      LIKE l-Camion
    FIELD FolEmb      LIKE l-FolEmb
    FIELD FecEmb      LIKE l-FecEmb
    FIELD HorEmb      LIKE l-HorEmb
    FIELD Estatus     LIKE l-Estatus
    FIELD CantEmb     LIKE l-CantEmb 
    FIELD Corte       AS CHAR      FORMAT "x(1)"
    FIELD Pedidos     AS CHARACTER FORMAT 'x(27)'
    FIELD DevTot      AS LOGICAL 
    INDEX Idx-Def Id-Remision.


DEF TEMP-TABLE w-CEmba
    FIELD Id-Remision LIKE Remision.Id-Remision
    FIELD Pedido      LIKE l-Pedidos
    FIELD FecReg      LIKE Remision.FecReg
    FIELD Id-Cliente  LIKE Remision.Id-Cliente
    FIELD RazonSocial LIKE Remision.RazonSocial FORMAT 'x(27)'
    FIELD Id-Vendedor LIKE Remision.Id-Vendedor
    FIELD Id-Entrega  LIKE Remision.Id-Entrega
    FIELD Descr       LIKE Entrega.Descr
    FIELD Tot         LIKE Remision.Tot
    FIELD Camion      LIKE l-Camion
    FIELD FolEmb      LIKE l-FolEmb
    FIELD FecEmb      LIKE l-FecEmb
    FIELD HorEmb      LIKE l-HorEmb
    FIELD Estatus     LIKE l-Estatus
    FIELD CantEmb     LIKE l-CantEmb 
    FIELD Corte       AS CHAR      FORMAT "x(1)"
    FIELD Aster       AS CHAR      FORMAT "x(1)"
    FIELD Pedidos     AS CHARACTER FORMAT 'x(27)'
    FIELD DevTot      AS LOGICAL 
    INDEX Idx-Def Id-Remision.


DEFINE DATASET dsRep FOR 
    w-CEmba,
    w-Datos 
 
    DATA-RELATION SucDetalle FOR w-Datos, w-CEmba
    RELATION-FIELDS (Id-Remision, Id-Remision).
    
DEFINE TEMP-TABLE ttFacturas NO-UNDO
    FIELD Id-Factura AS CHARACTER   
    FIELD Pedidos    AS CHARACTER   
    FIELD FecReg     AS DATE    . 

FORM
    Factura.Id-Factura  COLUMN-LABEL 'Fact'
    l-Pedidos            COLUMN-LABEL 'Pedido'
    Factura.FecReg       COLUMN-LABEL 'Fecha' FORMAT '99/99/99'
    Factura.id-Cliente   COLUMN-LABEL 'Cte'
    Factura.RazonSocial  COLUMN-LABEL 'Razon Social' FORMAT 'x(27)'
    Factura.Id-Vendedor  COLUMN-LABEL 'Vend'
    Factura.Id-Entrega         COLUMN-LABEL 'En'
    Entrega.Descr        COLUMN-LABEL 'Entrega' FORMAT 'x(15)'
    Factura.Tot          COLUMN-LABEL 'Total' FORMAT "zzzz,zz9.99"
    SPACE(0)
    l-Aster               NO-LABEL
    l-FolEmb              COLUMN-LABEL 'Fol.Emb' FORMAT 'x(7)'
    l-FecEmb              COLUMN-LABEL 'Fec.Emb.' FORMAT '99/99/99'
    l-HorEmb              COLUMN-LABEL 'Hora'
    l-Estatus             COLUMN-LABEL 'E'
    SPACE(0)
    l-SVeces              NO-LABEL
    w-Datos.DevTot        COLUMN-LABEL 'DT' FORMAT 'Si/No'
    WITH FRAME f-Detalle OVERLAY DOWN WIDTH 134.

FORM 
    Factura.id-Factura  COLUMN-LABEL 'Remis'
    l-Pedidos            COLUMN-LABEL 'Pedido'
    Factura.FecReg       COLUMN-LABEL 'Fecha' FORMAT '99/99/99'
    Factura.id-Cliente   COLUMN-LABEL 'Cte'
    Factura.RazonSocial  COLUMN-LABEL 'Razon Social' FORMAT 'x(16)'
    Factura.Id-Vendedor  COLUMN-LABEL 'Vend'
    Factura.Id-Entrega         COLUMN-LABEL 'En'
    Entrega.Descr         COLUMN-LABEL 'Entrega' FORMAT 'x(7)'
    Factura.Tot          COLUMN-LABEL 'Total' FORMAT "zzzz,zz9.99"
    w-Datos.DevTot        COLUMN-LABEL 'DT' FORMAT 'Si/No' SKIP
    l-linea FORMAT "x(130)" NO-LABEL
    WITH FRAME f-Detallecond OVERLAY DOWN WIDTH 134.



DEFINE VARIABLE cFechaISOIni AS CHARACTER NO-UNDO.
DEFINE VARIABLE dFechaIni    AS DATE      NO-UNDO.
DEFINE VARIABLE cFechaISOFin AS CHARACTER NO-UNDO. 
DEFINE VARIABLE dFechaFin    AS DATE      NO-UNDO.


DEFINE VARIABLE vEstatus     AS CHARACTER NO-UNDO.

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE RepFacNormalRemision:        
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER l-FecIni     AS CHARACTER NO-UNDO.
    DEF INPUT PARAMETER l-FecFin     AS CHARACTER NO-UNDO.
    DEF VARIABLE l-index AS INTE.
    DEFINE OUTPUT PARAMETER TABLE FOR ttFacNormalRemision.
    
    
    cFechaISOIni = SUBSTRING(l-FecIni, 1, 10). 

    cFechaISOIni = SUBSTRING(cFechaISOIni, 9, 2) + "/" +  /* DD */ 
        SUBSTRING(cFechaISOIni, 6, 2) + "/" +  /* MM */            
        SUBSTRING(cFechaISOIni, 1, 4).         /* YYYY */

    dFechaIni = DATE(cFechaISOIni).   

    cFechaISOFin = SUBSTRING(l-FecFin, 1, 10). 

    cFechaISOFin = SUBSTRING(cFechaISOFin, 9, 2) + "/" +  /* DD */ 
        SUBSTRING(cFechaISOFin, 6, 2) + "/" +  /* MM */            
        SUBSTRING(cFechaISOFin, 1, 4).         /* YYYY */

    dFechaFin = DATE(cFechaISOFin).
 

    FOR EACH w-Datos EXCLUSIVE-LOCK:
        DELETE w-Datos.
    END.
    FOR EACH w-CEmba EXCLUSIVE-LOCK:
        DELETE w-CEmba.
    END.   

    ASSIGN 
        l-index = 1. // SELECCION PARA FACTURAS ASI COMO MENCIONO NJCC 
    
    
    FOR EACH Remision WHERE Remision.Pagada = FALSE 
        AND Remision.TipoVenta = 2 AND Remision.FecReg >= dFechaIni
        AND Remision.FecReg <= dFechaFin
        AND (IF l-Index = 4 THEN Remision.Id-Vendedor = "0100" ELSE (IF l-Index = 1 THEN Remision.Id-Vendedor <> "0100" ELSE TRUE))
        NO-LOCK BY Remision.Id-Remision:

        IF Remision.Feccanc <> ? THEN NEXT.
        IF remision.tipoventa = 1 AND remision.id-Remis BEGINS "@" THEN NEXT.
        IF remision.tipoventa = 2 AND CAN-DO("0,1,2,3,4,5,6,7,8,9",SUBSTRING(remision.id-remis,7,1)) THEN NEXT.
        IF NOT Remision.Id-Remision MATCHES "*J" AND l-Index = 3 THEN NEXT. 
        PAUSE 0.
        FIND Entrega OF Remision NO-LOCK NO-ERROR.

        ASSIGN 
            l-linea = FILL(" ", 82) + FILL("_", 55).
        l-Camion = 0.
        l-FolEmb = "".
        l-FecEmb = ?.
        l-HorEmb = "".
        l-NOExis = TRUE.
        FOR EACH EstPedido WHERE EstPedido.Id-Factura = Remision.Id-Remision
            NO-LOCK BREAK BY EstPedido.FecEmb DESCENDING BY EstPedido.HorEmb DESCENDING:
            IF FIRST-OF(EstPedido.FecEmb) THEN 
            DO:
                l-Camion = IF EstPedido.Id-Embarque > "" THEN EstPedido.Id-Camion ELSE 0.
                l-FolEmb = EstPedido.Id-Embarque.
                l-FecEmb = EstPedido.FecEmb.
                l-HorEmb = IF EstPedido.HorEmb <> 0
                    THEN STRING(EstPedido.HorEmb,"hh:mm") ELSE "".
                l-NOExis = FALSE.
                LEAVE.
            END.
        END.

        l-Estatus = 0.
        l-CantEmb = 0.
        FOR EACH DetEmbarque WHERE DetEmbarque.Id-Factura = Remision.Id-Remision
            NO-LOCK BREAK BY DetEmbarque.Id-Embarque:
            
            l-Estatus = DetEmbarque.Estatus.
            IF DetEmbarque.Estatus = 2 THEN
                l-CantEmb = l-CantEmb + 1.
            ELSE IF DetEmbarque.Estatus = 9 AND DetEmbarque.Motivo = 9 THEN
                    l-CantEmb = 9.
    
            IF LAST-OF(DetEmbarque.Id-Embarque) AND l-NOExis = TRUE THEN 
            DO:
                FIND Embarque WHERE Embarque.Id-Embarque = DetEmbarque.Id-Embarque
                    NO-LOCK NO-ERROR.
                IF AVAILABLE Embarque THEN 
                DO:
                    l-Camion = Embarque.Id-Camion.
                    l-FolEmb = Embarque.Id-Embarque.
                    l-FecEmb = Embarque.FecReg.
                    l-HorEmb = STRING(Embarque.HorReg,"hh:mm").
                END.
            END.
        END.

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
    
        IF l-index = 1 THEN 
        DO:
            
            FIND FIRST Vendedor WHERE Vendedor.Id-Vendedor = Remision.Id-Vendedor
                NO-LOCK NO-ERROR.
            IF AVAILABLE Vendedor THEN 
            DO:
                FIND FIRST Empleado WHERE empleado.Iniciales = Vendedor.Iniciales NO-LOCK NO-ERROR.
            END.
        
            CREATE ttFacNormalRemision.
            ASSIGN 
                ttFacNormalRemision.IdRemision  = Remision.id-Remision
                ttFacNormalRemision.Pedido      = l-Pedidos
                ttFacNormalRemision.FecReg      = Remision.FecReg
                ttFacNormalRemision.IdCliente   = Remision.id-Cliente
                ttFacNormalRemision.RazonSocial = Remision.RazonSocial
                ttFacNormalRemision.IdVendedor  = Remision.Id-Vendedor
                ttFacNormalRemision.Vendedor    = IF AVAILABLE Empleado THEN empleado.Nombre ELSE ""
                ttFacNormalRemision.IdEntrega   = Remision.Id-Entrega
                ttFacNormalRemision.Descr       = Entrega.Descr 
                WHEN AVAILABLE Entrega
                ttFacNormalRemision.Tot         = Remision.Tot
                ttFacNormalRemision.FolEmb      = l-FolEmb
                ttFacNormalRemision.FecEmb      = l-FecEmb
                ttFacNormalRemision.HorEmb      = l-HorEmb
                ttFacNormalRemision.InformacionDelPago = l-PagInfo.
            /* Asignaciones condicionales */
            IF l-Camion <> 0 THEN
                ttFacNormalRemision.Camion = l-Camion.

            IF l-Estatus <> 0 THEN
                ttFacNormalRemision.Estatus = STRING(l-Estatus).
                
        END. /* si lo pide normal */
    END.
END PROCEDURE.