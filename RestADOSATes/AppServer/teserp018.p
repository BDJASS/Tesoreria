@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*------------------------------------------------------------------------
    File        : teserp018.p
    Purpose     : REPORTE DE FACTURAS DE CONTADO FALTANTES DE PAGO NORMALES.

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
DEF    VAR      l-reporte AS CHAR      NO-UNDO.

DEF    VAR      l-veces   AS INT       NO-UNDO INITIAL 0.
DEF    VAR      l-linea   AS CHAR.
DEF    VAR      l-Camion  AS INTEGER   FORMAT "zz9" NO-UNDO.
DEF    VAR      l-FolEmb  LIKE Embarque.Id-Embarque NO-UNDO.
DEF    VAR      l-FecEmb  AS DATE      FORMAT '99/99/9999' NO-UNDO.
DEF    VAR      l-HorEmb  AS CHAR      FORMAT "x(5)" NO-UNDO.
DEF    VAR      l-CantEmb AS INTEGER   NO-UNDO.
DEF    VAR      l-Estatus LIKE DetEmbarque.Estatus NO-UNDO.  
DEF    VAR      l-SVeces  AS CHAR      NO-UNDO FORMAT "x(2)".
DEF    VAR      l-Aster   AS CHAR      NO-UNDO FORMAT "x(1)".
DEF    VAR      l-Prim    AS LOGICAL   NO-UNDO.
DEF    VAR      l-NOExis  AS LOGICAL   NO-UNDO.
DEF    VAR      l-ListDev AS CHAR      NO-UNDO.
DEF    VAR      l-TotDev  AS DECIMAL   NO-UNDO.
DEF    VAR      l-opcion  AS CHAR      EXTENT 2 FORMAT "x(10)" 
    INITIAL ["Normal", "Condensado"] NO-UNDO.

DEFINE VARIABLE l-Pedidos AS CHARACTER NO-UNDO.

DEF TEMP-TABLE ttFacNormal
    FIELD Tipo        AS CHAR
    FIELD IdRemision  LIKE Remision.Id-Remision
    FIELD Pedido      LIKE l-Pedidos
    FIELD FecReg      LIKE Remision.FecReg
    FIELD IdCliente   LIKE Remision.Id-Cliente
    FIELD RazonSocial LIKE Remision.RazonSocial FORMAT 'x(27)'
    FIELD IdVendedor  LIKE Remision.Id-Vendedor
    FIELD Vendedor    LIKE empleado.Nombre
    FIELD IdEntrega   LIKE Remision.Id-Entrega
    FIELD Descr       LIKE Entrega.Descr
    FIELD Tot         LIKE Remision.Tot
    FIELD FolEmb      LIKE l-FolEmb
    FIELD Camion      LIKE l-Camion
    FIELD FecEmb      LIKE l-FecEmb
    FIELD HorEmb      LIKE l-HorEmb
    FIELD Estatus     AS CHAR
    FIELD DevTot      AS LOGICAL 
    FIELD Pedidos     AS CHARACTER FORMAT 'x(27)'
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
PROCEDURE GetRepFacNormal:    
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER l-FecIni     AS CHARACTER NO-UNDO.
    DEF INPUT PARAMETER l-FecFin     AS CHARACTER NO-UNDO.
    DEF INPUT PARAMETER l-index      AS INTE.
    DEFINE OUTPUT PARAMETER DATASET FOR dsRep.
    DEFINE OUTPUT PARAMETER TABLE FOR ttFacNormal.
    
    
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
    FOR EACH Factura WHERE Factura.Plazo = 0 AND
        Factura.FecReg >= dFechaIni AND  
        Factura.FecReg <= dFechaFin NO-LOCK
        BREAK BY Factura.FecReg BY Factura.Id-Factura:
    
        PAUSE 0.
        IF Factura.FecCanc <> ? OR Factura.Id-Cliente < 11 OR Factura.Id-Factura BEGINS "@" THEN NEXT.
        IF NOT Factura.Id-Factura BEGINS "5" AND l-index = 3 THEN NEXT.
    
        FIND FIRST HistMovCte WHERE HistMovCte.RefSaldo = Factura.Id-Factura AND HistMovCte.Id-Mc = 1
            NO-LOCK NO-ERROR.
        IF AVAILABLE HistMovCte THEN NEXT.

        FOR EACH MovCliente WHERE MovCliente.Refsaldo = Factura.Id-Factura AND
            MovCliente.Id-MC    > 3 NO-LOCK.
            ACCUMULATE Movcliente.Importe (TOTAL).
        END.
        IF (Factura.tot + (ACCUM TOTAL MovCliente.Importe)) < 10 THEN NEXT.
    
        l-Aster = " ".
        IF (ACCUM TOTAL MovCliente.Importe) <> 0 THEN
            l-Aster = "*".

        FIND Entrega OF Factura NO-LOCK NO-ERROR.
        ASSIGN 
            l-linea = FILL(" ", 82) + FILL("_", 55).

        IF FIRST-OF(Factura.FecReg) THEN
            DISPLAY STRING(Factura.FecReg,'99/99/99') FORMAT "x(8)" LABEL "Fecha" 
                WITH FRAME f-sel SIDE-LABELS OVERLAY
                ROW 10 CENTERED.
    
        l-Camion = 0.
        l-FolEmb = ''.
        l-FecEmb = ?.
        l-HorEmb = "".
        FOR EACH EstPedido WHERE EstPedido.Id-Factura = Factura.Id-Factura
            NO-LOCK BREAK BY EstPedido.FecEmb DESCENDING BY EstPedido.HorEmb DESCENDING:
            IF FIRST-OF(EstPedido.FecEmb) THEN 
            DO:
                l-Camion = EstPedido.Id-Camion.
                l-FolEmb = EstPedido.Id-Embarque.
                l-FecEmb = EstPedido.FecEmb.
                l-HorEmb = IF EstPedido.HorEmb <> 0
                    THEN STRING(EstPedido.HorEmb,"hh:mm") ELSE "".
                LEAVE.
            END.
        END.
        l-Estatus = 0.
        l-CantEmb = 0.
        FOR EACH DetEmbarque WHERE DetEmbarque.Id-Factura = Factura.Id-Factura
            NO-LOCK BREAK BY DetEmbarque.Id-Embarque:
            l-Estatus = DetEmbarque.Estatus.
            IF DetEmbarque.Estatus = 2 THEN
                l-CantEmb = l-CantEmb + 1.
            ELSE IF DetEmbarque.Estatus = 9 AND DetEmbarque.Motivo = 9 THEN
                    l-CantEmb = 9.
        END.

        IF NUM-ENTRIES(Factura.Pedidos) > 1 THEN
            ASSIGN l-Pedidos = SUBSTRING(Factura.Pedidos,1,7) + "*".
        ELSE
            ASSIGN l-Pedidos = Factura.Pedidos.

        IF l-index = 1 THEN 
        DO:
            CREATE ttFacNormal.
            ASSIGN 
                ttFacNormal.Tipo        = ""
                ttFacNormal.IdRemision  = Factura.id-Factura
                ttFacNormal.Pedido      = l-Pedidos
                ttFacNormal.FecReg      = Factura.FecReg    
                ttFacNormal.idCliente   = Factura.id-Cliente  
                ttFacNormal.RazonSocial = Factura.RazonSocial
                ttFacNormal.IdVendedor  = Factura.Id-Vendedor
                ttFacNormal.IdEntrega   = Factura.Id-Entrega
                ttFacNormal.Descr       = Entrega.Descr 
                WHEN AVAILABLE Entrega
                ttFacNormal.Tot         = Factura.Tot
                ttFacNormal.FolEmb      = l-FolEmb
                ttFacNormal.FecEmb      = l-FecEmb
                ttFacNormal.HorEmb      = l-HorEmb
                ttFacNormal.Estatus     = STRING(l-Estatus).   

        END. /* si lo pide normal */
        ELSE 
        DO:
        /*   DISPLAY STREAM s-salida
             Factura.id-Factura
             l-Pedidos
             Factura.FecReg  
             Factura.id-Cliente
             Factura.RazonSocial
             Factura.Id-Vendedor
             Factura.Id-Entrega
             Entrega.Descr WHEN AVAILABLE Entrega
             Factura.Tot
             l-linea
           WITH FRAME f-Detallecond.
           DOWN STREAM s-salida WITH FRAME f-detallecond.  */
        END. /* si es condensado */   

        IF (Factura.Id-Entrega <> 16) AND
            ((l-Camion = 0) AND (l-Estatus = 0))
            AND (Factura.FecReg < TODAY - 5)
            AND (Factura.FecReg >= 06/01/2002)
            AND (NOT Factura.Id-Factura BEGINS "4") THEN 
        DO:

            l-ListDev = "".
            l-TotDev = 0.
            FIND FIRST Devolucion WHERE Devolucion.Id-Factura = Factura.Id-Factura
                AND Devolucion.VtaCanc = TRUE
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE Devolucion THEN 
            DO:
                FOR EACH Devolucion WHERE Devolucion.Id-Factura = Factura.Id-Factura
                    AND Devolucion.FecCanc = ?
                    NO-LOCK:
                    l-ListDev = l-ListDev + ' -' + STRING(Devolucion.Id-Dev,'999999').
                    l-TotDev = l-TotDev + Devolucion.Tot.
                END.
            END.
            ELSE ASSIGN l-TotDev  = Devolucion.Tot
                    l-ListDev = ' -' + STRING(Devolucion.Id-Dev,'999999').
       
            FIND FIRST DetFactura WHERE DetFactura.Id-Factura = Factura.Id-Factura
                AND DetFactura.Tipo > 2 AND DetFactura.Tipo < 5 NO-LOCK NO-ERROR.
            CREATE w-CEmba.
            ASSIGN 
                w-CEmba.Id-Remision = Factura.id-Factura
                w-CEmba.Pedido      = l-pedidos
                w-CEmba.FecReg      = Factura.FecReg
                w-CEmba.Id-Cliente  = Factura.id-Cliente
                w-CEmba.RazonSocial = Factura.RazonSocial
                w-CEmba.Id-Vendedor = Factura.Id-Vendedor
                w-CEmba.Id-Entrega  = Factura.Id-Entrega
                w-CEmba.Descr       = Entrega.Descr
                w-CEmba.Tot         = Factura.Tot
                w-CEmba.Camion      = l-Camion
                w-CEmba.FolEmb      = l-FolEmb
                w-CEmba.FecEmb      = l-FecEmb
                w-CEmba.HorEmb      = l-HorEmb
                w-CEmba.Estatus     = l-Estatus
                w-CEmba.CantEmb     = l-CantEmb
                w-CEmba.Aster       = " "
                w-CEmba.Corte       = IF AVAILABLE DetFactura THEN "C" ELSE ""
                w-CEmba.DevTot      = AVAILABLE Devolucion OR (l-TotDev > 0 AND l-TotDev >= Factura.Tot - 1) 
                w-CEmba.Pedidos     = Factura.Pedidos + l-ListDev.
        END.  
    END.
    STATUS DEFAULT "Generando Facturas Contado x Cancelar Pasa x Mercancia...".
    FIND Entrega WHERE Entrega.Id-Entrega = 16 NO-LOCK NO-ERROR.
    FOR EACH Remision WHERE Remision.Pagada = FALSE 
        AND Remision.TipoVenta = 2 
        AND Remision.FecReg >= dFechaIni
        AND Remision.FecReg <= dFechaFin
        AND Remision.FecReg < TODAY - 3
        AND Remision.Id-Entrega = 16
        NO-LOCK BY Remision.Id-Remision:

        IF Remision.Feccanc <> ? /*OR 
       Remision.Id-Entrega <> 16 OR
       Remision.FecReg >= TODAY - 3*/ THEN NEXT.
        IF remision.tipoventa = 1 AND remision.id-Remis BEGINS "@" THEN NEXT.
        IF remision.tipoventa = 2 AND CAN-DO("0,1,2,3,4,5,6,7,8,9",SUBSTRING(remision.id-remis,7,1)) THEN NEXT.
        IF NOT Remision.Id-Remision MATCHES "*J" AND l-Index = 3 THEN NEXT. 
    
        FIND Cliente WHERE Cliente.Id-Cliente = Remision.Id-Cliente NO-LOCK NO-ERROR.
        IF AVAILABLE Cliente THEN
            FIND Zona WHERE Zona.Id-Zona = Cliente.Id-Zona NO-LOCK NO-ERROR.
        ELSE RELEASE Zona.
        IF AVAILABLE Zona AND Zona.Ubic <> 1 AND Remision.FecReg > TODAY - 7 THEN NEXT.
        PAUSE 0.

        ASSIGN 
            l-linea = FILL(" ", 82) + FILL("_", 58).
        l-Camion = 0.
        l-FolEmb = ''.
        l-FecEmb = ?.
        l-HorEmb = "".
        l-NOExis = TRUE.
        FOR EACH EstPedido WHERE EstPedido.Id-Factura = Remision.Id-Remision
            NO-LOCK BREAK BY EstPedido.FecEmb DESCENDING BY EstPedido.HorEmb DESCENDING:
            IF FIRST-OF(EstPedido.FecEmb) THEN 
            DO:
                l-Camion = EstPedido.Id-Camion.
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

        IF ((l-Camion = 0 OR l-Camion = 182) AND 
            (l-Estatus = 0 OR l-Estatus = 9))
            THEN 
        DO:

            l-ListDev = "".
            l-TotDev = 0.
            FIND FIRST Devolucion WHERE Devolucion.Id-Factura = Remision.Id-Remision
                AND Devolucion.VtaCanc = TRUE
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE Devolucion THEN 
            DO:
                FOR EACH Devolucion WHERE Devolucion.Id-Factura = Remision.Id-Remision
                    AND Devolucion.FecCanc = ?
                    NO-LOCK:
                    l-ListDev = l-ListDev + ' -' + STRING(Devolucion.Id-Dev,'999999').
                    l-TotDev = l-TotDev + Devolucion.Tot.
                END.
            END.
            ELSE ASSIGN l-TotDev  = Devolucion.Tot
                    l-ListDev = ' -' + STRING(Devolucion.Id-Dev,'999999').

            FIND FIRST DetRemis WHERE DetRemis.Id-Remision = Remision.Id-Remision
                AND DetRemis.Tipo > 2 AND DetRemis.Tipo < 5 NO-LOCK NO-ERROR.
            /*       IF NOT AVAILABLE DetRemis THEN DO:*/
            CREATE w-Datos.
            ASSIGN 
                w-Datos.Id-Remision = Remision.id-Remision
                w-Datos.Pedido      = l-pedidos
                w-Datos.FecReg      = Remision.FecReg
                w-Datos.Id-Cliente  = Remision.id-Cliente
                w-Datos.RazonSocial = Remision.RazonSocial
                w-Datos.Id-Vendedor = Remision.Id-Vendedor
                w-Datos.Id-Entrega  = Remision.Id-Entrega
                w-Datos.Descr       = Entrega.Descr
                w-Datos.Tot         = Remision.Tot
                w-Datos.Camion      = l-Camion
                w-Datos.FolEmb      = l-FolEmb
                w-Datos.FecEmb      = l-FecEmb
                w-Datos.HorEmb      = l-HorEmb
                w-Datos.Estatus     = l-Estatus
                w-Datos.CantEmb     = l-CantEmb
                w-Datos.Corte       = IF AVAILABLE DetRemis THEN "C" ELSE ""
                w-Datos.DevTot      = AVAILABLE Devolucion OR (l-TotDev > 0 AND l-TotDev >= Remision.Tot - 1)
                w-Datos.Pedidos     = Remision.Pedidos + l-ListDev.
        /*       END.  */
        END.  
    END.
    STATUS DEFAULT "Generando Facturas Credito x Cancelar Pasa x Mercancia...".
    FOR EACH Factura WHERE /*Factura.Id-Factura >= "1" AND
                       Factura.Id-Factura < "3" AND*/
        Factura.FecReg >= dFechaIni AND  
        Factura.FecReg <= dFechaFin AND
        Factura.FecReg < TODAY - 3 AND
        Factura.Id-Entrega = 16 AND
        Factura.Id-Cliente >= 11
        NO-LOCK:
        PAUSE 0.
        IF Factura.Feccanc <> ? /*OR Factura.Id-Cliente < 5 OR
       Factura.Id-Entrega <> 16 OR
       Factura.FecReg >= TODAY - 3 OR
       Factura.Id-Factura < "1" or Factura.Id-Factura >= "3"*/ THEN NEXT.
        IF Factura.Id-Factura BEGINS "0" OR Factura.Id-Factura BEGINS "4" THEN NEXT.
        IF NOT Factura.Id-Factura BEGINS "5" AND l-Index = 3 THEN NEXT.
     
    
        FIND Cliente WHERE Cliente.Id-Cliente = Factura.Id-Cliente NO-LOCK NO-ERROR.
        IF AVAILABLE Cliente THEN
            FIND Zona WHERE Zona.Id-Zona = Cliente.Id-Zona NO-LOCK NO-ERROR.
        ELSE RELEASE Zona.
        IF AVAILABLE Zona AND Zona.Ubic <> 1 AND Factura.FecReg > TODAY - 7 THEN NEXT.

        FIND FIRST HistMovCte WHERE HistMovCte.RefSaldo = Factura.Id-Factura AND HistMovCte.Id-Mc = 1
            NO-LOCK NO-ERROR.
        IF AVAILABLE HistMovCte THEN NEXT.
    
        FOR EACH MovCliente WHERE MovCliente.Refsaldo = Factura.Id-Factura AND
            MovCliente.Id-MC    > 3 NO-LOCK.
            ACCUMULATE Movcliente.Importe (TOTAL).
        END.
        IF (Factura.tot + (ACCUM TOTAL MovCliente.Importe)) < 10 THEN NEXT.

        l-Aster = " ".
        IF (ACCUM TOTAL MovCliente.Importe) <> 0 THEN l-Aster = "*".

        FIND Entrega OF Factura NO-LOCK NO-ERROR.
        l-Camion = 0.
        l-FolEmb = ''.
        l-FecEmb = ?.
        l-HorEmb = "".
        l-NOExis = TRUE.
        FOR EACH EstPedido WHERE EstPedido.Id-Factura = Factura.Id-Factura
            NO-LOCK BREAK BY EstPedido.FecEmb DESCENDING BY EstPedido.HorEmb DESCENDING:
            IF LAST-OF(EstPedido.FecEmb) THEN 
            DO:
                l-Camion = EstPedido.Id-Camion.
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
        FOR EACH DetEmbarque WHERE DetEmbarque.Id-Factura = Factura.Id-Factura
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

        IF ((l-Camion = 0 OR l-Camion = 182) AND (l-Estatus = 0 OR l-Estatus = 9))
            THEN 
        DO:

            l-ListDev = "".
            l-TotDev = 0.
            FIND FIRST Devolucion WHERE Devolucion.Id-Factura = Factura.Id-Factura
                AND Devolucion.VtaCanc = TRUE
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE Devolucion THEN 
            DO:
                FOR EACH Devolucion WHERE Devolucion.Id-Factura = Factura.Id-Factura
                    AND Devolucion.FecCanc = ?
                    NO-LOCK:
                    l-ListDev = l-ListDev + ' -' + STRING(Devolucion.Id-Dev,'999999').
                    l-TotDev = l-TotDev + Devolucion.Tot.
                END.
            END.
            ELSE ASSIGN l-TotDev  = Devolucion.Tot
                    l-ListDev = ' -' + STRING(Devolucion.Id-Dev,'999999').

            FIND FIRST DetFactura WHERE DetFactura.Id-Factura = Factura.Id-Factura
                AND DetFactura.Tipo > 2 AND DetFactura.Tipo < 5 NO-LOCK NO-ERROR.
            /*       IF NOT AVAILABLE DetFactura THEN DO:*/
            CREATE w-Datos.
            ASSIGN 
                w-Datos.Id-Remision = Factura.id-Factura
                w-Datos.Pedidos     = l-pedidos
                w-Datos.FecReg      = Factura.FecReg
                w-Datos.Id-Cliente  = Factura.id-Cliente
                w-Datos.RazonSocial = Factura.RazonSocial
                w-Datos.Id-Vendedor = Factura.Id-Vendedor
                w-Datos.Id-Entrega  = Factura.Id-Entrega
                w-Datos.Descr       = Entrega.Descr
                w-Datos.Tot         = Factura.Tot
                w-Datos.Camion      = l-Camion
                w-Datos.FolEmb      = l-FolEmb
                w-Datos.FecEmb      = l-FecEmb
                w-Datos.HorEmb      = l-HorEmb
                w-Datos.Estatus     = l-Estatus
                w-Datos.CantEmb     = l-CantEmb
                w-Datos.Corte       = IF AVAILABLE DetFactura THEN "C" ELSE ""
                w-Datos.DevTot      = AVAILABLE Devolucion OR (l-TotDev > 0 AND l-TotDev >= Factura.Tot - 1)
                w-Datos.Pedidos     = Factura.Pedidos + l-ListDev.
        /*       END. */ 
        END.  
    END.


    STATUS DEFAULT "Generando Facturas x Cancelar en Embarques....".
    l-Prim = TRUE.
    FOR EACH DetEmbarque WHERE DetEmbarque.Estatus = 9
        AND DetEmbarque.Motivo = 9
        NO-LOCK BY DetEmbarque.Id-Factura:

        FIND FIRST w-Datos WHERE w-Datos.Id-Remision = DetEmbarque.Id-Factura
            NO-LOCK NO-ERROR.
        IF AVAILABLE w-Datos THEN NEXT.

        FIND FIRST w-CEmba WHERE w-CEmba.Id-Remision = DetEmbarque.Id-Factura
            NO-LOCK NO-ERROR.
        IF AVAILABLE w-CEmba THEN NEXT.

        FIND Factura WHERE Factura.Id-Factura = DetEmbarque.Id-Factura
            NO-LOCK NO-ERROR.
        FIND Remision WHERE Remision.Id-Remision = DetEmbarque.Id-Factura
            NO-LOCK NO-ERROR.

        IF (AVAILABLE Factura AND Factura.FecCanc = ?) THEN 
        DO:
            IF NOT Factura.Id-Factura BEGINS "5" AND l-Index = 3 THEN NEXT. 
       
            FIND FIRST RelFacEnv WHERE RelFacEnv.Id-Factura = Factura.Id-Factura NO-LOCK NO-ERROR.
            IF AVAILABLE RelFacEnv THEN NEXT.

            FOR EACH MovCliente WHERE MovCliente.Refsaldo = Factura.Id-Factura
                AND MovCliente.Id-MC    > 3 NO-LOCK.
                ACCUMULATE Movcliente.Importe (TOTAL).
            END.
            FOR EACH HistMovCte WHERE HistMovCte.Refsaldo = Factura.Id-Factura
                AND HistMovCte.Id-MC    > 3 NO-LOCK.
                ACCUMULATE HistMovCte.Importe (TOTAL).
            END.
            IF (Factura.tot + (ACCUM TOTAL MovCliente.Importe) +
                (ACCUM TOTAL HistMovCte.Importe)) < 10 THEN NEXT.

            l-Aster = " ".
            IF ((ACCUM TOTAL MovCliente.Importe) +
                (ACCUM TOTAL HistMovCte.Importe)) <> 0 THEN
                l-Aster = "*".

            FIND Entrega OF Factura NO-LOCK NO-ERROR.

            l-Camion = 0.
            l-FolEmb = ''.
            l-FecEmb = ?.
            l-HorEmb = "".
            FOR EACH EstPedido WHERE EstPedido.Id-Factura = Factura.Id-Factura
                NO-LOCK BREAK BY EstPedido.FecEmb DESCENDING BY EstPedido.HorEmb DESCENDING:
                IF FIRST-OF(EstPedido.FecEmb) THEN 
                DO:
                    l-Camion = EstPedido.Id-Camion.
                    l-FolEmb = EstPedido.Id-Embarque.
                    l-FecEmb = EstPedido.FecEmb.
                    l-HorEmb = IF EstPedido.HorEmb <> 0
                        THEN STRING(EstPedido.HorEmb,"hh:mm") ELSE "".
                    LEAVE.
                END.
            END.
            l-Estatus = 9.
            l-CantEmb = 9.


            l-ListDev = "".
            l-TotDev = 0.
            FIND FIRST Devolucion WHERE Devolucion.Id-Factura = Factura.Id-Factura
                AND Devolucion.VtaCanc = TRUE
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE Devolucion THEN 
            DO:
                FOR EACH Devolucion WHERE Devolucion.Id-Factura = Factura.Id-Factura
                    AND Devolucion.FecCanc = ?
                    NO-LOCK:
                    l-ListDev = l-ListDev + ' -' + STRING(Devolucion.Id-Dev,'999999').
                    l-TotDev = l-TotDev + Devolucion.Tot.
                END.
            END.
            ELSE ASSIGN l-TotDev  = Devolucion.Tot
                    l-ListDev = ' -' + STRING(Devolucion.Id-Dev,'999999').

            FIND FIRST DetFactura WHERE DetFactura.Id-Factura = Factura.Id-Factura
                AND DetFactura.Tipo > 2 AND DetFactura.Tipo < 5 NO-LOCK NO-ERROR.
            CREATE w-CEmba.
            ASSIGN 
                w-CEmba.Id-Remision = Factura.id-Factura
                w-CEmba.Pedido      = l-pedidos
                w-CEmba.FecReg      = Factura.FecReg
                w-CEmba.Id-Cliente  = Factura.id-Cliente
                w-CEmba.RazonSocial = Factura.RazonSocial
                w-CEmba.Id-Vendedor = Factura.Id-Vendedor
                w-CEmba.Id-Entrega  = Factura.Id-Entrega
                w-CEmba.Descr       = Entrega.Descr
                w-CEmba.Tot         = Factura.Tot
                w-CEmba.Camion      = l-Camion
                w-CEmba.FolEmb      = l-FolEmb
                w-CEmba.FecEmb      = l-FecEmb
                w-CEmba.HorEmb      = l-HorEmb
                w-CEmba.Estatus     = l-Estatus
                w-CEmba.CantEmb     = l-CantEmb
                w-CEmba.Aster       = l-Aster
                w-CEmba.Corte       = IF AVAILABLE DetFactura THEN "C" ELSE ""
                w-CEmba.DevTot      = AVAILABLE Devolucion OR (l-TotDev > 0 AND l-TotDev >= Factura.Tot - 1)
                w-CEmba.Pedidos     = Factura.Pedidos + l-ListDev.

        END.
        ELSE IF (AVAILABLE Remision AND Remision.FecCanc = ? AND Remision.Pagada = FALSE) THEN 
            DO:
                IF NOT Remision.Id-Remision MATCHES "*J" AND l-Index = 3 THEN NEXT. 
                FIND Entrega OF Remision NO-LOCK NO-ERROR.

                l-Camion = 0.
                l-FolEmb = ''.
                l-FecEmb = ?.
                l-HorEmb = "".
                FOR EACH EstPedido WHERE EstPedido.Id-Factura = Remision.Id-Remision
                    NO-LOCK BREAK BY EstPedido.FecEmb BY EstPedido.HorEmb:
                    IF LAST-OF(EstPedido.FecEmb) THEN 
                    DO:
                        l-Camion = EstPedido.Id-Camion.
                        l-FolEmb = EstPedido.Id-Embarque.
                        l-FecEmb = EstPedido.FecEmb.
                        l-HorEmb = IF EstPedido.HorEmb <> 0
                            THEN STRING(EstPedido.HorEmb,"hh:mm") ELSE "".
                    END.
                END.
                l-Estatus = DetEmbarque.Estatus.
                l-CantEmb = 9.

                l-ListDev = "".
                l-TotDev = 0.
                FIND FIRST Devolucion WHERE Devolucion.Id-Factura = Remision.Id-Remision
                    AND Devolucion.VtaCanc = TRUE
                    NO-LOCK NO-ERROR.
                IF NOT AVAILABLE Devolucion THEN 
                DO:
                    FOR EACH Devolucion WHERE Devolucion.Id-Factura = Remision.Id-Remision
                        AND Devolucion.FecCanc = ?
                        NO-LOCK:
                        l-ListDev = l-ListDev + ' -' + STRING(Devolucion.Id-Dev,'999999').
                        l-TotDev = l-TotDev + Devolucion.Tot.
                    END.
                END.
                ELSE ASSIGN l-TotDev  = Devolucion.Tot
                        l-ListDev = ' -' + STRING(Devolucion.Id-Dev,'999999').
                FIND FIRST DetRemis WHERE DetRemis.Id-Remision = Remision.Id-Remision
                    AND DetRemis.Tipo > 2 AND DetRemis.Tipo < 5 NO-LOCK NO-ERROR.
                CREATE w-CEmba.
                ASSIGN 
                    w-CEmba.Id-Remision = Remision.id-Remision
                    w-CEmba.Pedido      = l-pedidos   
                    w-CEmba.FecReg      = Remision.FecReg
                    w-CEmba.Id-Cliente  = Remision.id-Cliente
                    w-CEmba.RazonSocial = Remision.RazonSocial
                    w-CEmba.Id-Vendedor = Remision.Id-Vendedor
                    w-CEmba.Id-Entrega  = Remision.Id-Entrega
                    w-CEmba.Descr       = Entrega.Descr
                    w-CEmba.Tot         = Remision.Tot
                    w-CEmba.Camion      = l-Camion
                    w-CEmba.FolEmb      = l-FolEmb
                    w-CEmba.FecEmb      = l-FecEmb
                    w-CEmba.HorEmb      = l-HorEmb
                    w-CEmba.Estatus     = l-Estatus
                    w-CEmba.CantEmb     = l-CantEmb
                    w-CEmba.Aster       = " "
                    w-CEmba.Corte       = IF AVAILABLE DetRemis THEN "C" ELSE ""
                    w-CEmba.DevTot      = AVAILABLE Devolucion OR (l-TotDev > 0 AND l-TotDev >= Remision.Tot - 1)
                    w-CEmba.Pedidos     = Remision.Pedidos + l-ListDev.
            END.
    END.


    l-Prim = TRUE.
    FOR EACH w-CEmba NO-LOCK:


        IF NUM-ENTRIES(w-CEmba.Pedidos) > 1 THEN
            ASSIGN l-Pedidos = SUBSTRING(w-CEmba.Pedidos,1,7) + "*".
        ELSE
            ASSIGN l-Pedidos = SUBSTRING(w-CEmba.Pedidos,1,7).
    
        FIND FIRST Vendedor WHERE Vendedor.Id-Vendedor = w-CEmba.Id-Vendedor
            NO-LOCK NO-ERROR.
        IF AVAILABLE Vendedor THEN 
        DO:
            FIND FIRST Empleado WHERE empleado.Iniciales = Vendedor.Iniciales NO-LOCK NO-ERROR.
        END.
        IF l-index = 1 THEN 
        DO:
            CREATE ttFacNormal.
            ASSIGN 
                ttFacNormal.Tipo        = "FACS X CANCELAR EN EMBARQUE"
                ttFacNormal.IdRemision  = w-CEmba.id-Remision
                ttFacNormal.Pedido      = l-Pedidos
                ttFacNormal.FecReg      = w-CEmba.FecReg     
                ttFacNormal.idCliente   = w-CEmba.id-Cliente  
                ttFacNormal.RazonSocial = w-CEmba.RazonSocial
                ttFacNormal.IdVendedor  = w-CEmba.Id-Vendedor
                ttFacNormal.Vendedor    = IF AVAILABLE Empleado THEN empleado.Nombre ELSE ""
                ttFacNormal.IdEntrega   = w-CEmba.Id-Entrega
                ttFacNormal.Descr       = w-CEmba.Descr
                ttFacNormal.Tot         = w-CEmba.Tot
                ttFacNormal.FolEmb      = w-CEmba.FolEmb
                ttFacNormal.FecEmb      = w-CEmba.FecEmb
                ttFacNormal.HorEmb      = w-CEmba.HorEmb           
                ttFacNormal.DevTot      = w-CEmba.DevTot
                ttFacNormal.Camion      = w-Cemba.Camion
                ttFacNormal.Pedidos     = w-Cemba.Pedidos.   
            
            IF w-Cemba.Estatus = 0 THEN 
                vEstatus = "".  /* Caso Estatus = 0 → string vacío */
            ELSE 
            DO:
                /* Lógica original para Estatus ≠ 0 */
                vEstatus = STRING(w-Cemba.Estatus).
                IF (w-Cemba.Estatus = 2 AND w-Cemba.CantEmb > 1) OR 
                    (w-Cemba.Estatus = 9 AND w-Cemba.CantEmb = 9) THEN
                    vEstatus = vEstatus + "-" + STRING(w-Cemba.CantEmb, "9").
            END.  
            ttFacNormal.Estatus = vEstatus.  /* Asignación final */
        END. /* si lo pide normal */  
    
    END. 


    l-Prim = TRUE.
    FOR EACH w-Datos NO-LOCK:



        IF NUM-ENTRIES(w-Datos.Pedidos) > 1 THEN
            ASSIGN l-Pedidos = SUBSTRING(w-Datos.Pedidos,1,7) + "*".
        ELSE
            ASSIGN l-Pedidos = SUBSTRING(w-Datos.Pedidos,1,7).
        FIND FIRST Vendedor WHERE Vendedor.Id-Vendedor = w-Datos.Id-Vendedor
            NO-LOCK NO-ERROR.
        IF AVAILABLE Vendedor THEN 
        DO:
            FIND FIRST Empleado WHERE empleado.Iniciales = Vendedor.Iniciales NO-LOCK NO-ERROR.
        END.
        IF l-index = 1 THEN 
        DO:
            CREATE ttFacNormal.
            ASSIGN 
                ttFacNormal.Tipo        = "FACTS X CANC (PASA X MCIA)"
                ttFacNormal.IdRemision  = w-Datos.id-Remision
                ttFacNormal.Pedido      = l-Pedidos
                ttFacNormal.FecReg      = w-Datos.FecReg     
                ttFacNormal.idCliente   = w-Datos.id-Cliente  
                ttFacNormal.RazonSocial = w-Datos.RazonSocial
                ttFacNormal.IdVendedor  = w-Datos.Id-Vendedor
                ttFacNormal.Vendedor    = IF AVAILABLE Empleado THEN empleado.Nombre ELSE ""
                ttFacNormal.IdEntrega   = w-Datos.Id-Entrega
                ttFacNormal.Descr       = w-Datos.Descr
                ttFacNormal.Tot         = w-Datos.Tot
                ttFacNormal.FolEmb      = w-Datos.FolEmb
                ttFacNormal.FecEmb      = w-Datos.FecEmb
                ttFacNormal.HorEmb      = IF w-Datos.Corte = "C" THEN "CORTE" ELSE ""
                ttFacNormal.DevTot      = w-Datos.DevTot
                ttFacNormal.Camion      = w-Datos.Camion
                ttFacNormal.Pedidos     = w-Datos.Pedidos. 
        
            IF w-Datos.Estatus = 0 THEN 
                vEstatus = "".  /* Caso Estatus = 0 → string vacío */
            ELSE 
            DO:
                /* Lógica original para Estatus ≠ 0 */
                vEstatus = STRING(w-Datos.Estatus).
                IF (w-Datos.Estatus = 2 AND w-Datos.CantEmb > 1) OR 
                    (w-Datos.Estatus = 9 AND w-Datos.CantEmb = 9) THEN
                    vEstatus = vEstatus + "-" + STRING(w-Datos.CantEmb, "9").
            END.  
            ttFacNormal.Estatus = vEstatus.  /* Asignación final */
             
        /*
                 w-Datos.Camion      WHEN w-Datos.Camion <> 0 @ l-Camion
                 w-Datos.FecEmb      @ l-FecEmb
                 w-Datos.HorEmb      @ l-HorEmb
        */

        END. /* si lo pide normal */
    END.

END PROCEDURE.