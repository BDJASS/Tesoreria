@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*------------------------------------------------------------------------
    File        : teserp003.p
    Purpose     : HU02   /CancelaFact
                  Cancelación de Ventas [Factura]
                  COMO esponsable de tesoreria QUIERO poder capturar una factura 
                  y visualizar  la información de -[datos del cliente, 
                  articulos vendidos y totales] 
                  PARA poder cancelarla si asi es requerido o a solicitud.
    Author(s)   : sis10
    Created     : Fecha actual   
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW. /* Manejo de errores global */

DEFINE TEMP-TABLE ttFactura NO-UNDO           
    FIELD IdFactura   AS CHARACTER FORMAT "x(15)" /* Número de factura */
    FIELD IdCliente   AS INTEGER                  /* Número de cliente */
    FIELD FecReg      AS DATE                     /* Fecha de registro */
    FIELD RazonSocial AS CHARACTER FORMAT "x(40)" /* Razón social del cliente */
    FIELD CalleNo     AS CHARACTER FORMAT "x(50)" /* Dirección */
    FIELD Colonia     AS CHARACTER FORMAT "x(30)" /* Colonia */
    FIELD TipoPrecio  LIKE Factura.TipoPrecio
    FIELD Ciudad      AS CHARACTER FORMAT "x(30)" /* Ciudad */
    FIELD Estado      AS CHARACTER FORMAT "x(30)" /* Estado */
    FIELD CP          AS CHARACTER FORMAT "x(10)" /* Código postal */
    FIELD IdVendedor  LIKE Factura.Id-Vendedor
    FIELD IdCiudad    LIKE Factura.Id-Ciudad
    FIELD Subtotal    AS DECIMAL   FORMAT ">>>,>>9.99" /* Subtotal */
    FIELD Descuento   AS DECIMAL   FORMAT ">>>,>>9.99" /* Descuento */
    FIELD ImpFlete    AS DECIMAL   FORMAT ">>>,>>9.99" /* Importe Flete */
    FIELD ImpSeguro   AS DECIMAL   FORMAT ">>>,>>9.99" /* Importe Seguro */
    FIELD IVA         AS DECIMAL   FORMAT ">>>,>>9.99" /* IVA */
    FIELD Total       AS DECIMAL   FORMAT ">>>,>>9.99" /* Total */ 
    FIELD Requisicion LIKE Factura.requisicion
    FIELD IdTransp    LIKE Factura.id-trasporte
    FIELD Transporte  AS CHAR.    

DEFINE TEMP-TABLE ttDetFactura NO-UNDO
    FIELD IdFactura      AS CHARACTER FORMAT "x(15)" /* Número de factura */
    FIELD IdArticulo     AS CHARACTER FORMAT "x(10)" /* Código de artículo */
    FIELD Descripcion    LIKE DetFactura.Descr /* Descripción del artículo */
    FIELD Presentacion   LIKE DetFactura.Descr /* Presentación */
    FIELD Cantidad       AS DECIMAL   FORMAT ">>>,>>9" /* Cantidad vendida */
    FIELD PrecioUnitario AS DECIMAL   FORMAT ">>>,>>9.99" /* Precio unitario */
    FIELD Descuento      LIKE detfactura.descto
    FIELD Importe        AS DECIMAL   FORMAT ">>>,>>9.99". /* Importe total */

DEFINE DATASET dsFactura FOR ttFactura, ttDetFactura
    DATA-RELATION RelFacturaDetalle FOR ttFactura, ttDetFactura 
    RELATION-FIELDS (IdFactura, IdFactura).

DEFINE TEMP-TABLE tt-Factura LIKE Factura.
DEFINE TEMP-TABLE tt-DetFactura LIKE DetFactura.
DEFINE TEMP-TABLE tt-DetSerie LIKE DetSerie.

DEFINE VARIABLE l-IdFactura    AS CHARACTER NO-UNDO FORMAT "x(15)". /* Número de factura */
DEFINE VARIABLE l-Nivel        AS INTEGER   NO-UNDO. /* Nivel del usuario */
DEFINE VARIABLE l-NMov         AS INTEGER   NO-UNDO.
DEFINE VARIABLE l-recid        AS RECID     NO-UNDO.
DEFINE VARIABLE l-tipo         AS INTEGER   NO-UNDO.
DEFINE VARIABLE l-UsoCfdi      LIKE Factura.Id-UsoCFDI NO-UNDO.
DEFINE VARIABLE l-UsoCfdiDes   AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-Req          LIKE Factura.Requisicion NO-UNDO.
DEFINE VARIABLE l-RFiscal      LIKE Rfiscal.Id-RFiscal NO-UNDO.
DEFINE VARIABLE l-Cliente      LIKE Factura.Id-Cliente NO-UNDO.
DEFINE VARIABLE l-DescrRFiscal AS CHARACTER NO-UNDO FORMAT "X(58)".
DEFINE VARIABLE l-Aceptar      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE l-Pres         LIKE ArtPres.Descr NO-UNDO FORMAT 'X(8)'.

DEFINE VARIABLE l-Timbrada     AS LOGICAL   NO-UNDO.
DEFINE BUFFER g-Folio     FOR Folio.
DEFINE BUFFER b-movcaja   FOR MovCaja.
DEFINE BUFFER b-Pedido    FOR Pedido.
DEFINE BUFFER b-EstPedido FOR EstPedido.
DEFINE BUFFER b-Movim     FOR Movim.


DEF    VAR      l-fecvence   AS DATE.
DEF    VAR      l-ubic       AS CHAR .
DEF    VAR      l-recmov     AS RECID.

DEF    VAR      l-rec        AS RECID     NO-UNDO.
DEF    VAR      l-usuario    LIKE Password.Usuario.
DEF    VAR      l-acuse      LIKE Acuse.id-Acuse.
DEF    VAR      cp-question  AS CHAR.
DEF    VAR      cp-answer    AS LOGICAL.

DEF    VAR      l-NFactura   LIKE Factura.Id-Factura NO-UNDO.
DEF    VAR      l-NUUID      LIKE Factura.UUID NO-UNDO.
DEF    VAR      l-Anter      AS LOGICAL   NO-UNDO.
DEF    VAR      l-AnoAnt     AS INTEGER   NO-UNDO.
DEF    VAR      l-MesAnt     AS INTEGER   NO-UNDO.

DEFINE VARIABLE l-Asunto     AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-Contenido  AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-MailDe     AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-ResponderA AS CHARACTER NO-UNDO.  
DEFINE VARIABLE l-Mail       AS CHARACTER NO-UNDO.  
DEFINE VARIABLE v-Enviado    AS LOGICAL   NO-UNDO.


DEFINE VARIABLE g-Origen     AS CHARACTER NO-UNDO.

DEF    VAR      l-genmov     AS LOGICAL.
DEF    VAR      l-solorem    AS LOGICAL.
DEF    VAR      l-cant       LIKE DetRemis.Cant.
DEF    VAR      l-mensaje    AS CHAR      NO-UNDO.
DEF    VAR      l-Men        AS CHAR      NO-UNDO.
DEF    VAR      l-parcial    AS LOG       NO-UNDO.
DEF    VAR      l-recdev     AS RECID     NO-UNDO.
DEF    VAR      l-i          AS INTEGER   NO-UNDO.
DEF    VAR      l-teclas     AS CHAR      INITIAL "GO,ENTER,RETURN" NO-UNDO.
DEF    VAR      l-cve        LIKE Password.Password.
DEF    VAR      l-Flete      AS DECI      FORMAT "ZZZ,ZZZ,ZZ9.99" NO-UNDO.
DEF    VAR      l-Seguro     LIKE l-Flete NO-UNDO.
DEF    VAR      l-titulo     AS CHAR.
DEF    VAR      l-saldo      LIKE MovCliente.Saldo.
DEF    VAR      l-resp       AS LOGICAL   FORMAT 'Si/No'.
DEF    VAR      l-factura    LIKE Factura.Id-Factura.
/* **********************  Internal Procedures  *********************** */


/* ***************************  Main Procedure *************************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetRefactura:
    DEFINE INPUT  PARAMETER pIdFactura AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER pTipo      AS INT. 
    DEFINE INPUT  PARAMETER pIdUser    AS CHARACTER NO-UNDO. /* Usuario a validar */
    DEFINE INPUT  PARAMETER pConfirmar AS LOGICAL NO-UNDO INITIAL FALSE. /* Confirmación del usuario */
    DEFINE OUTPUT PARAMETER Respuesta  AS CHAR. 
    DEFINE OUTPUT PARAMETER IdError    AS LOGICAL.
    DEFINE OUTPUT PARAMETER DATASET FOR dsFactura.
    
    DEF VAR l-SuperUser     AS CHAR NO-UNDO INITIAL 'GEE,FRANC,ALEX,VIVE,ELF'.
    
    DEF VAR l-Permisos      AS CHAR NO-UNDO INITIAL 'MLLR,DLGR'. /* USUARIOS PEDIDOS */
    
    DEF VAR l-permisosLista AS CHAR NO-UNDO INITIAL "OLDS,MLLR,DLGR,ELF". /* USUARIOS QUE PUEDEN CANCELAR FACT EN DIFERENTE
                                                                          DIA */
    
    
    IF pConfirmar = ? THEN pConfirmar = FALSE.
    /* Validación del Usuario */
    FIND Usuario WHERE Usuario.Id-User = pIdUser NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Usuario THEN 
    DO:
        ASSIGN 
            Respuesta = "El usuario especificado no existe."
            IdError = TRUE.
        RETURN.
    END.
    
    IF NOT CAN-DO("1,2,3", STRING(pTipo)) THEN 
    DO: ASSIGN
        Respuesta = "Tipo de Factura No Valida"
        IdError = TRUE.
        RETURN.
    END. 

    IF AVAILABLE Usuario AND Usuario.id-ubicacion <> "" THEN 
    DO:
        ASSIGN  
            g-Origen = Usuario.id-ubicacion.
    END.
    ELSE 
    DO:
        ASSIGN 
            Respuesta = "El usuario especificado " + pIdUser + " no cuenta con Id-Ubicacion"
            IdError = TRUE.
        RETURN.
    END.
    /* EL PROGRAMA SOLICITA G-origen */
    
    
    FIND Factura WHERE Factura.id-factura = pIdFactura
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Factura THEN 
    DO: ASSIGN
        Respuesta =  "El Folio de la factura no esta registrada"
        IdError = TRUE.
        RETURN.
    END.
    IF Factura.FecCanc <> ? THEN 
    DO: 
        FIND FIRST Empleado WHERE empleado.Iniciales = Factura.UsuarioCanc NO-LOCK NO-ERROR.
        ASSIGN
        Respuesta = "La factura ya fue cancelada el dia " + STRING(Factura.FecCancel) +
                   " por " + (IF AVAILABLE Empleado THEN Empleado.Nombre ELSE Factura.UsuarioCanc)
        IdError = TRUE.
        RETURN.  
    END.
    IF {salt0005.i} = 'MATRIZ' AND Factura.Id-factura BEGINS '4' AND
        Factura.FecReg < 10/31/2016 THEN 
    DO: ASSIGN
        Respuesta = 'La factura es de Saltillo. Solo se puede cancelar en la ' +
            'Sucursal.'
        IdError = TRUE.
        RETURN.
    END.
    IF {salt0005.i} = 'SALTILLO' AND NOT Factura.Id-factura BEGINS '4' THEN 
    DO: ASSIGN
        Respuesta = 'La factura es de Matriz. Solo se puede cancelar en la ' +
            'Matriz.'
        IdError = TRUE.
        RETURN.
    END.
      
    IF YEAR(TODAY) - YEAR(Factura.FecReg) > 1 THEN 
    DO: ASSIGN
        Respuesta = "La factura tiene mas de un a�o, no puede realizarse la cancelacion"
        IdError = TRUE.
        RETURN.
    END.
    IF YEAR(TODAY) <> YEAR(Factura.FecReg) AND MONTH(TODAY) > 3 THEN 
    DO: ASSIGN
        Respuesta = "No puede cancelar facturas de un a�o anterior despues de Marzo."
        IdError = TRUE.
        RETURN.
    END.
      
    IF INDEX(l-SuperUser, pIdUser) = 0 AND {salt0005.i} = 'MATRIZ' THEN 
    DO:
        IF (g-Origen = "03A" AND NOT Factura.Id-Factura BEGINS '0') OR
            (g-Origen = "12" AND NOT Factura.id-Factura BEGINS '5') OR
            (g-Origen = "11" AND NOT Factura.id-Factura BEGINS '4') OR
            (g-Origen = "6" AND NOT Factura.id-Factura BEGINS '6') OR
            (g-Origen = "7" AND NOT Factura.id-Factura BEGINS '7') OR
            (g-Origen = "8" AND NOT Factura.id-Factura BEGINS '8') OR 
            (g-Origen = "9" AND NOT Factura.id-Factura BEGINS '9') OR 
            (g-Origen = "10" AND NOT Factura.id-Factura BEGINS 'N') THEN 
        DO: ASSIGN
            Respuesta = 'No se permite cancelar factura de diferentes tiendas.'
            IdError = TRUE.
            RETURN.
        END.   
    END.
     
    l-Resp = FALSE.
    IF Factura.FecReg <> TODAY THEN   
    DO:
        IF LOOKUP(pIdUser, l-SuperUser) = 0 AND NOT CAN-DO(l-PermisosLista,pIdUser) THEN 
        DO: ASSIGN
            Respuesta = 'No se permite cancelar factura de diferente dia.'
            IdError = TRUE.
            RETURN.
        END.
        ELSE 
        DO:
            IF MONTH(TODAY) <> MONTH(Factura.FecReg) OR YEAR(TODAY) <> YEAR(Factura.FecReg) THEN 
            DO:
                /*
                ASSIGN l-resp = TRUE.
                MESSAGE 'DESEA QUE SE GENERE UN ACUSE?' UPDATE l-resp
                        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO.
                IF NOT l-resp AND DAY(TODAY) > 7 AND USERID("dictdb") <> "franc" THEN DO:
                   MESSAGE 'No se permite cancelar factura de MES CERRADO sin acuse.'.
                   PAUSE 2 NO-MESSAGE.
                   RETURN.
                END.
                */
                IF NOT pConfirmar THEN 
                DO:
                    ASSIGN 
                        Respuesta = "Se realizara una cancelacion de una factura de un mes ya cerrado..."
                        IdError = FALSE.
                    RETURN.
                END.     
            END.
        END.
    END.
     
    IF INDEX(l-Permisos, pIdUser) > 0  THEN 
    DO:
        IF Factura.Pedidos = "" THEN 
        DO: ASSIGN
            Respuesta =  "La factura no pertenece a ningun pedido, cancelacion no procede."
            IdError = TRUE.
            RETURN.
        END.
        FIND FIRST EstPedido WHERE EstPedido.Id-Factura = Factura.Id-Factura
            AND EstPedido.Estatus >= 6 NO-LOCK NO-ERROR.
        IF AVAILABLE EstPedido THEN 
        DO: ASSIGN
            Respuesta = "La factura de credito ya fue embarcada, cancelacion no procede."
            IdError = TRUE.
            RETURN.
        END.
        FIND FIRST Pedido WHERE Pedido.Id-Pedido = ENTRY(1,Factura.Pedidos)
            AND NOT CAN-DO("02B,FUG",Pedido.Id-Alm) NO-LOCK NO-ERROR.
        /*
                 FIND FIRST Pedido WHERE Pedido.Id-Factura = Factura.Id-Factura
                                     AND NOT CAN-DO("02B,FUG",Pedido.Id-Alm) NO-LOCK NO-ERROR.
        */
        IF AVAILABLE Pedido THEN 
        DO: ASSIGN
            Respuesta =
                "La venta no pertenece a su almacen, cancelacion no precede."
                IdError = TRUE.
            RETURN.
        END.
    END.

    FIND FIRST Devolucion WHERE Devolucion.Id-Factura = Factura.id-Factura
        AND Devolucion.TipoVenta = 3 
        AND Devolucion.FecCanc = ?
        NO-LOCK NO-ERROR.
    IF AVAILABLE Devolucion AND NOT Devolucion.VtaCanc
        AND Devolucion.FecCanc = ? THEN 
    DO:
        ASSIGN
            Respuesta = "La Factura tiene articulos registrados por devolucion" +
          " por lo tanto no se permite cancelar facturas parciales. En caso de "
          + " querer realizar la cancelacion hagalo con una devolucion."
          IdError = TRUE.    
        RETURN.   
    END.
    /* pendiente validar que la factura no tenga ningun abono */
    FIND Cliente WHERE Cliente.id-cliente = Factura.id-cliente NO-LOCK NO-ERROR.
    FIND Transporte WHERE Transporte.Id-Tran = Factura.Id-Tran NO-LOCK NO-ERROR.
    FIND Ciudad WHERE Ciudad.id-ciudad = Factura.id-ciudad NO-LOCK NO-ERROR.
    IF AVAILABLE Ciudad THEN
        FIND Estado OF Ciudad NO-LOCK NO-ERROR.
    ASSIGN 
        l-recid = RECID(Factura)
        l-tipo  = 3.

    CREATE ttFactura.
    ASSIGN 
        ttFactura.IdFactura   = Factura.Id-Factura
        ttFactura.FecReg      = Factura.FecReg
        ttFactura.IdCliente   = Factura.id-cliente
        ttFactura.RazonSocial = Factura.RazonSocial
        ttFactura.CalleNo     = Factura.CalleNo
        ttFactura.Colonia     = Factura.Colonia
        ttFactura.TipoPrecio  = Factura.TipoPrecio  /* nueva columna */ 
        ttFactura.CP          = Cliente.CP
        ttFactura.IdVendedor  = Factura.Id-vendedor  /* nueva columna */ 
        ttFactura.IdCiudad    = Factura.Id-ciudad   /* nueva */ 
        ttFactura.Ciudad      = IF AVAILABLE Ciudad THEN Ciudad.Nombre ELSE " "
        ttFactura.Estado      = IF AVAILABLE Estado THEN Estado.Nomcto ELSE " "
        ttFactura.Subtotal    = Factura.Subtotal
        ttFactura.Descuento   = Factura.Descuento   
        ttFactura.ImpFlete    = Factura.ImpFlete
        ttFactura.ImpSeguro   = Factura.ImpSeguro
        ttFactura.IVA         = Factura.Iva      
        ttFactura.Total       = Factura.Tot
        ttFactura.Requisicion = Factura.requisicion    
        ttFactura.IdTran    = Factura.Id-Tran
        ttFactura.Transporte  = IF AVAILABLE Transporte THEN Transporte.Nombre ELSE "".
         
    
    /* Obtener el detalle de los artículos vendidos */  
    FOR EACH DetFactura OF Factura NO-LOCK:
        
        FIND ArtPres WHERE ArtPres.Id-Articulo = DetFactura.Id-Articulo
            AND ArtPres.Id-Pres = DetFactura.Id-Pres NO-LOCK NO-ERROR.
        ASSIGN 
            l-Pres = IF AVAILABLE ArtPres THEN ArtPres.Descr ELSE ".".     
        
        
        CREATE ttDetFactura.
        ASSIGN 
            ttDetFactura.IdFactura      = Factura.Id-Factura
            ttDetFactura.IdArticulo     = DetFactura.Id-Articulo
            ttDetFactura.Descripcion    = DetFactura.Descr
            ttDetFactura.Presentacion   = l-Pres
            ttDetFactura.Cantidad       = detfactura.Cant
            ttDetFactura.PrecioUnitario = DetFactura.PrecUnit
            ttDetFactura.Descuento      = detfactura.descto
            ttDetFactura.Importe        = DetFactura.Importe.   
    END.  
          
         
END PROCEDURE.   


