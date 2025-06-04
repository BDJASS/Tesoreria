@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*------------------------------------------------------------------------
    File        : teserp003.p
    Purpose     : HU02   /CancelaFact
                  tesa0580.p 
                  Cancelación de Ventas [Factura]
                  COMO esponsable de tesoreria QUIERO poder capturar una factura 
                  y visualizar  la información de -[datos del cliente, 
                  articulos vendidos y totales] 
                  PARA poder cancelarla si asi es requerido o a solicitud.
    Author(s)   : sis10
    Created     : Fecha actual   
  ----------------------------------------------------------------------*/

/*   Ticket TASK 806 - User Story 269 HU02 Cancelacion Ventas
  1. Quitar los campos[ requisicion y transporte] cambiar 
     por el campo que indique que la mercancia fue entregada y Devolucion
     JASS29052025
 */
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
    FIELD Transporte  AS CHAR
    FIELD Embarcado   AS LOGICAL
    FIELD Devolucion  AS LOGICAL.    

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
    
    DEF VAR l-SuperUser     AS CHAR    NO-UNDO INITIAL 'GEE,FRANC,ALEX,VIVE,ELF,NJCC'.
    
    DEF VAR l-Permisos      AS CHAR    NO-UNDO INITIAL 'MLLR,DLGR'. /* USUARIOS PEDIDOS */
    
    DEF VAR l-permisosLista AS CHAR    NO-UNDO INITIAL "OLDS,MLLR,DLGR". /* USUARIOS QUE PUEDEN CANCELAR FACT EN DIFERENTE
                                                                          DIA */
    
    DEF VAR l-Embarcado     AS LOGICAL NO-UNDO INITIAL FALSE.
    DEF VAR l-Devolucion    AS LOGICAL NO-UNDO INITIAL FALSE.
    
    IF pConfirmar = ? THEN pConfirmar = FALSE.
    /* Validación del Usuario */
    FIND Usuario WHERE Usuario.Id-User = pIdUser NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Usuario THEN 
    DO:
        ASSIGN 
            Respuesta = "El usuario especificado no existe."
            IdError   = TRUE.
        RETURN.
    END.
    
    IF NOT CAN-DO("1,2,3", STRING(pTipo)) THEN 
    DO: 
        ASSIGN
            Respuesta = "Tipo de Factura No Valida"
            IdError   = TRUE.
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
            IdError   = TRUE.
        RETURN.
    END.
    /* EL PROGRAMA SOLICITA G-origen */
    
    /* BUSCA EN TIPO CREDITO */ 
    IF pTipo = 3 THEN 
    DO:
        FIND Factura WHERE Factura.id-factura = pIdFactura
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Factura THEN 
        DO: 
            ASSIGN
                Respuesta = "El Folio de la factura no esta registrada"
                IdError   = TRUE.
            RETURN.
        END.
    
        IF Factura.FecCanc <> ? THEN 
        DO: 
            DEFINE VARIABLE cCancela       AS CHARACTER NO-UNDO.
            DEFINE VARIABLE cAutoriza      AS CHARACTER NO-UNDO.
            DEFINE VARIABLE cMensajeMotivo AS CHARACTER NO-UNDO.
    
            /* Obtener nombre de quien cancela */
            IF Factura.UsuarioCanc <> "" THEN 
            DO:
                FIND FIRST Empleado WHERE empleado.Iniciales = Factura.UsuarioCanc NO-LOCK NO-ERROR.
                cCancela = IF AVAILABLE Empleado THEN Empleado.Nombre ELSE Factura.UsuarioCanc.
            END.
            ELSE 
            DO:
                cCancela = "Desconocido".
            END.
    
            /* Obtener nombre de quien autoriza */
            IF Factura.UsuarioSol <> "" THEN 
            DO:
                FIND FIRST Empleado WHERE empleado.Iniciales = Factura.UsuarioSol NO-LOCK NO-ERROR.
                cAutoriza = IF AVAILABLE Empleado THEN Empleado.Nombre ELSE Factura.UsuarioSol.
            END.
            ELSE 
            DO:
                cAutoriza = "No especificado".
            END.
    
            /* Manejar el motivo solo si existe */
            IF Factura.Motivo <> "" AND Factura.Motivo <> ? THEN
                cMensajeMotivo = " Motivo: " + STRING(Factura.Motivo) + ".".
            ELSE
                cMensajeMotivo = "".
    
            /* Construir mensaje final */
            ASSIGN
                Respuesta = "La factura ya fue cancelada el dia " + STRING(Factura.FecCancel) +
                   " por " + cCancela + "." +
                   (IF Factura.UsuarioSol <> "" 
                    THEN "Solicitado por: " + cAutoriza + "." 
                    ELSE "") + 
                   cMensajeMotivo  
                IdError   = TRUE.
            RETURN.  
        END.             
        IF {salt0005.i} = 'MATRIZ' AND Factura.Id-factura BEGINS '4' AND
            Factura.FecReg < 10/31/2016 THEN 
        DO: 
            ASSIGN
                Respuesta = 'La factura es de Saltillo. Solo se puede cancelar en la ' +
            'Sucursal.'
                IdError   = TRUE.
            RETURN.
        END.
        IF {salt0005.i} = 'SALTILLO' AND NOT Factura.Id-factura BEGINS '4' THEN 
        DO: 
            ASSIGN
                Respuesta = 'La factura es de Matriz. Solo se puede cancelar en la ' +
            'Matriz.'
                IdError   = TRUE.
            RETURN.
        END.
      
        IF YEAR(TODAY) - YEAR(Factura.FecReg) > 1 THEN 
        DO: 
            ASSIGN
                Respuesta = "La factura tiene mas de un a�o, no puede realizarse la cancelacion"
                IdError   = TRUE.
            RETURN.
        END.
        IF YEAR(TODAY) <> YEAR(Factura.FecReg) AND MONTH(TODAY) > 3 THEN 
        DO: 
            ASSIGN
                Respuesta = "No puede cancelar facturas de un a�o anterior despues de Marzo."
                IdError   = TRUE.
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
            DO: 
                ASSIGN
                    Respuesta = 'No se permite cancelar factura de diferentes tiendas.'
                    IdError   = TRUE.
                RETURN.
            END.   
        END.
     
        l-Resp = FALSE.
        IF Factura.FecReg <> TODAY THEN   
        DO:
            IF LOOKUP(pIdUser, l-SuperUser) = 0 AND NOT CAN-DO(l-PermisosLista,pIdUser) THEN 
            DO: 
                ASSIGN
                    Respuesta = 'No se permite cancelar factura de diferente dia.'
                    IdError   = TRUE.
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
                            IdError   = FALSE.
                        RETURN.
                    END.     
                END.
            END.
        END.
    
        /* Se pidio esta validacion que tiene Ventas Solo que ellos no pueden cancelar
           Tesoria si las cancela, pero no saben si estaba embarcada, se manda el aviso al Front
           para que coloque mensaje y decidan si cancelan o no  */
        IF INDEX(l-SuperUser, pIdUser) > 0  THEN 
        DO:                                       

            FIND FIRST EstPedido WHERE EstPedido.Id-Factura = Factura.Id-Factura
                AND EstPedido.Estatus >= 6 NO-LOCK NO-ERROR.
            IF AVAILABLE EstPedido THEN 
            DO: 
                ASSIGN
                    l-Embarcado = TRUE.
            //    Respuesta = "La factura de credito ya fue embarcada, cancelacion no procede."
            //    IdError   = TRUE.
          //   RETURN.
            END.
        END.      
    
    
        IF INDEX(l-Permisos, pIdUser) > 0  THEN 
        DO:
            IF Factura.Pedidos = "" THEN 
            DO: 
                ASSIGN
                    Respuesta = "La factura no pertenece a ningun pedido, cancelacion no procede."
                    IdError   = TRUE.
                RETURN.
            END.
            FIND FIRST EstPedido WHERE EstPedido.Id-Factura = Factura.Id-Factura
                AND EstPedido.Estatus >= 6 NO-LOCK NO-ERROR.
            IF AVAILABLE EstPedido THEN 
            DO: 
                ASSIGN
                    Respuesta = "La factura de credito ya fue embarcada, cancelacion no procede."
                    IdError   = TRUE.
                RETURN.
            END.
            FIND FIRST Pedido WHERE Pedido.Id-Pedido = ENTRY(1,Factura.Pedidos)
                AND NOT CAN-DO("02B,FUG",Pedido.Id-Alm) NO-LOCK NO-ERROR.
            /*
                     FIND FIRST Pedido WHERE Pedido.Id-Factura = Factura.Id-Factura
                                         AND NOT CAN-DO("02B,FUG",Pedido.Id-Alm) NO-LOCK NO-ERROR.
            */
            IF AVAILABLE Pedido THEN 
            DO: 
                ASSIGN
                    Respuesta = "La venta no pertenece a su almacen, cancelacion no precede."
                    IdError   = TRUE.
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
                IdError   = TRUE.    
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
            ttFactura.IdTran      = Factura.Id-Tran
            ttFactura.Transporte  = IF AVAILABLE Transporte THEN Transporte.Nombre ELSE ""
            ttFactura.Embarcado   = l-Embarcado
            ttFactura.Devolucion  = l-Devolucion.
         
    
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
     
    END. /* TERMINA DE BUSCAR POR TIPO 3 */  
    
    /* ************************************************************** */ 
    /* BUSCA EN TIPO CONTADO */ 
    IF pTipo = 2 THEN 
    DO:
        
        FIND Remision WHERE Remision.Id-Remision = pIdFactura  NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Remision THEN 
        DO:
            ASSIGN 
                Respuesta = "El folio de la factura no esta registrada"
                IdError   = TRUE.
            RETURN.
        END.
        IF Remision.Tipoventa <> 2 THEN 
        DO:
            ASSIGN 
                Respuesta = "La factura no corresponde con el tipo de movimiento"
                IdError   = TRUE.
            RETURN.
        END.
        IF Remision.FecCanc <> ? THEN 
        DO: 
            DEFINE VARIABLE cCancela2       AS CHARACTER NO-UNDO.
            DEFINE VARIABLE cAutoriza2      AS CHARACTER NO-UNDO.
            DEFINE VARIABLE cMensajeMotivo2 AS CHARACTER NO-UNDO.
    
            /* Obtener nombre de quien cancela */
            IF Remision.UsuarioCanc <> "" THEN 
            DO:
                FIND FIRST Empleado WHERE empleado.Iniciales = Remision.UsuarioCanc NO-LOCK NO-ERROR.
                cCancela2 = IF AVAILABLE Empleado THEN Empleado.Nombre ELSE Remision.UsuarioCanc.
            END.
            ELSE 
            DO:
                cCancela2 = "Desconocido".
            END.
    
            /* Obtener nombre de quien autoriza */
            IF Remision.UsuarioSol <> "" THEN 
            DO:
                FIND FIRST Empleado WHERE empleado.Iniciales = Remision.UsuarioSol NO-LOCK NO-ERROR.
                cAutoriza2 = IF AVAILABLE Empleado THEN Empleado.Nombre ELSE Remision.UsuarioSol.
            END.
            ELSE 
            DO:
                cAutoriza2 = "No especificado".
            END.
    
            /* Manejar el motivo solo si existe */
            IF Remision.Motivo <> "" AND Remision.Motivo <> ? THEN
                cMensajeMotivo2 = " Motivo: " + STRING(Remision.Motivo) + ".".
            ELSE
                cMensajeMotivo2 = "".
    
            /* Construir mensaje final */
            ASSIGN
                Respuesta = "La factura ya fue cancelada el dia " + STRING(Remision.FecCancel) +
                   " por " + cCancela2 + "." +
                   (IF Remision.UsuarioSol <> "" 
                    THEN "Solicitado por: " + cAutoriza2 + "." 
                    ELSE "") + 
                   cMensajeMotivo2     
                IdError   = TRUE.
            RETURN.  
        END. 
        IF Remision.FecCanc <> ? THEN 
        DO:
            ASSIGN 
                Respuesta = "La factura ya fue cancelada"
                IdError   = TRUE.
            RETURN.
        END.
        IF Remision.Facglobal <> "" THEN 
        DO:
            ASSIGN 
                Respuesta = "La Factura se incluyo en una factura global"
                IdError   = TRUE.
            RETURN.
        END.
     
        IF YEAR(TODAY) - YEAR(Remision.FecReg) > 1 THEN 
        DO:
            ASSIGN 
                Respuesta = "La factura tiene mas de un ano, no puede realizarse la cancelacion"
                IdError   = TRUE.
            RETURN.
        END.
        IF YEAR(TODAY) <> YEAR(Remision.FecReg) AND MONTH(TODAY) > 3 THEN 
        DO:
            ASSIGN 
                Respuesta = "No puede cancelar facturas de un ano anterior despues de Marzo."
                IdError   = TRUE.
            RETURN.
        END.
 
 
        
        /* Se pidio esta validacion que tiene Ventas Solo que ellos no pueden cancelar
           Tesoria si las cancela, pero no saben si estaba embarcada, se manda el aviso al Front
           para que coloque mensaje y decidan si cancelan o no  */
        IF INDEX(l-SuperUser, pIdUser) > 0  THEN 
        DO:                                       

            FIND FIRST EstPedido WHERE EstPedido.Id-Factura = Remision.Id-Remision
                AND EstPedido.Estatus >= 6 NO-LOCK NO-ERROR.
            IF AVAILABLE EstPedido THEN 
            DO: 
                ASSIGN
                    l-Embarcado = TRUE.
            //    Respuesta = "La factura de contado ya fue embarcada, cancelacion no procede."
            //    IdError   = TRUE.
          //   RETURN.
            END.
        END.  
     
        IF Remision.FecReg <> TODAY AND LOOKUP(pIdUser,l-SuperUser) = 0  THEN 
        DO:
            IF NOT CAN-DO(l-PermisosLista,pIdUser) THEN 
            DO:
                ASSIGN 
                    Respuesta = "No se permite cancelar factura de distinto dia."
                    IdError   = TRUE.
                RETURN.
            END.
            ELSE 
            DO:
                IF Remision.Pagada = TRUE THEN 
                DO:
                    ASSIGN 
                        Respuesta = "Factura de contado ya pagada, cancelacion no procede."
                        IdError   = TRUE.
                    RETURN.
                END.
                FIND FIRST EstPedido WHERE EstPedido.Id-Factura = Remision.Id-Remision
                    AND EstPedido.Estatus >= 6 NO-LOCK NO-ERROR.
                IF AVAILABLE EstPEdido THEN 
                DO:
                    ASSIGN 
                        Respuesta = "La factura de contado ya fue embarcada, cancelacion no procede."
                        IdError   = TRUE.
                    RETURN.
                END.
                FIND FIRST Pedido WHERE Pedido.Id-Pedido = ENTRY(1,Remision.Pedidos)
                    AND NOT CAN-DO("02B,FUG",Pedido.Id-Alm) NO-LOCK NO-ERROR.
                /*
                         FIND FIRST Pedido WHERE Pedido.Id-Factura = Remision.Id-Remision
                                             AND NOT CAN-DO("02B,FUG",Pedido.Id-Alm) NO-LOCK NO-ERROR.
                */
                IF AVAILABLE Pedido THEN 
                DO:
                    ASSIGN 
                        Respuesta = "La venta no pertenece a su almacen, cancelacion no precede."
                        IdError   = TRUE.
                    RETURN.
                END.
            END.
        END.
        IF INDEX(l-SuperUser, pIdUser) = 0 AND {salt0005.i} = 'MATRIZ' THEN 
        DO:
            IF (((g-Origen = "03A" AND NOT Remision.Id-Ubic MATCHES "*MAS*") OR
                (g-Origen = "11"  AND NOT Remision.Id-Ubic MATCHES "*11*") OR
                (g-Origen = "12"  AND NOT Remision.Id-Ubic MATCHES "*12*") OR
                (g-Origen = "6"   AND NOT Remision.Id-Ubic MATCHES "*6*")  OR
                (g-Origen = "7"   AND NOT Remision.Id-Ubic MATCHES "*7*")  OR
                (g-Origen = "8"   AND NOT Remision.Id-Ubic MATCHES "*8*")  OR
                (g-Origen = "9"   AND NOT Remision.Id-Ubic MATCHES "*9*")  OR 
                (g-Origen = "10"  AND NOT Remision.Id-Ubic MATCHES "*10*")) AND Remision.TipoVenta = 2) THEN 
            DO:
           
                ASSIGN 
                    Respuesta = "No se permite cancelar ventas de diferentes tiendas."
                    IdError   = TRUE.
                RETURN.
            END.
        END.
    
        IF MONTH(TODAY) <> MONTH(Remision.FecReg) OR
            YEAR(TODAY) <> YEAR(Remision.FecReg) THEN 
        DO:
            IF LOOKUP(pIdUser,l-SuperUser) = 0  THEN 
            DO:
                ASSIGN 
                    Respuesta = "No se permite cancelar factura de distinto mes."
                    IdError   = TRUE.
                RETURN.
            END.
            ELSE 
            DO:
            
                IF NOT pConfirmar THEN 
                DO:
                    ASSIGN 
                        Respuesta = "La Remision sera CANCELADA aun y cuando es de otro mes."
                        IdError   = FALSE.
                    RETURN.
                END.      
            END.
        END.

        FIND FIRST Devolucion WHERE Devolucion.Id-Factura = Remision.Id-Remision
            AND Devolucion.TipoVenta <> 3 NO-LOCK NO-ERROR.
        ASSIGN 
            l-parcial = FALSE.
        IF AVAILABLE Devolucion AND NOT Devolucion.VtaCanc AND
            Devolucion.FecCanc = ?  THEN 
        DO:
            ASSIGN 
                l-parcial = TRUE
                Respuesta = "La Factura tiene articulos registrados por devolucion" +
    " por lo tanto no se permite cancelar facturas parciales. En caso de " +
    " querer realizar la cancelacion hagalo con una devolucion."
                IdError   = TRUE.
            RETURN.  
        END.

        FIND Cliente WHERE Cliente.id-cliente = Remision.id-cliente NO-LOCK
            NO-ERROR.
        FIND Ciudad WHERE Ciudad.id-ciudad = Remision.id-ciudad NO-LOCK NO-ERROR.
        IF AVAILABLE Ciudad THEN
            FIND Estado OF Ciudad NO-LOCK NO-ERROR.

        ASSIGN 
            l-recid = RECID(Remision)
            l-tipo  = Remision.Tipoventa.
        
        CREATE ttFactura.
        ASSIGN 
            ttFactura.IdFactura   = Remision.Id-Remision
            ttFactura.FecReg      = Remision.FecReg
            ttFactura.IdCliente   = Remision.id-Cliente
            ttFactura.RazonSocial = Remision.RazonSocial
            ttFactura.CalleNo     = Remision.CalleNo
            ttFactura.Colonia     = Remision.Colonia 
            ttFactura.TipoPrecio  = Remision.Tipoprecio  
            ttFactura.CP          = Cliente.CP
            ttFactura.IdVendedor  = Remision.Id-vendedor  
            ttFactura.IdCiudad    = Remision.Id-Ciudad   
            ttFactura.Ciudad      = IF AVAILABLE Ciudad THEN Ciudad.Nombre ELSE " "
            ttFactura.Estado      = IF AVAILABLE Estado THEN Estado.Nomcto ELSE " "
            ttFactura.Subtotal    = Remision.subtotal
            ttFactura.Descuento   = Remision.descuento  
            /*   ttFactura.ImpFlete    = 
                 ttFactura.ImpSeguro   =    */
            ttFactura.IVA         = Remision.Iva    
            ttFactura.Total       = Remision.Tot
            /*  ttFactura.Requisicion =  
                ttFactura.IdTran      = 
                ttFactura.Transporte  = */
            ttFactura.Embarcado   = l-Embarcado
            ttFactura.Devolucion  = l-Devolucion.       
            
        FOR EACH DetRemis OF Remision NO-LOCK:
     
            FIND ArtPres WHERE ArtPres.Id-Articulo = DetRemis.Id-Articulo
                AND ArtPres.Id-Pres = DetRemis.Id-Pres NO-LOCK NO-ERROR.
            ASSIGN 
                l-Pres = IF AVAILABLE ArtPres THEN ArtPres.Descr ELSE ".".  
            CREATE ttDetFactura.
            ASSIGN 
                ttDetFactura.IdFactura      = Remision.Id-Remision
                ttDetFactura.IdArticulo     = DetRemis.id-art
                ttDetFactura.Descripcion    = DetRemis.Descr
                ttDetFactura.Presentacion   = l-Pres
                ttDetFactura.Cantidad       = DetRemis.Cant
                ttDetFactura.PrecioUnitario = DetRemis.PrecUnit
                ttDetFactura.Descuento      = DetRemis.PorcDesc
                ttDetFactura.Importe        = DetRemis.Importe . 

        END.
          
    END. /* */ 
        
END PROCEDURE.   


