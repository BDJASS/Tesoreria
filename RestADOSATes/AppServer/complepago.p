@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : complepago.p
    Purpose     : 

    Syntax      :

    Description :    

    Author(s)   : sis6
    Created     : Fri May 09 17:30:23 CST 2025
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.
   
DEFINE TEMP-TABLE ttAcuse 
    FIELD IdAcuse     AS CHARACTER
    FIELD FecReg      AS DATE
    FIELD FecDep      AS DATE
    FIELD FecPago     AS DATE
    FIELD IdCliente   AS INTEGER
    FIELD RazonSocial AS CHARACTER    
    FIELD Estatus     AS INTEGER
    FIELD Observ      AS CHARACTER
    FIELD FormaPago   AS CHARACTER
    FIELD oError      AS CHARACTER.
    
DEFINE TEMP-TABLE ttDocAcuse
    FIELD IdAcuse   AS CHARACTER
    FIELD FecDoc    AS DATE
    FIELD Documento AS CHARACTER
    FIELD Descr     AS CHARACTER
    FIELD ImpPago   AS DECIMAL
    FIELD ImpDescPP AS DECIMAL
    FIELD DescEsp   AS DECIMAL
    FIELD ImpDevol  AS DECIMAL
    FIELD Dias      AS INTEGER.
    
DEFINE TEMP-TABLE ttPagoAcuse
    FIELD IdAcuse     AS CHARACTER
    FIELD IdTP        AS INTEGER
    FIELD Descr       AS CHARACTER
    FIELD ImpRecibido AS DECIMAL
    FIELD FecCheque   AS DATE
    FIELD Cheque      AS CHARACTER
    FIELD IdBanco     AS INTEGER
    FIELD NomBanco    AS CHARACTER.
    
DEFINE TEMP-TABLE ttTotPagoAcuse
    FIELD IdAcuse   AS CHARACTER
    FIELD TotPago   AS DECIMAL
    FIELD TotCambio AS DECIMAL
    FIELD TotCred   AS DECIMAL.
    
DEFINE DATASET dsAcuse FOR 
    ttAcuse,
    ttDocAcuse,
    ttTotPagoAcuse, 
    ttPagoAcuse 
    DATA-RELATION DatosAcuse FOR ttAcuse, ttDocAcuse
    RELATION-FIELDS (IdAcuse, IdAcuse) NESTED
    DATA-RELATION DetalleAcuse FOR ttAcuse, ttTotPagoAcuse
    RELATION-FIELDS (IdAcuse, IdAcuse) NESTED
    DATA-RELATION PagosAcuse FOR ttTotPagoAcuse, ttPagoAcuse
    RELATION-FIELDS (IdAcuse, IdAcuse) NESTED.
    
DEFINE VARIABLE l-SuperUser AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-hora      AS CHAR      FORMAT "x(8)".

DEF    VAR      l-totrec    LIKE PagoAcuse.ImpRecibido.
DEF    VAR      l-cambio    AS DECIMAL   FORMAT 'ZZ,ZZ9.99'.
DEF    VAR      l-tc        LIKE PagoAcuse.TC FORMAT 'Z9.99'.
DEF    VAR      l-Serie     AS CHAR      NO-UNDO.
DEF    VAR      l-Folio     LIKE Folio.Folio NO-UNDO.
DEF    VAR      l-CPago     LIKE CPago.Id-CPago NO-UNDO.

DEF BUFFER b-CPago FOR CPago.

DEF    VAR      l-MetodoDePago AS CHAR      NO-UNDO.
DEF    VAR      v-listaMes     AS CHAR      NO-UNDO 
    INITIAL 'ENE,FEB,MAR,ABR,MAY,JUN,JUL,AGO,SEP,OCT,NOV,DIC'.
DEF    VAR      l-Suma         AS DECIMAL   NO-UNDO.
DEF    VAR      l-TIva         AS DECIMAL   NO-UNDO INITIAL 0.
DEF    VAR      l-CPAnter      LIKE CPago.Id-CPago NO-UNDO.
DEF    VAR      l-TipoCambio   AS DECIMAL   DECIMALS 4 NO-UNDO.
DEF    VAR      v-RFiscal      LIKE Factura.Id-RFiscal NO-UNDO.
DEF    VAR      v-CPFiscal     LIKE Factura.CP NO-UNDO.

DEF    VAR      l-Moneda       LIKE CPago.Id-Moneda NO-UNDO.
DEF    VAR      l-PMoneda      AS CHAR      NO-UNDO.
DEF    VAR      l-FMoneda      AS CHAR      NO-UNDO.
DEF    VAR      l-ImpPago      LIKE DocAcuse.ImpPago NO-UNDO.
DEF    VAR      l-SaldoFin     LIKE DocAcuse.SaldoFin NO-UNDO.

DEF    VAR      l-FactorPago   AS DECIMAL   DECIMALS 6 NO-UNDO.
DEF    VAR      l-ImporteDR    AS DECIMAL   DECIMALS 6 NO-UNDO.
DEF    VAR      l-BaseDR       AS DECIMAL   NO-UNDO.

DEFINE VARIABLE g-IVA          AS INTEGER   NO-UNDO.

/* Para manejo de conexion con web service */
DEF    VAR      hWS            AS HANDLE.
DEF    VAR      hEmiteCFDSoap  AS HANDLE.
DEF    VAR      vlconnect      AS LOGICAL   NO-UNDO.
DEF    VAR      vldisconnect   AS LOGICAL   NO-UNDO.
DEF    VAR      servidor       AS CHAR      NO-UNDO.

/* Variables para funcion del web service */
DEF    VAR      v-respuesta    AS CHAR      NO-UNDO.
DEF    VAR      v-comprobante  AS LONGCHAR  NO-UNDO.
DEF    VAR      v-UUID         AS LONGCHAR  NO-UNDO.
    
/* Variables para guardar cada una de las etiquetas del xml */
DEF    VAR      v-emisor       AS CHAR      NO-UNDO.
DEF    VAR      v-expEn        AS CHAR      NO-UNDO.
DEF    VAR      v-recep        AS CHAR      NO-UNDO.
DEF    VAR      v-dom          AS CHAR      NO-UNDO.
DEF    VAR      v-enca         AS CHAR      NO-UNDO.
DEF    VAR      v-deta         AS LONGCHAR  NO-UNDO.
DEF    VAR      v-pago         AS LONGCHAR  NO-UNDO.
DEF    VAR      v-concep       AS LONGCHAR  NO-UNDO.
DEF    VAR      v-impsto       AS CHAR      NO-UNDO.
DEF    VAR      v-correo       AS CHAR      NO-UNDO.
DEF    VAR      v-cfdirel      AS LONGCHAR  NO-UNDO.

DEF    VAR      v-CalleNo      AS CHAR      NO-UNDO.
DEF    VAR      v-exped        AS CHAR      NO-UNDO.
DEF    VAR      v-rfcCte       AS CHAR      NO-UNDO.
DEF    VAR      v-letras       AS CHAR      NO-UNDO.
DEF    VAR      v-reng         AS INT       NO-UNDO.
DEF    VAR      v-razon        AS CHAR      NO-UNDO.
DEF    VAR      i              AS INT       NO-UNDO.

DEF    VAR      v-rfc          AS CHAR      NO-UNDO.
DEF    VAR      v-serie        AS CHAR      NO-UNDO.
DEF    VAR      v-folio        AS CHAR      NO-UNDO.

/* Variables para extraer los valores de la respuesta del WebService */
DEF    VAR      v-valores      AS CHAR      EXTENT 4 NO-UNDO.
DEF    VAR      v-tam          AS INT       NO-UNDO.
DEF    VAR      v-ind          AS INT       NO-UNDO.
DEF    VAR      v-pos          AS INT       NO-UNDO.
DEF    VAR      v-siz          AS INT       NO-UNDO.
DEF    VAR      v-num          AS INT       NO-UNDO.
DEF    VAR      l-Intent       AS INT       NO-UNDO.
DEF    VAR      l-MaxInt       AS INT       NO-UNDO INITIAL 3.

/* Variables para envio de correo de notificacion */
DEFINE VARIABLE l-Asunto       AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-Contenido    AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-MailDe       AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-ResponderA   AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-Mail         AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-Enviado      AS LOGICAL   NO-UNDO.

DEFINE VARIABLE l-Mensaje      AS CHARACTER NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetComplePago:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER Acuse AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER IdUser AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcJson AS LONGCHAR NO-UNDO.

    FIND adosa.URL WHERE adosa.URL.Parametro = "CompPagoCyC" NO-LOCK NO-ERROR.
    IF AVAILABLE adosa.URL THEN ASSIGN l-SuperUser = adosa.URL.Valor.

    CREATE ttAcuse.

    IF LOOKUP(IdUser,l-SuperUser) = 0 THEN 
    DO:
        ASSIGN 
            ttAcuse.oError = "Usuario No Permitido".  
        RELEASE ttAcuse.  
        DATASET dsAcuse:WRITE-JSON("LONGCHAR", opcJson, TRUE).
        RETURN.
    END.   
    MESSAGE "Procesando complepago.p del Usuario: " + STRING(IdUser) VIEW-AS ALERT-BOX. 
    FIND Acuse WHERE Acuse.Id-Acuse = Acuse NO-LOCK NO-ERROR.

    IF NOT AVAILABLE Acuse THEN 
    DO:                  
        ASSIGN 
            ttAcuse.oError = "El Acuse no existe.".
        RELEASE ttAcuse.
        DATASET dsAcuse:WRITE-JSON("LONGCHAR", opcJson, TRUE).
        RETURN.
    END.
    IF Acuse.Estatus <> 4 THEN 
    DO:
                  
        ASSIGN 
            ttAcuse.oError = "El Acuse no esta depositado...".
        RELEASE ttAcuse.
        DATASET dsAcuse:WRITE-JSON("LONGCHAR", opcJson, TRUE).
        RETURN.
    END.
    IF Acuse.Tipo <> "N" THEN 
    DO:                  
        ASSIGN 
            ttAcuse.oError = "Solo se pueden corregir acuses NORMALES, no ANTICIPOS...".
        RELEASE ttAcuse.
        DATASET dsAcuse:WRITE-JSON("LONGCHAR", opcJson, TRUE).
        RETURN.
    END. 
    FIND FIRST CPago WHERE CPago.Id-Acuse = Acuse.Id-Acuse
        AND CPago.FecCanc = ?
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE CPago THEN 
    DO:
                  
        ASSIGN 
            ttAcuse.oError = "El Acuse no tiene complemento de pago ...".
        RELEASE ttAcuse.
        DATASET dsAcuse:WRITE-JSON("LONGCHAR", opcJson, TRUE).
        RETURN.
    END. 
    IF CPago.FecReg < TODAY - 90 THEN 
    DO:
                  
        ASSIGN 
            ttAcuse.oError = "El complemento de pago es muy antiguo ...".
        IF IdUser <> "NCR" AND IdUser <> "RGP" THEN 
        DO:
            RELEASE ttAcuse.
            DATASET dsAcuse:WRITE-JSON("LONGCHAR", opcJson, TRUE).
            RETURN.
        END.
    END.
               
               
    FIND FIRST Acuse WHERE Acuse.Id-Acuse = Acuse NO-LOCK NO-ERROR.
               
    IF AVAILABLE Acuse THEN 
    DO:
        ASSIGN 
            l-hora = (IF LENGTH(Acuse.Comen[3]) >= 40 THEN 
                          SUBSTRING(Acuse.Comen[3],42,50) 
                       ELSE "SIN IMPR").
                       
        ASSIGN 
            ttAcuse.IdAcuse     = Acuse.Id-Acuse
            ttAcuse.IdCliente   = Acuse.Id-Cliente
            ttAcuse.FecReg      = Acuse.FecReg
            ttAcuse.RazonSocial = CPago.RazonSocial
            ttAcuse.Observ      = Acuse.Comen[1] + " " + Acuse.Comen[2] + " " + Acuse.Comen[3]
            ttAcuse.Estatus     = Acuse.Estatus
            ttAcuse.FecDep      = Acuse.FecDep
            ttAcuse.FecPago     = CPago.FecPag
            ttAcuse.FormaPago   = CPago.FormaDePago.
        
        FOR EACH DocAcuse WHERE DocAcuse.Id-Acuse = Acuse.Id-Acuse NO-LOCK:
            FIND TabMC WHERE TabMC.Id-MC = DocAcuse.Id-MC NO-LOCK NO-ERROR.
            
            CREATE ttDocAcuse.
            
            ASSIGN 
                ttDocAcuse.IdAcuse   = DocAcuse.Id-Acuse
                ttDocAcuse.FecDoc    = DocAcuse.FecDoc
                ttDocAcuse.Documento = DocAcuse.Documento
                ttDocAcuse.Descr     = IF AVAILABLE TabMC THEN TabMC.Descr ELSE ""
                ttDocAcuse.ImpPago   = DocAcuse.ImpPago
                ttDocAcuse.ImpDescPP = DocAcuse.ImpDescPP
                ttDocAcuse.DescEsp   = DocAcuse.ImpDescEsp + DocAcuse.ImpDescAdc
                ttDocAcuse.ImpDevol  = DocAcuse.ImpDevol
                ttDocAcuse.Dias      = Acuse.FecDep - DocAcuse.FecDoc.
            
            RELEASE ttDocAcuse.
        
        END.  
        
        ASSIGN 
            l-totrec = 0
            l-cambio = 0
            l-tc     = 0.
        
        FOR EACH PagoAcuse WHERE PagoAcuse.Id-Acuse = Acuse.Id-Acuse NO-LOCK:
            
            FIND TipoPago OF PagoAcuse NO-LOCK NO-ERROR.
            IF AVAILABLE TipoPago THEN 
            DO:
                IF TipoPago.MN THEN
                    ASSIGN l-totrec = l-totrec + PagoAcuse.ImpRecibido
                        l-cambio = l-cambio + PagoAcuse.ImpRecibido - PagoAcuse.Importe.
                ELSE
                    ASSIGN l-totrec = l-totrec + (PagoAcuse.ImpRecibido * PagoAcuse.TC)
                        l-cambio = l-cambio +
                                  ((PagoAcuse.ImpRecibido - PagoAcuse.Importe) *
                                    PagoAcuse.TC)
                        l-tc     = PagoAcuse.TC.
            END.
            ELSE
                ASSIGN l-totrec = l-totrec + PagoAcuse.ImpRecibido
                    l-cambio = l-cambio + PagoAcuse.ImpRecibido - PagoAcuse.Importe.
            CREATE ttPagoAcuse.
            ASSIGN 
                ttPagoAcuse.IdAcuse     = PagoAcuse.Id-Acuse
                ttPagoAcuse.IdTP        = PagoAcuse.Id-Tp
                ttPagoAcuse.ImpRecibido = PagoAcuse.ImpRecibido
                ttPagoAcuse.FecCheque   = PagoAcuse.FecCheque
                ttPagoAcuse.Cheque      = PagoAcuse.Cheque
                ttPagoAcuse.IdBanco     = PagoAcuse.Id-Banco.
            IF PagoAcuse.Id-dev > 0 THEN
                ASSIGN ttPagoAcuse.Descr = 'DEV. ' + STRING(PagoAcuse.Id-dev).
            ELSE 
            DO:
                FIND TipoPago OF PagoAcuse NO-LOCK NO-ERROR .
                ASSIGN 
                    ttPagoAcuse.NomBanco = ''
                    ttPagoAcuse.Descr    = TipoPago.Descr .
                IF TipoPago.Descr MATCHES '*CHEQUE*' THEN 
                DO:
                    FIND Banco WHERE Banco.Id-Banco = PagoAcuse.Id-Banco NO-LOCK NO-ERROR.
                    ASSIGN 
                        ttPagoAcuse.NomBanco = IF AVAILABLE Banco THEN Banco.Nombre ELSE ''.
                END.
                ELSE 
                DO:
                    FIND TarjetaC WHERE TarjetaC.Id-tarjeta = PagoAcuse.Id-banco NO-LOCK
                        NO-ERROR.
                    ASSIGN 
                        ttPagoAcuse.NomBanco = IF AVAILABLE TarjetaC THEN TarjetaC.Nombre ELSE ''.
                END.
            END.
            
            RELEASE ttPagoAcuse.
            
        END.   
        
        CREATE ttTotPagoAcuse.
        
        ASSIGN 
            ttTotPagoAcuse.IdAcuse   = Acuse.Id-Acuse
            ttTotPagoAcuse.TotPago   = l-totrec
            ttTotPagoAcuse.TotCambio = l-cambio
            ttTotPagoAcuse.TotCred   = l-tc.
               
        RELEASE ttTotPagoAcuse.
        RELEASE ttAcuse.
    
    END.
    DATASET dsAcuse:WRITE-JSON("LONGCHAR", opcJson, TRUE).
END PROCEDURE.

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE PostPagoAcuse:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER  Acuse         AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER  l-FormaDePago AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER  l-FecPag      AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER  IdUser AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER Mensaje AS CHARACTER NO-UNDO.
    
    
    
    
    MESSAGE "Complemento POST del Usuario: " + STRING(IdUser) + " Acuse: " + STRING(Acuse)+
              " Forma " + STRING(l-FormaDePago) + " Fecha: " + STRING(l-FecPag) VIEW-AS ALERT-BOX. 
    FIND Acuse WHERE Acuse.Id-Acuse = Acuse EXCLUSIVE-LOCK NO-ERROR.
    
    FIND FIRST CPago WHERE CPago.Id-Acuse = Acuse.Id-Acuse
        AND CPago.FecCanc = ?
        NO-LOCK NO-ERROR.
        
    /* Buscar el CPago original a copiar */
    FIND b-CPago WHERE b-CPago.Id-CPago = CPago.Id-CPago EXCLUSIVE-LOCK NO-ERROR.
    
    FIND FIRST SysGeneral NO-LOCK NO-ERROR.
    ASSIGN 
        g-IVA = SysGeneral.Porc-IVA.
    
    IF AVAILABLE b-CPago THEN 
    DO:
        ASSIGN 
            l-CPAnter = b-CPago.Id-CPago.
        /* Obtener nuevo folio */
        FIND Folio WHERE Folio.Id-Doc = "CPAGO" EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE Folio THEN 
        DO:
            l-Serie = Folio.Prefijo.
            l-Folio = Folio.Folio.
            l-CPago = Folio.Prefijo + STRING(Folio.Folio,"9999999999").
            ASSIGN 
                Folio.Folio = Folio.Folio + 1.
        END.
        ELSE RETURN.

        RELEASE Folio.

        /* Crear el nuevo CPago y copiar datos del original */
        CREATE CPago.
        BUFFER-COPY b-CPago TO CPago
            ASSIGN 
            CPago.Id-CPago    = l-CPago
            CPago.FecReg      = TODAY
            CPago.FecPag      = l-FecPag
            CPago.FormaDePago = l-FormaDePago
            CPago.UUID        = "".

        /* Cancelar el anterior */
        ASSIGN 
            b-CPago.FecCanc     = TODAY
            b-CPago.UsuarioCanc = IdUser
            Acuse.Id-CPago      = l-CPago.

    END.
    ELSE 
    DO:
        /* Si no encontró el original, detener proceso con mensaje */
        ASSIGN 
            Mensaje = "No se encontró el complemento de pago original para copiacr.".   
        RETURN.
    END.
    
    FIND FIRST PagoAcuse OF Acuse WHERE PagoAcuse.CPFormaPago <> "" NO-LOCK NO-ERROR.
    l-PMoneda    = "MXN".
    l-Moneda     = 1.
    l-TipoCambio = 1.
    IF PagoAcuse.Id-Moneda = 3 OR PagoAcuse.Id-Moneda = 5 THEN
        ASSIGN l-PMoneda    = "USD"
            l-Moneda     = 3
            l-TipoCambio = PagoAcuse.TipoCambio.

    /* Traer los datos necesarios de las demas tablas */
    FIND FIRST DocAcuse OF Acuse NO-LOCK NO-ERROR.
    FIND FIRST PagoAcuse OF Acuse NO-LOCK NO-ERROR.
    FIND Cliente OF Acuse NO-LOCK NO-ERROR.
    
    l-Suma = 0.
    FOR EACH DocAcuse OF Acuse NO-LOCK:
        IF DocAcuse.Id-Moneda <> PagoAcuse.Id-Moneda THEN 
        DO:
            IF DocAcuse.Id-Moneda > 1 AND PagoAcuse.Id-Moneda = 1 THEN 
            DO:
                l-Suma = l-Suma + (DocAcuse.ImpPago * PagoAcuse.TipoCambio).
                l-TipoCambio = PagoAcuse.TipoCambio.
            END.
            ELSE IF DocAcuse.Id-Moneda = 1 AND PagoAcuse.Id-Moneda > 1 THEN 
                DO:
                    l-Suma = l-Suma + (DocAcuse.ImpPago / PagoAcuse.TipoCambio).
                    l-TipoCambio = PagoAcuse.TipoCambio.
                END.
        END.
        ELSE l-Suma = l-Suma + DocAcuse.ImpPago.
    END.

    IF Acuse.Id-Cliente <> 3 THEN 
    DO:
        IF LENGTH(Cliente.RFC) = 12 AND Cliente.NomEmpresa > "" THEN
            v-razon = TRIM(Cliente.NomEmpresa).
        ELSE IF Cliente.Tipo = 1 THEN v-razon = TRIM(Cliente.Propietario).
            ELSE v-razon = TRIM(Cliente.RazonSocial).

        ASSIGN 
            CPago.RazonSocial = v-razon
            CPago.RFC         = CAPS(TRIM(REPLACE(Cliente.Rfc," ",""))).

        v-CPFiscal = TRIM(Cliente.CP).
        IF Cliente.RFC = "XAXX010101000" OR Cliente.RFC = "XEXX010101000"
            THEN ASSIGN v-RFiscal  = "616"
                v-CPFiscal = "64000".
        ELSE v-RFiscal = TRIM(Cliente.Id-RFiscal).
    END.
    ELSE 
    DO:
        FIND FIRST DocAcuse OF Acuse NO-LOCK NO-ERROR.
        FIND Cliente OF Acuse NO-LOCK NO-ERROR.
        FIND Factura WHERE Factura.Id-Factura = DocAcuse.Documento NO-LOCK NO-ERROR.
        IF AVAILABLE Factura THEN 
        DO:
            IF LENGTH(Factura.RFC) = 12 AND Factura.NomEmpresa > "" THEN
                v-razon = TRIM(Factura.NomEmpresa).
            ELSE v-razon = TRIM(Factura.RazonSocial).
            ASSIGN 
                CPago.RazonSocial = v-razon
                CPago.Rfc         = CAPS(TRIM(REPLACE(Factura.Rfc," ",""))).
            v-CPFiscal = TRIM(Factura.CP).
            IF Factura.RFC = "XAXX010101000" OR Factura.RFC = "XEXX010101000"
                THEN ASSIGN v-RFiscal  = "616"
                    v-CPFiscal = "64000".
            ELSE v-RFiscal = TRIM(Factura.Id-RFiscal).
        END.
    END.
    v-razon = REPLACE(v-razon,CHR(38),'&#38;').
    v-razon = REPLACE(v-razon,'"', '&quot;').
    v-razon = TRIM(REPLACE(v-razon,'|',' ')).
    v-razon = REPLACE(v-razon,"'",'&apos;').
    v-razon = REPLACE(v-razon,'<','&lt;').
    v-razon = REPLACE(v-razon,'>','&gt;').
    v-razon = REPLACE(v-razon,CHR(165),'&#209;').
    v-razon = REPLACE(v-razon,CHR(154),'&#209;').
    RUN Colapsa(INPUT-OUTPUT v-razon).
       
    /* 0. Encabezado de XML */
    v-comprobante = '<?xml version="1.0" encoding="utf-8"?><Comprobante xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" version="4.0" '.
    v-comprobante = v-comprobante + ' serie="' + l-Serie + '" '.
    v-comprobante = v-comprobante + ' LugarExpedicion="64000" '.
    v-exped = "Expedida en Monterrey, N.L.".
    v-expEn = '<ExpedidoEn calle="Zaragoza Norte" noExterior="435" noInterior="" colonia="Monterrey Centro" localidad="" referencia="" municipio="Monterrey" estado="Nuevo Leon" pais="Mexico" codigoPostal="64000"/>'.    
    servidor = "-WSDL http://192.0.1.7/wseDocEmiteCFD/wsEmiteCFD.asmx?wsdl".

    /*Cambio Junio 2011: la forma de pago debe ser PAGO EN UNA SOLA EXHIBICION*/
    v-comprobante = v-comprobante + 'formaDePago="99" '.

    v-correo = "".
    IF Acuse.CPBuzonFiscal <> "" THEN 
        v-correo = Acuse.CPBuzonFiscal.
    ELSE IF Cliente.Id-Cliente >= 12 AND Cliente.CPBuzonFiscal <> "" THEN 
            v-correo = Cliente.CPBuzonFiscal.
        ELSE IF Cliente.Id-Cliente >= 12 AND Cliente.BuzonFiscal <> "" THEN 
                v-correo = Cliente.BuzonFiscal.
       
    /*
    IF v-correo > "" THEN v-correo = v-correo + ";flucio@adosa.com.mx".
    ELSE v-correo = "flucio@adosa.com.mx".
    */

    v-correo = REPLACE(v-correo,'&','&amp;').


    v-rfcCte = REPLACE(CPago.rfc,'&','&amp;').
    v-rfcCte = TRIM(REPLACE(v-rfcCte,' ','')).

    v-exped = "Expedida en Monterrey, N.L.".

    /* 1. Datos de Facturacion del Cliente o Receptor */
    IF CPago.Id-Cliente = 3 THEN 
    DO:
    
        /* Corregir caracter especial en la direccion */
        v-CalleNo = REPLACE(Factura.CalleNo,'&','&amp;').
        v-CalleNo = REPLACE(v-CalleNo,'"', '&quot;').
        v-CalleNo = TRIM(REPLACE(v-CalleNo,'|',' ')).
        v-CalleNo = REPLACE(v-CalleNo,"'",'&apos;').
        v-CalleNo = REPLACE(v-CalleNo,'<','&lt;').
        v-CalleNo = REPLACE(v-CalleNo,'>','&gt;').
    
        v-recep = '<Receptor rfc="' + v-rfcCte + '" RegimenFiscalReceptor="' + v-RFiscal + '" DomicilioFiscalReceptor="' + v-CPFiscal + '" UsoCFDI="CP01" nombre="' + v-razon + '" >'.
    
        v-enca = '<ImprimeEncabezado DecimalesXml="2" '.
        v-enca = v-enca + 'CodigoBarra="' + l-CPago + '" RazonSocial="' + v-razon + '" RFC="R.F.C. ' + v-rfcCte + '" '.
        IF v-correo <> "" THEN v-enca = v-enca + 'correo="' + v-correo + '" '.
    
        v-dom = '<Domicilio calle="' + TRIM(v-CalleNo) + '" noExterior="" noInterior="" colonia="' + TRIM(Factura.Colonia) + '" localidad="" '.
        v-enca = v-enca + 'CalleNo="" Colonia="" Delegacion="" '.
        
        /*RUN LlenaDomicilio(INPUT-OUTPUT v-enca, INPUT Cliente.Id-Ciudad, INPUT 'Ciudad', INPUT 'Estado', INPUT 'Pais').*/
        v-enca = v-enca + 'Ciudad="" Estado="" Pais="" '.
     
        RUN LlenaDomicilio(INPUT-OUTPUT v-dom, INPUT Factura.Id-Ciudad, INPUT 'municipio', INPUT 'estado', INPUT 'pais').
    
        v-dom = v-dom + 'codigoPostal="' + (IF LENGTH(TRIM(Factura.CP)) <> 0 THEN TRIM(Factura.CP) ELSE '') + '" />'.
        RUN Colapsa(INPUT-OUTPUT v-dom).
    
        /*v-enca = v-enca + 'CP="' + (IF AVAILABLE Cliente AND LENGTH(TRIM(Cliente.CP)) <> 0 THEN 'CP ' + TRIM(Cliente.CP) ELSE '') + '" '.*/
        v-enca = v-enca + 'CP="" '.
    

    END.
    ELSE 
    DO:
    
        /* Corregir caracter especial en la direccion */
        v-CalleNo = REPLACE(Cliente.CalleNo,'&','&amp;').
        v-CalleNo = REPLACE(v-CalleNo,'"', '&quot;').
        v-CalleNo = TRIM(REPLACE(v-CalleNo,'|',' ')).
        v-CalleNo = REPLACE(v-CalleNo,"'",'&apos;').
        v-CalleNo = REPLACE(v-CalleNo,'<','&lt;').
        v-CalleNo = REPLACE(v-CalleNo,'>','&gt;').
    
        v-recep = '<Receptor rfc="' + v-rfcCte + '" RegimenFiscalReceptor="' + v-RFiscal + '" DomicilioFiscalReceptor="' + v-CPFiscal + '" UsoCFDI="CP01" nombre="' + v-razon + '" >'.
    
        v-enca = '<ImprimeEncabezado DecimalesXml="2" '.
        v-enca = v-enca + 'CodigoBarra="' + l-CPago + '" RazonSocial="' + v-razon + '" RFC="R.F.C. ' + v-rfcCte + '" '.
        IF v-correo <> "" THEN v-enca = v-enca + 'correo="' + v-correo + '" '.
    
        v-dom = '<Domicilio calle="' + TRIM(v-CalleNo) + '" noExterior="" noInterior="" colonia="' + TRIM(Cliente.Colonia) + '" localidad="" '.
        v-enca = v-enca + 'CalleNo="" Colonia="" Delegacion="" '.
        
        /*RUN LlenaDomicilio(INPUT-OUTPUT v-enca, INPUT Cliente.Id-Ciudad, INPUT 'Ciudad', INPUT 'Estado', INPUT 'Pais').*/
        v-enca = v-enca + 'Ciudad="" Estado="" Pais="" '.
        RUN LlenaDomicilio(INPUT-OUTPUT v-dom, INPUT Cliente.Id-Ciudad, INPUT 'municipio', INPUT 'estado', INPUT 'pais').
    
        v-dom = v-dom + 'codigoPostal="' + (IF AVAILABLE Cliente AND LENGTH(TRIM(Cliente.CP)) <> 0 THEN TRIM(Cliente.CP) ELSE '') + '" />'.
        RUN Colapsa(INPUT-OUTPUT v-dom).
    
        /*v-enca = v-enca + 'CP="' + (IF AVAILABLE Cliente AND LENGTH(TRIM(Cliente.CP)) <> 0 THEN 'CP ' + TRIM(Cliente.CP) ELSE '') + '" '.*/
        v-enca = v-enca + 'CP="" '.
    
    END.


    /* 1.1 Datos de Embarque */
    v-enca = v-enca + 'EmbAtencion="" EmbRazonSocial="" EmbCalleNo="" EmbColonia="" EmbDelegacion="" EmbCiudad="" EmbEstado="" EmbPais="" EmbCP="" EmbTel="" EmbFax="" Transporte="" '.

    /* 2. Datos de Venta y Totales */
    v-comprobante = v-comprobante + 'condicionesDePago="CONTADO" '.
    v-enca = v-enca + 'Condicion="CONTADO" '.

    v-enca = v-enca + 'Vencimiento="" Autorizacion="" Pedido="" Requisicion="" '. 
    v-enca = v-enca + 'cliente="' + STRING(CPago.Id-Cliente) + '" '.
    v-enca = v-enca + 'Fecha="' + STRING(DAY(CPago.FecReg),">9") + '-' + ENTRY(MONTH(CPago.FecReg),v-listaMes) + '-' + STRING(YEAR(CPago.FecReg),">>>9") + '" '.
    v-enca = v-enca + 'Id_Factura="' + CPago.Id-CPago + '" Propietario="" Tarimas="" Bultos="" Entrega="" Vendedor="" Zona="" '.
    v-enca = v-enca + 'Cobrador="" RutaEmbarque="" '.

    /* 2.1 Datos de totales de la factura */
    v-comprobante = v-comprobante + 'subTotal="0"  motivoDescuento="" '.
    v-comprobante = v-comprobante + 'total="0" metodoDePago="' + l-MetodoDePago + '" tipoDeComprobante="P" '.

    /*
    v-comprobante = v-comprobante + 'total="' + STRING(NCR.Tot) + '" metodoDePago="' + l-MetodoDePago + '" NumCtaPago="' + l-NumCtaPago +  
    */

    v-comprobante = v-comprobante + 'ReferenciaCFD="' + CPago.Id-CPago + '" xmlns="http://www.sat.gob.mx/cfd/2">'.

    v-enca = v-enca + 'SubTotal="' + STRING(0,"ZZZ,ZZZ,ZZ9.99") + '" '. 
    v-enca = v-enca + 'Descuento="" '.
    v-enca = v-enca + 'ValorNeto="' + STRING(0,'$ZZZ,ZZZ,ZZ9.99') + '" IVA="'+ STRING(0,"$ZZZ,ZZZ,ZZ9.99") + '" '.
    v-enca = v-enca + 'FleteEtiqueta="" FleteValor="" Total="' + STRING(0,"ZZZ,ZZZ,ZZ9.99") + '" '.

    v-letras = ''.
    /*RUN /usr2/adosa/procs/rtn0005.p(INPUT NCR.Tot, OUTPUT v-letras).*/
    
    v-enca = v-enca + 'DiaPago="" CantLetras="' + v-letras + '" Interes="" Expedida="' + v-exped + '"/>'.

    /* 3. Datos de Renglones de la Nota de Credito dependiendo el tipo de la nota */
    v-reng = 1.

    v-concep = '<Concepto cantidad="1" '. 
    v-concep = v-concep + 'unidad="ACTIVIDAD" ClaveUnidad="ACT" ClaveProdServ="84111506" '.
    v-concep = v-concep + 'noIdentificacion="" descripcion="PAGO" valorUnitario="0" importe="0">'.
    v-concep = v-concep + '</Concepto>'.

    v-deta = v-deta + '<ImprimeDetalle secuencia="' + STRING(v-reng) + '" tipoLinea="1" descripcion="PAGO" unidad="ACT" porcIVA="" tipoPrecio="" '.
    v-deta = v-deta + 'noIdentificacion="" cantidad="1" '.
    v-deta = v-deta + 'valorUnitario="0" importe="0" comentario="" '.
    v-deta = v-deta + 'descuento=""/>'.
    v-reng = v-reng + 1. 

    v-pago = '<Pago FechaPago="' + STRING(YEAR(CPago.FecPag),"9999") + '-' + STRING(MONTH(CPago.FecPag),"99") + '-' + STRING(DAY(CPago.FecPag),"99") + 
        'T12:00:00" ' +
        'FormaDePagoP="' + CPago.FormaDePago + '" MonedaP="' +
        TRIM(l-PMoneda) + '" Monto="' + TRIM(STRING(l-Suma,"zzzzzzzzzzz9.99")) + '"'.
    IF l-Moneda = 3 THEN
        v-pago = v-pago + ' TipoCambioP="' + TRIM(STRING(l-TipoCambio,"zzz9.9999")) + '"'.
    v-pago = v-pago + '> '.
    /*'FormaDePagoP="' + CPago.FormaDePago + '" MonedaP="MXN" Monto="' + TRIM(STRING(l-Suma)) + '" NumOperacion="" ' +
    'RfcEmisorCtaOrd="" NomBancoOrdExt="" CtaOrdenante="" RfcEmisorCtaBen="" CtaBeneficiario=""> '.*/

    FOR EACH DocAcuse OF Acuse WHERE DocAcuse.ImpPago > 0 NO-LOCK:
        FIND Factura WHERE Factura.Id-Factura = DocAcuse.Documento NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Factura THEN NEXT.
        l-FMoneda = "MXN".
        IF DocAcuse.Id-Moneda = 3 OR DocAcuse.Id-Moneda = 5 THEN
            l-FMoneda = "USD".

        v-pago = v-pago + '<DoctoRelacionado IdDocumento="' + Factura.UUID + '" '.
        IF ENTRY(2,Factura.Folioe) > "" THEN v-pago = v-pago + 'Serie="' + ENTRY(2,Factura.Folioe) + '" '.
        l-ImpPago  = IF DocAcuse.ImpPago > DocAcuse.SaldoAnt
            THEN DocAcuse.SaldoAnt ELSE DocAcuse.ImpPago.
        l-SaldoFin = IF DocAcuse.SaldoFin < 0 THEN 0 ELSE DocAcuse.SaldoFin.
        v-pago = v-pago + 'Folio="' + ENTRY(3,Factura.Folioe) + '" MonedaDR="' +
            TRIM(l-FMoneda) + '" MetodoDePagoDR="PPD" NumParcialidad="' + 
            TRIM(STRING(DocAcuse.NParcialidad)) + '" ' +
            'ImpSaldoAnt="' + TRIM(STRING(DocAcuse.SaldoAnt)) + 
            '" ImpSaldoInsoluto="' + TRIM(STRING(l-SaldoFin)) + '" ' +
            'ImpPagado="' + TRIM(STRING(l-ImpPago)) + '"'.
        /*
            IF (DocAcuse.Id-Moneda = 3 OR DocAcuse.Id-Moneda = 5) AND l-Moneda = 1 THEN
        */
        IF (DocAcuse.Id-Moneda <> l-Moneda) THEN
            v-pago = v-pago + ' TipoCambioDR="' + TRIM(STRING(l-TipoCambio,"zzz9.9999")) + '"'.
        v-pago = v-pago + '> '.

        l-FactorPago = l-ImpPago / Factura.Tot.
        IF l-FactorPago > 1 OR ABS(Factura.Tot - l-ImpPago) < 1 THEN 
            l-FactorPago = 1.
        IF l-FactorPago < 1 THEN 
        DO:
            l-ImporteDR = ROUND(Factura.Iva * l-FactorPago,6).
            l-BaseDR = ROUND(l-ImporteDR / (g-iva / 100),2).
        /*l-ImporteDR = ROUND(l-BaseDR * (g-iva / 100),6).*/
        /*
               IF DocAcuse.Id-Moneda <> PagoAcuse.Id-Moneda THEN DO:
                  IF DocAcuse.Id-Moneda > 1 AND PagoAcuse.Id-Moneda = 1 THEN DO:
                     l-BaseDR = ROUND(l-BaseDR * PagoAcuse.TipoCambio,2).
                  END.
                  ELSE IF DocAcuse.Id-Moneda = 1 AND PagoAcuse.Id-Moneda > 1 THEN DO:
                     l-BaseDR = ROUND(l-BaseDR / PagoAcuse.TipoCambio,2).
                  END.
               END.
        */
        END.
        ELSE 
        DO:
            l-ImporteDR = ROUND(Factura.Iva,6).
            l-BaseDR = ROUND(l-ImporteDR / (g-iva / 100),2).
        END.
        IF l-BaseDR > 0 THEN 
        DO:
            v-pago = v-pago + '<TrasladoDR ' + 
                'BaseDR="' + TRIM(STRING(l-BaseDR,"zzzzzzzzzzz9.99")) + 
                '" ImpuestoDR="002" TipoFactorDR="Tasa" TasaOCuotaDR="0.160000" ' +
                'ImporteDR="' + TRIM(STRING(l-ImporteDR,"zzzzzzzzzzz9.999999")) + 
                '"/>'.
        END.
        ELSE 
        DO:
            l-BaseDR = l-ImpPago.
            l-ImporteDR = 0.
            v-pago = v-pago + '<TrasladoDR ' + 
                'BaseDR="' + TRIM(STRING(l-BaseDR,"zzzzzzzzzzz9.99")) + 
                '" ImpuestoDR="002" TipoFactorDR="Exento"/>'.
        END.
        v-pago = v-pago + '</DoctoRelacionado> '.
    END.
    v-pago = v-pago + "</Pago>".

    
    
    /* 3.1 Impuestos */
    /* ************************************************************************************************************************************* */
    /* Cuidar que la tasa este correcta */
    v-impsto = '<Impuestos totalImpuestosTrasladados="' + TRIM(STRING(l-TIva,"zzzzzzzzz9.99")) + '" ><Traslados>'.
    v-impsto = v-impsto + '<Traslado impuesto="IVA" tasa="16" importe="' + TRIM(STRING(l-TIva,"zzzzzzzzz9.99")) + '"/></Traslados></Impuestos>'.

    /* 4. Unir partes de la Factura */ /* Pruebas: AAA010101AAA Prod: AOF870529IU7 */
    v-comprobante = v-comprobante + '<Emisor rfc="AOF870529IU7" nombre="ABASTECEDORA DE OFICINAS">'.
    v-comprobante = v-comprobante + '<DomicilioFiscal calle="Zaragoza Norte" noExterior="435" noInterior="" colonia="Monterrey Centro" localidad="" referencia="" '.
    v-comprobante = v-comprobante + 'municipio="Monterrey" estado="Nuevo Leon" pais="Mexico" codigoPostal="64000"/>'.
    v-comprobante = v-comprobante + v-expEn + ' <RegimenFiscal Regimen="601"/></Emisor>'.

    v-comprobante = v-comprobante + v-enca + v-deta + v-pago + v-recep + v-dom + '</Receptor>'.
    v-comprobante = v-comprobante + '<Conceptos>' + v-concep + '</Conceptos>'.
    v-comprobante = v-comprobante /*+ v-impsto*/ + '</Comprobante>'.
    
    OUTPUT TO VALUE("/usr2/compartido/request/" + CPago.Id-CPago + ".xml").
    EXPORT v-comprobante.
    OUTPUT CLOSE.
     
    /* Realizar llamadas y operaciones con el webService de eDoc */
    
    CREATE SERVER hWS.
    vlconnect = hWS:CONNECT(servidor) NO-ERROR.
            
    IF vlconnect THEN 
    DO:
        //Se habilita en produccion las 3 lineas siguientes
            RUN wsEmiteCFDSoap SET hEmiteCFDSoap ON hws.
            RUN IntentaCFD.  
            vldisconnect = hWS:DISCONNECT().        
    END.   
    ELSE 
    DO:
        ASSIGN 
            Mensaje = " No se pudo conectar al Web Service".
        
    END.
    
    IF l-Mensaje <> "" THEN ASSIGN Mensaje += l-Mensaje.
    
    MESSAGE "Procesando complepago.p POST del Usuario MENSAJE: " + STRING(Mensaje) + ""  + STRING(l-Mensaje) VIEW-AS ALERT-BOX. 
    
    IF l-Mensaje = "" THEN 
    DO :
        RUN EnviaCorreo(INPUT IdUser, INPUT l-CPago).
        RUN CancelaCFD(INPUT l-CPAnter, INPUT IdUser). //Se habilita para producci�n  
        ASSIGN 
            Mensaje = "Ajuste Realizado con Exito ".
    END.  
    
    RELEASE CPago.  
    RELEASE Folio.
    RELEASE Acuse.  
    MESSAGE "Procesando complepago.p POST del Usuario LIBERANDO TABLAS " VIEW-AS ALERT-BOX. 
    
/* Agrega aquí cualquier otra tabla que hayas hecho EXCLUSIVE-LOCK */
    
END PROCEDURE.

PROCEDURE CancelaCFD:
    
    DEFINE INPUT PARAMETER numCPago LIKE CPago.Id-CPago NO-UNDO.
    DEFINE INPUT PARAMETER idUser AS CHARACTER NO-UNDO.

    /* Para manejo de conexion con web service */
    DEF VAR hWS           AS HANDLE.
    DEF VAR hEmiteCFDSoap AS HANDLE.
    DEF VAR vlconnect     AS LOGICAL NO-UNDO.
    DEF VAR servidor      AS CHAR    NO-UNDO.

    /* Variables para funcion del web service */
    DEF VAR v-respuesta   AS CHAR    NO-UNDO.
    DEF VAR v-serie       AS CHAR    NO-UNDO.
    DEF VAR v-folio       AS CHAR    NO-UNDO.
    DEF VAR v-estatus     AS CHAR    NO-UNDO.

    /* Variables para extraer los valores de la respuesta del WebService */
    DEF VAR v-valores     AS CHAR    EXTENT 4 NO-UNDO.
    DEF VAR v-tam         AS INT     NO-UNDO.
    DEF VAR v-ind         AS INT     NO-UNDO.
    DEF VAR v-pos         AS INT     NO-UNDO.
    DEF VAR v-siz         AS INT     NO-UNDO.
    DEF VAR v-num         AS INT     NO-UNDO.

    FIND CPago WHERE CPago.Id-CPago = numCPago NO-LOCK NO-ERROR.

    IF AVAILABLE CPago THEN 
    DO:

        v-estatus = ENTRY(1,CPago.Folioe).
        v-serie   = ENTRY(2,CPago.Folioe).
        v-folio   = ENTRY(3,CPago.Folioe).

    END.
    ELSE
        v-estatus = ''.
   
    IF v-estatus = 'A' THEN 
    DO:
        /* Asigna en que servidor se cancelara */
        servidor = "-WSDL http://192.0.1.7/wseDocEmiteCFD/wsEmiteCFD.asmx?wsdl". 
                               
        /* Realizar llamadas y operaciones con el webService de eDoc */
        CREATE SERVER hWS.
        vlconnect = hWS:CONNECT(servidor) NO-ERROR.
        
        IF vlconnect THEN 
        DO:

            RUN wsEmiteCFDSoap SET hEmiteCFDSoap ON hws.
    
            RUN CancelaFactura IN hEmiteCFDSoap(INPUT 'AOF870529IU7', INPUT v-serie, INPUT v-folio, OUTPUT v-respuesta).    

            OUTPUT TO VALUE("/usr2/compartido/request/" + CPago.Id-CPago + ".canc").
            EXPORT v-respuesta.
            OUTPUT CLOSE.
    
            /* Separar los valores devueltos por el web service */
            v-tam = LENGTH(v-respuesta, "CHARACTER").
            v-ind = 1.
            DO v-num = 1 TO 4:
        
                v-pos = INDEX(v-respuesta,"_",v-ind).
                v-siz = v-pos - v-ind.
            
                IF v-pos = 0 THEN
                    v-siz = v-tam - v-ind + 1.
             
                v-valores[v-num] = SUBSTRING(v-respuesta,v-ind,v-siz).
            
                v-ind = v-pos + 1.
        
            END.
    
            IF v-valores[1] = "ERROR" THEN 
            DO:    
            
                ASSIGN 
                    l-Mensaje = "Ocurrio un error al intentar cancelar el CFDI CPago " + v-valores[3].
            
            END.            
            ELSE 
            DO:
        
                DO TRANSACTION:
            
                    FIND CPago WHERE CPago.Id-CPago = numCPago EXCLUSIVE-LOCK.
                    ASSIGN 
                        CPago.Folioe      = v-valores[1] + ',' + v-valores[3] + ',' + v-valores[4]
                        CPago.FecCanc     = TODAY
                        CPago.UsuarioCanc = idUser.
                    /*    
                    FIND Acuse WHERE Acuse.Id-Acuse = CPago.Id-Acuse
                        EXCLUSIVE-LOCK NO-ERROR.
                    IF AVAILABLE Acuse THEN 
                        ASSIGN  = "".
                        
                    RELEASE Acuse.   */   
                    RELEASE CPago.
            
                END.
            
                ASSIGN 
                    l-Mensaje = "El CFDI CPago se Cancelo con exito, Folio: " + v-valores[1] + "," + v-valores[2] + "," + v-valores[3] + "," + v-valores[4].            
            
            END.
    END.
    ELSE 
    DO:
        
        ASSIGN 
            l-Mensaje = "Ocurrio un error. No se puede conectar con el servicio...".
        
    END.        
END.
ELSE IF v-estatus = 'C' THEN DO:
        
ASSIGN 
    l-Mensaje = "El CFDI CPago ya esta cancelado".
        
END.    
     ELSE DO:
        
ASSIGN 
    l-Mensaje = "El CFDI CPago no a sido generada".
        
END.        
       
END.


PROCEDURE Colapsa:
    DEF INPUT-OUTPUT PARAMETER v-colapsa AS CHAR NO-UNDO.
    DEF VAR v-newStr AS CHAR NO-UNDO.
    DEF VAR v-letra  AS CHAR NO-UNDO.
    DEF VAR v-tot    AS INT  NO-UNDO.
    DEF VAR v-indi   AS INT  NO-UNDO.
    DEF VAR espacios AS INT  NO-UNDO INITIAL 0.
    
    v-tot = LENGTH(v-colapsa, "CHARACTER").
    
    DO v-indi = 1 TO v-tot:
        v-letra = SUBSTRING(v-colapsa,v-indi,1).
        
        IF v-letra = CHR(32) THEN
            espacios = espacios + 1.
        ELSE
            espacios = 0.
        
        IF espacios < 2 THEN
            v-newStr = v-newStr + v-letra.
    END.
    
    v-colapsa = v-newStr.
    
END.

PROCEDURE LlenaDomicilio:
    DEF INPUT-OUTPUT PARAMETER v-dom AS CHAR NO-UNDO.
    DEF INPUT PARAMETER v-city AS INT  NO-UNDO.
    DEF INPUT PARAMETER v-lab1 AS CHAR NO-UNDO.
    DEF INPUT PARAMETER v-lab2 AS CHAR NO-UNDO.
    DEF INPUT PARAMETER v-lab3 AS CHAR NO-UNDO.

    FIND Ciudad WHERE Ciudad.Id-Ciudad = v-city NO-LOCK NO-ERROR.
    
    IF AVAILABLE Ciudad THEN 
    DO:
    
        FIND Estado WHERE Estado.Id-Estado = Ciudad.Id-Estado NO-LOCK NO-ERROR.
        
        IF AVAILABLE Estado THEN 
        DO:
        
            FIND Pais WHERE Pais.Id-Pais = Estado.Id-Pais NO-LOCK NO-ERROR.
            
            IF AVAILABLE Pais THEN 
            DO:
            
                v-dom = v-dom + v-lab1 + '="' + TRIM(Ciudad.Nombre) + '" '.
                v-dom = v-dom + v-lab2 + '="' + TRIM(Estado.Nombre) + '" '.
                v-dom = v-dom + v-lab3 + '="' + TRIM(Pais.Nombre) + '" '.

            END.
        
        END.
    
    END.
    
END.
    

PROCEDURE IntentaCFD.
    l-Intent = 1.
    DO WHILE l-Intent <= l-MaxInt:

        RUN EmiteCFD IN hEmiteCFDSoap(INPUT v-comprobante, OUTPUT v-respuesta).
        RUN RecuperaUUID IN hEmiteCFDSoap(INPUT CPago.Id-CPago, OUTPUT v-UUID).
      
        v-tam = LENGTH(v-respuesta, "CHARACTER").
        v-ind = 1.
        DO v-num = 1 TO 4:
      
            v-pos = INDEX(v-respuesta,"_",v-ind).
            v-siz = v-pos - v-ind.
          
            IF v-pos = 0 THEN
                v-siz = v-tam - v-ind + 1.
           
            v-valores[v-num] = SUBSTRING(v-respuesta,v-ind,v-siz).
          
            v-ind = v-pos + 1.
      
        END.
      
        IF v-valores[1] <> "ERROR" AND v-valores[1] BEGINS 'A' THEN 
        DO: 
      
            v-rfc = v-valores[2].
            v-serie = v-valores[3].
            v-folio = v-valores[4].
              
            DO TRANSACTION:
                ASSIGN 
                    CPago.Folioe = v-valores[1] + ',' + v-serie + ',' + v-folio
                    CPago.UUID   = SUBSTRING(STRING(v-UUID),1,INDEX(STRING(v-UUID),",") - 1).
                RELEASE CPago.
            END.
            LEAVE.
        END.
        ELSE 
        DO:
            IF l-Intent < l-MaxInt THEN l-Intent = l-Intent + 1.
            ELSE 
            DO:
                OUTPUT TO VALUE("/usr2/compartido/request/" + CPago.Id-CPago + ".err").
                EXPORT v-respuesta.
                OUTPUT CLOSE.
                ASSIGN 
                    l-Mensaje = "Ocurrio un error al generar cfdi de comprobante de pago".
                LEAVE.
            END.          
        END.
    END.
END.


PROCEDURE EnviaCorreo.
    DEFINE INPUT PARAMETER idUsuario AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER idCpago LIKE l-CPago NO-UNDO.
    FIND CPago WHERE CPago.Id-CPago = idCpago NO-LOCK NO-ERROR.
    FIND Usuario WHERE Usuario.Id-User = idUsuario NO-LOCK NO-ERROR.
    ASSIGN
        l-Asunto    = "MODIFICACION DE COMPLEMENTO DE PAGO"
        l-Contenido = "<html><head>" +
                      "<p class=MsoNormal align=center style='text-align:left'><span style='font-size:14.0pt;font-family:Verdana'>" +
                      "Se le informa que fue realizada la siguiente modificacion a complemento de pago:<br/><br/>" +
                      "ACUSE: <b>" + CPago.Id-Acuse +  "</b><br/>" +
                      "CLIENTE: <b>" + STRING(CPago.Id-Cliente,"99999") + " " + CPago.RazonSocial + "</b><br/>" +
                      "FOLIO: <b>" + CPago.Id-CPago + "</b><br/>" +
                      "FECHA PAGO: <b>" + STRING(CPago.FecPag,"99/99/9999") + "</b><br/>" +
                      "FORMA PAGO: <b>" + CPago.FormaDePago + " </b><br/><br/>" +

                      "FECHA: <b>" + STRING(TODAY,"99/99/9999") + "</b><br/>" +
                      "HORA: <b>" + STRING(TIME,"hh:mm:ss") + "</b><br/>" +
                      "USUARIO: <b>" + STRING(idUsuario) + " " + Usuario.Nom-Usuario + "</b><br/><br/>" +
                      "<o:p></o:p></span></p>" +
                      '<meta http-equiv="Content-Type" content="text/html; charset=windows-1252">' +
                      "</head>".
                      
    IF AVAILABLE Usuario THEN
        ASSIGN
            l-Mail       = Usuario.e-Mail
            v-MailDe     = Usuario.e-mail + ";" + Usuario.Nom-Usuario
            v-ResponderA = Usuario.e-mail + ";" + Usuario.Nom-Usuario.
            
    ASSIGN 
        l-Mail = "flucio@adosa.com.mx;desarrollo10@adosa.com.mx".
            
    /* Asigna el mail de respuesta */
    IF v-ResponderA <> "" THEN
        ASSIGN
            v-MailDe = v-MailDe + "^" + v-ResponderA.
            
    /* Activa confirmacion de lectura */
    ASSIGN
        v-MailDe = v-MailDe + CHR(1) + "No,No".
    
     {programas/inva0007.i
                &Asunto     = "l-Asunto"
                &contenido  = "l-Contenido"
                &Iniciales  = "'SIS10'"
                &Direccion  = "l-Mail"
                &Refer      = "'DIRECTO'"
                &Attachment = ""
      }    
    
    /* Se quita porque tarda mucho en enviar el correo con Rest           
    RUN /usr2/adosa/procs/correo01.p (INPUT l-Mail,
        INPUT v-MailDe,
        INPUT "",
        INPUT "", /*nombre del archivo solamente */
        INPUT "", /*ruta completa del archivo */
        INPUT l-Asunto,
        INPUT l-Contenido,
        OUTPUT v-Enviado).
    */  
END   

