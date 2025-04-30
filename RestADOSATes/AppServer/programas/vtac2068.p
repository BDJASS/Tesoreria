/*
 Empresa : ADOSA
 Programa: vtac2068.p
 Funcion : Genera la factura electronica para globales de tickets (detallada)
 Autor   : FLC
 Fecha   : 22 ABR 2019
*/

 //{sia00000.var}
DEF INPUT PARAMETER numFactIni LIKE Factura.Id-Factura NO-UNDO.

DEF VAR tasa       AS DECIMAL NO-UNDO INITIAL 0.

/* Para manejo de conexion con web service */
DEF VAR hWS               AS HANDLE.
DEF VAR hEmiteCFDSoap     AS HANDLE.
DEF VAR vlconnect         AS LOGICAL NO-UNDO.
DEF VAR vldisconnect      AS LOGICAL NO-UNDO.
DEF VAR servidor          AS CHAR NO-UNDO.

/* Variables para funcion del web service */
DEF VAR v-respuesta   AS CHAR NO-UNDO.
DEF VAR v-comprobante AS LONGCHAR NO-UNDO.
DEF VAR v-UUID AS LONGCHAR NO-UNDO.

DEF VAR v-listaMes AS CHAR NO-UNDO 
        INITIAL 'ENE,FEB,MAR,ABR,MAY,JUN,JUL,AGO,SEP,OCT,NOV,DIC'.

/* Variables para guardar cada una de las etiquetas del xml */
DEF VAR v-emisor AS CHAR NO-UNDO.
DEF VAR v-domFis AS CHAR NO-UNDO.
DEF VAR v-expEn  AS CHAR NO-UNDO.
DEF VAR v-recep  AS CHAR NO-UNDO.
DEF VAR v-dom    AS CHAR NO-UNDO.
DEF VAR v-enca   AS CHAR NO-UNDO.
DEF VAR v-deta   AS LONGCHAR NO-UNDO.
DEF VAR v-concep AS LONGCHAR NO-UNDO.
DEF VAR v-impsto AS CHAR NO-UNDO.
DEF VAR v-puerto AS CHAR NO-UNDO.
DEF VAR v-pedNo  AS CHAR NO-UNDO.
DEF VAR v-addnd  AS CHAR NO-UNDO.
DEF VAR v-global AS CHAR NO-UNDO.

/* Parametros para generar el pdf */
DEF VAR v-rfc     AS CHAR     NO-UNDO.
DEF VAR v-serie   AS CHAR     NO-UNDO.
DEF VAR v-folio   AS CHAR     NO-UNDO.

DEF VAR v-fecVen  AS DATE     NO-UNDO.
DEF VAR v-letras  AS CHAR     NO-UNDO.
DEF VAR v-digi    AS CHAR     NO-UNDO.
DEF VAR v-exped   AS CHAR     NO-UNDO.
DEF VAR v-descrip AS CHAR     NO-UNDO.
DEF VAR v-razon   AS CHAR     NO-UNDO.
DEF VAR v-CalleNo AS CHAR     NO-UNDO.
DEF VAR v-rfcCte  AS CHAR     NO-UNDO.
DEF VAR v-reng    AS INT      NO-UNDO.
DEF VAR v-pedido  AS CHAR     NO-UNDO.
DEF VAR i         AS INT      NO-UNDO.
DEF VAR msuma     AS INT      NO-UNDO.
DEF VAR mfa_veri  AS INT      NO-UNDO.
DEF VAR l-k       AS INT      NO-UNDO.
DEF VAR v-fecAdu  AS CHAR     NO-UNDO.
DEF VAR l-IvaZona AS DECIMAL  NO-UNDO.
DEF VAR v-okRFC   AS LOGICAL  NO-UNDO.
DEF VAR l-MetodoDePago AS CHAR NO-UNDO.
DEF VAR l-NumCtaPago AS CHAR NO-UNDO.
DEF VAR l-LeyendaMetodo AS CHAR NO-UNDO.
DEF VAR l-ClavePS LIKE Articulo.Id-ClavePS NO-UNDO.
DEF VAR v-UsoCFDI LIKE Factura.Id-UsoCFDI NO-UNDO.
DEF VAR l-DetIVA  AS DECIMAL DECIMALS 6 NO-UNDO.
DEF VAR l-TDetIVA  AS DECIMAL DECIMALS 6 NO-UNDO.
DEF VAR v-correo  AS CHAR NO-UNDO.
DEF VAR l-Base     AS DECIMAL NO-UNDO.
DEF VAR l-Base0    AS DECIMAL NO-UNDO.
DEF VAR l-TBase    AS DECIMAL NO-UNDO.

DEF TEMP-TABLE v-pedim
    FIELD PedNo  LIKE DetPedim.PedNo
    FIELD Puerto LIKE DetPedim.Puerto
    FIELD FecPed LIKE DetPedim.FecPed
    INDEX ip-1 AS PRIMARY pedNo Puerto.
    
DEF TEMP-TABLE w-Impuestos
    FIELD Tasa AS DECIMAL
    FIELD Base AS DECIMAL
    FIELD Importe AS DECIMAL DECIMALS 6
    INDEX Idx-Def Tasa.

/* Variables para extraer los valores de la respuesta del WebService */
DEF VAR v-valores AS CHAR EXTENT 4 NO-UNDO.
DEF VAR v-tam     AS INT NO-UNDO.
DEF VAR v-ind     AS INT NO-UNDO.
DEF VAR v-pos     AS INT NO-UNDO.
DEF VAR v-siz     AS INT NO-UNDO.
DEF VAR v-num     AS INT NO-UNDO.
DEF VAR l-Intent  AS INT NO-UNDO.
DEF VAR l-MaxInt  AS INT NO-UNDO INITIAL 2.
DEF VAR l-FecVenta AS DATE NO-UNDO INITIAL ?.

DEF VAR l-NumTienda LIKE CliEmb.Id-CliEmb NO-UNDO.
DEF VAR l-CodCliente AS CHAR NO-UNDO.

DEF BUFFER buffFactura FOR Factura.

FIND FIRST SysGeneral NO-LOCK NO-ERROR.
ASSIGN l-IvaZona = SysGeneral.porc-iva.
FIND buffFactura WHERE buffFactura.Id-Factura = numFactIni NO-LOCK.
    
    /* Traer al buffer de lectura los registros necesarios que pertenecen a la factura */
    FIND Factura WHERE Factura.Id-Factura = numFactIni NO-LOCK NO-ERROR.
    FIND Transporte OF Factura NO-LOCK NO-ERROR.
    FIND Cliente OF Factura NO-LOCK NO-ERROR.
    FIND Entrega OF Factura NO-LOCK NO-ERROR.
    FIND CondVta OF Factura NO-LOCK NO-ERROR.
    FIND Vendedor OF Factura NO-LOCK NO-ERROR.
    FIND Zona OF Cliente NO-LOCK NO-ERROR.

    /* No permitir generar el folio electronico 2 veces */
    IF Factura.Folioe <> '' THEN RETURN.
    IF Factura.Id-Cliente > 10 OR Factura.Id-Cliente = 3 OR Factura.Id-Cliente = 1 THEN NEXT.
    
    
    IF AVAILABLE Vendedor THEN
        FIND Empleado WHERE Empleado.iniciales = Vendedor.iniciales NO-LOCK NO-ERROR.
    
    /* 1. Datos de Facturacion del Cliente o Receptor */
/*
    v-razon = REPLACE(Factura.RazonSocial,'&','&amp;').
    v-razon = REPLACE(v-razon,'"', '&quot;').
    v-razon = TRIM(REPLACE(v-razon,'|',' ')).
    v-razon = REPLACE(v-razon,"'",'&apos;').
    v-razon = REPLACE(v-razon,'<','&lt;').
    v-razon = REPLACE(v-razon,'>','&gt;').
    RUN Colapsa(INPUT-OUTPUT v-razon).
*/
    v-razon = "PUBLICO EN GENERAL.".
    v-rfcCte = CAPS(TRIM(REPLACE(Factura.rfc,' ',''))).        
    v-rfcCte = REPLACE(v-rfcCte,'&','&amp;').
    
    /* Corregir caracter especial en la direccion */
    v-CalleNo = REPLACE(Factura.CalleNo,'&','&amp;').
    v-CalleNo = REPLACE(v-CalleNo,'"', '&quot;').
    v-CalleNo = TRIM(REPLACE(v-CalleNo,'|',' ')).
    v-CalleNo = REPLACE(v-CalleNo,"'",'&apos;').
    v-CalleNo = REPLACE(v-CalleNo,'<','&lt;').
    v-CalleNo = REPLACE(v-CalleNo,'>','&gt;').

    IF Factura.BuzonFiscal <> "" THEN 
       v-correo = Factura.BuzonFiscal.
    ELSE v-correo = "".
    v-correo = REPLACE(v-correo,'&','&amp;').
 
    v-UsoCFDI = "S01".
    
    v-recep = '<Receptor rfc="' + v-rfcCte + '" UsoCFDI="' + TRIM(v-usocfdi) + '" nombre="' + v-razon + '" RegimenFiscalReceptor="616" DomicilioFiscalReceptor="64000">'.
    v-enca = '<ImprimeEncabezado DecimalesXml="2" CodigoBarra="' + Factura.Id-Factura + '" RazonSocial="' + v-razon + '" RFC="R.F.C. ' + v-rfcCte + '" '.
    IF v-correo <> "" THEN v-enca = v-enca + 'correo="' + v-correo + '" '.
    
    v-dom = '<Domicilio calle="' + TRIM(v-CalleNo) + '" noExterior="' + (IF AVAILABLE Cliente AND Cliente.NumExt <> '' THEN Cliente.NumExt ELSE '') + '" noInterior="" colonia="' + TRIM(Factura.Colonia) + '" '.
    v-enca = v-enca + 'CalleNo="' + TRIM(v-CalleNo) + '" Colonia="COL. ' + TRIM(Factura.Colonia) + '" '.
    v-dom = v-dom + 'localidad="" '.
    v-enca = v-enca + 'Delegacion="" '. 
    
    RUN LlenaDomicilio(INPUT-OUTPUT v-enca, INPUT Factura.Id-Ciudad, INPUT 'Ciudad', INPUT 'Estado', INPUT 'Pais').
    RUN LlenaDomicilio(INPUT-OUTPUT v-dom, INPUT Factura.Id-Ciudad, INPUT 'municipio', INPUT 'estado', INPUT 'pais').
    
    v-dom = v-dom + 'codigoPostal="' + Factura.cp + '" />'.
    v-enca = v-enca + 'CP="' + Factura.cp + '" '.
    
    RUN Colapsa(INPUT-OUTPUT v-dom).
    
    v-enca = v-enca + 'EmbAtencion="" EmbRazonSocial="" EmbCalleNo="" EmbColonia="" EmbDelegacion="" EmbCiudad="" EmbEstado="" EmbPais="" EmbCP="" EmbTel="" EmbFax="" '.
    
    IF AVAILABLE Transporte THEN
        v-enca = v-enca + 'Transporte="E' + (IF AVAILABLE Cliente THEN STRING(Factura.Id-RutaEmb,"99") ELSE '') + ' ' + 
                          STRING(Transporte.Id-Tran) + ' ' + Transporte.Nombre + '" '.
    ELSE
        v-enca = v-enca + 'Transporte="E' + (IF AVAILABLE Cliente THEN STRING(Factura.Id-RutaEmb,"99") ELSE '') + '" '.
        
    /* 2. Datos del Credito y Totales */
    v-comprobante = '<?xml version="1.0" encoding="utf-8"?><Comprobante xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" version="4.0" '.
    
    v-digi = SUBSTRING(Factura.Id-Factura,1,1).
    
    /*servidor = "-WSDL http://192.0.1.10:8087/wseDocEmiteCFD/wsEmiteCFD.asmx?wsdl".*/
    servidor = "-WSDL http://192.0.1.7/wseDocEmiteCFD/wsEmiteCFD.asmx?wsdl".
    
    CASE v-digi:
        WHEN '1' THEN DO:
            IF Factura.Id-Ubic BEGINS '12' THEN DO:
                v-comprobante = v-comprobante + ' serie="1" '.
                servidor = "-WSDL http://192.0.5.22/wseDocEmiteCFD/wsEmiteCFD.asmx?wsdl".
            END.
            ELSE
                v-comprobante = v-comprobante + ' serie="1" '.
            
            v-comprobante = v-comprobante + ' LugarExpedicion="88300" '.
            v-exped = "Expedida en Miguel Aleman, Tamps.".
            v-expEn = '<ExpedidoEn calle="Segunda Sur" noExterior="102" noInterior="" colonia="Centro" localidad="" referencia="" municipio="Miguel Aleman" estado="Tamaulipas" pais="Mexico" codigoPostal="88300"/>'.
            END.
        WHEN '2' THEN DO:
            v-comprobante = v-comprobante + ' serie="2" '.
            v-comprobante = v-comprobante + ' LugarExpedicion="64000" '.
            v-exped = "Expedida en Monterrey, N.L.".
            v-expEn = '<ExpedidoEn calle="Zaragoza Norte" noExterior="435" noInterior="" colonia="Monterrey Centro" localidad="" referencia="" municipio="Monterrey" estado="Nuevo Leon" pais="Mexico" codigoPostal="64000"/>'.
            END.
        WHEN '3' THEN DO:
            v-comprobante = v-comprobante + ' serie="3" '.
            v-comprobante = v-comprobante + ' LugarExpedicion="64000" '.
            v-exped = "Expedida en Monterrey, N.L.".
            v-expEn = '<ExpedidoEn calle="Zaragoza Norte" noExterior="435" noInterior="" colonia="Monterrey Centro" localidad="" referencia="" municipio="Monterrey" estado="Nuevo Leon" pais="Mexico" codigoPostal="64000"/>'.
            END.
        WHEN '4' THEN DO:
            v-comprobante = v-comprobante + ' serie="4" '.
            v-comprobante = v-comprobante + ' LugarExpedicion="25000" '.
            v-exped = "Expedida en Saltillo, Coah.".
            v-expEn = '<ExpedidoEn calle="Hidalgo" noExterior="1126" noInterior="" colonia="Centro" localidad="" referencia="" municipio="Saltillo" estado="Coahuila" pais="Mexico" codigoPostal="25000"/>'.
            IF Factura.FecReg < 10/31/2016 THEN
               servidor = "-WSDL http://192.0.2.22/wseDocEmiteCFD/wsEmiteCFD.asmx?wsdl".
            END.
        WHEN '5' THEN DO: 
            v-comprobante = v-comprobante + ' serie="5" '.
            v-comprobante = v-comprobante + ' LugarExpedicion="31000" '.
            v-exped = "Expedida en Chihuahua, Chih.". 
            v-expEn = '<ExpedidoEn calle="Julian Carrillo" noExterior="806" noInterior="" colonia="Centro" localidad="" referencia="" municipio="Chihuahua" estado="Chihuahua" pais="Mexico" codigoPostal="31000"/>'.
            servidor = "-WSDL http://192.0.5.22/wseDocEmiteCFD/wsEmiteCFD.asmx?wsdl".
            END.
        WHEN '6' THEN DO:
            v-comprobante = v-comprobante + ' serie="6" '.
            v-comprobante = v-comprobante + ' LugarExpedicion="67176" '.
            v-exped = "Expedida en Guadalupe, N.L.".
            v-expEn = '<ExpedidoEn calle="Av. Pablo Livas" noExterior="2500" noInterior="" colonia="Local 13 Plaza Mirador Mirador de la Silla" localidad="" referencia="" municipio="Guadalupe" estado="Nuevo Leon" pais="Mexico" codigoPostal="67176"/>'.
            END.            
        WHEN '7' THEN DO:
            v-comprobante = v-comprobante + ' serie="7" '.
            v-comprobante = v-comprobante + ' LugarExpedicion="64540" '.
            v-exped = "Expedida en Monterrey, N.L.".
            v-expEn = '<ExpedidoEn calle="Av. Ruiz Cortines" noExterior="3280" noInterior="1" colonia="Parque Industrial Regiomontano" localidad="" referencia="" municipio="Monterrey" estado="Nuevo Leon" pais="Mexico" codigoPostal="64540"/>'.
            END.            
        WHEN '8' THEN DO:
            v-comprobante = v-comprobante + ' serie="8" '.
            v-comprobante = v-comprobante + ' LugarExpedicion="64348" '.
            v-exped = "Expedida en Monterrey, N.L.".
            v-expEn = '<ExpedidoEn calle="Ruiz Cortines" noExterior="6410" noInterior="" colonia="Fracc. Portal de Cumbres" localidad="" referencia="" municipio="Monterrey" estado="Nuevo Leon" pais="Mexico" codigoPostal="64348"/>'.
            END.              
        WHEN '9' THEN DO:
            v-comprobante = v-comprobante + ' serie="9" '.
            v-comprobante = v-comprobante + ' LugarExpedicion="66468" '.
            v-exped = "Expedida en San Nicolas de los Garza, N.L.".
            v-expEn = '<ExpedidoEn calle="Diego Diaz de Berlanga" noExterior="469" noInterior="" colonia="Jardines de Santo Domingo" localidad="" referencia="" municipio="San Nicolas de los Garza" estado="Nuevo Leon" pais="Mexico" codigoPostal="66468"/>'.
            END.              
        WHEN 'N' THEN DO:
            v-comprobante = v-comprobante + ' serie="N" '.
            v-comprobante = v-comprobante + ' LugarExpedicion="66059" '.
            v-exped = "Expedida en Escobedo, N.L.".
            v-expEn = '<ExpedidoEn calle="Concordia Ote." noExterior="100" noInterior="L28E" colonia="Cerradas de Anahuac" localidad="" referencia="" municipio="Escobedo" estado="Nuevo Leon" pais="Mexico" codigoPostal="66059"/>'.
            END.              
        OTHERWISE DO:
            v-comprobante = v-comprobante + ' serie="0" '.
            v-comprobante = v-comprobante + ' LugarExpedicion="64000" '.
            v-exped = "Expedida en Monterrey, N.L.".
            v-expEn = '<ExpedidoEn calle="Zaragoza Norte" noExterior="435" noInterior="" colonia="Monterrey Centro" localidad="" referencia="" municipio="Monterrey" estado="Nuevo Leon" pais="Mexico" codigoPostal="64000"/>'.
            END.
    END CASE.

    /* 3. Datos de Renglones de la Factura */
    
    OUTPUT TO VALUE("/tmp/glo" + Factura.Id-Factura + ".txt").
    v-reng = 1.
    l-TBase = 0.
    l-TDetIva = 0.
    FOR EACH Remision WHERE Remision.FacGlobal= Factura.Id-Factura NO-LOCK:
        IF Remision.Subtotal = 0 AND Remision.IVA = 0 AND Remision.Tot = 0 THEN NEXT.
        IF Remision.Tot <= 0.02 THEN NEXT.
        IF l-FecVenta = ? THEN l-FecVenta = Remision.FecReg.
        v-descrip = "VENTA".
        v-deta = v-deta + '<ImprimeDetalle secuencia="' + STRING(v-reng) + '" tipoLinea="1" descripcion="' + TRIM(v-descrip) + '" '.
        v-deta = v-deta + 'unidad="PIEZA" '.
        v-deta = v-deta + 'porcIVA="" tipoPrecio="" '.
        v-deta = v-deta + 'noIdentificacion="' + Remision.Id-Remision + '" cantidad="1" '.
        v-deta = v-deta + 'valorUnitario="' + STRING(Remision.Tot - Remision.Iva,"->>,>>>,>>9.99") + '" importe="' + STRING(Remision.Tot - Remision.Iva,"->>,>>>,>>9.99") + '" comentario="" '.
        v-deta = v-deta + 'descuento=""'.
        v-deta = v-deta + '/>'.
        v-reng = v-reng + 1. 

        v-concep = v-concep + '<Concepto cantidad="1" '. 
        v-concep = v-concep + 'unidad="" ClaveUnidad="ACT" ClaveProdServ="01010101" '.
        v-concep = v-concep + 'noIdentificacion="' + Remision.Id-Remision + '" descripcion="VENTA" '.
        v-concep = v-concep + 'valorUnitario="' + TRIM(STRING(Remision.Tot - Remision.Iva,"zzzzzzzz9.99")) + '" importe="' + TRIM(STRING(Remision.Tot - Remision.Iva,"zzzzzzzz9.99")) + '">'.
        l-Base = 0.
        l-Base0 = 0.
        l-DetIva = 0.
        IF Remision.IVA > 0 THEN DO:
            l-Base   = ROUND(Remision.Iva / (l-IvaZona / 100),2).
            IF l-Base > (Remision.Tot - Remision.Iva) THEN 
               l-Base = Remision.Tot - Remision.Iva.
            l-DetIva = ROUND(l-Base * (l-IvaZona / 100),6).
            /*l-DetIva = l-Base * Remision.IVA. */
            ASSIGN v-concep = v-concep + '<Impuestos><Traslados>'.
            ASSIGN v-concep = v-concep + '<Traslado Base="' + TRIM(STRING(l-Base,"zzzzzzzzzzz9.99")) + '" Impuesto="002" TipoFactor="Tasa" TasaOCuota="' +
                   STRING((l-IvaZona / 100),"9.999999") + '" Importe="' + TRIM(STRING(l-DetIVA,"zzzzzzzz9.999999")) + '"/>'.
            l-TBase = l-TBase + l-Base.
            l-TDetIva = l-TDetIva + l-DetIva.

            FIND w-Impuestos WHERE w-Impuestos.Tasa = (l-IvaZona / 100) EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAILABLE w-Impuestos THEN DO:
               CREATE w-Impuestos.
               ASSIGN w-Impuestos.Tasa = (l-IvaZona / 100).
            END.
            ASSIGN w-Impuestos.Base = w-Impuestos.Base + l-Base
                   w-Impuestos.Importe = w-Impuestos.Importe + l-DetIva.
            RELEASE w-Impuestos.

            IF l-Base < (Remision.Tot - Remision.Iva) THEN DO:
               FIND FIRST DetRemis 
                    WHERE DetRemis.Id-Remision = Remision.Id-Remision
                      AND DetRemis.Tipo = 1
                      AND DetRemis.PorcIva = 0
                      NO-LOCK NO-ERROR.
               IF AVAILABLE DetRemis THEN DO:
                  l-Base0 = (Remision.Tot - Remision.Iva) - l-Base.
                  ASSIGN v-concep = v-concep + '<Traslado Base="' + TRIM(STRING(l-Base0,"zzzzzzzzzzz9.99")) + '" Impuesto="002" TipoFactor="Exento"/>'.
                  FIND w-Impuestos WHERE w-Impuestos.Tasa = 0  
                       EXCLUSIVE-LOCK NO-ERROR.
                  IF NOT AVAILABLE w-Impuestos THEN DO:
                     CREATE w-Impuestos.
                     ASSIGN w-Impuestos.Tasa = 0.
                  END.
                  ASSIGN w-Impuestos.Base = w-Impuestos.Base + l-Base0.
                  RELEASE w-Impuestos.
               END.
            END.
            ASSIGN v-concep = v-concep + '</Traslados></Impuestos>'.
        END.
        ELSE DO:
            l-Base0 = Remision.Tot - Remision.Iva.
            ASSIGN v-concep = v-concep + '<Impuestos><Traslados><Traslado Base="' + TRIM(STRING(l-Base0,"zzzzzzzzzzz9.99")) + 
                   '" Impuesto="002" TipoFactor="Exento"' +
                   '/></Traslados></Impuestos>'.
            FIND w-Impuestos WHERE w-Impuestos.Tasa = 0 EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAILABLE w-Impuestos THEN DO:
               CREATE w-Impuestos.
               ASSIGN w-Impuestos.Tasa = 0.
            END.
            ASSIGN w-Impuestos.Base = w-Impuestos.Base + l-Base0.
            RELEASE w-Impuestos.
        END.
        v-concep = v-concep + '</Concepto>'.
        EXPORT Remision.Id-Remision 
               (Remision.Tot - Remision.Iva)
               l-Base l-Base0 l-DetIva.
        
        
    END.
    OUTPUT CLOSE.
    
    /*Cambio Junio 2011: la forma de pago debe ser PAGO EN UNA SOLA EXHIBICION*/
    /*Cambio Nov 2017: la forma de pago debe ser 99 En Credito*/
    v-comprobante = v-comprobante + 'formaDePago="01" '.
    
    
    /* 2.2 Datos del plazo */
    IF Factura.Id-Cond <> 0 THEN DO:
        
        IF Factura.Id-Cond = 2 AND Factura.Plazo > 30 THEN
            ASSIGN v-fecVen = Factura.FecReg + 30.
        ELSE
            ASSIGN v-fecVen = Factura.FecReg + Factura.Plazo.
            
        v-enca = v-enca + 'Vencimiento="' + STRING(DAY(v-fecVen),"99") + '-' + ENTRY(MONTH(v-fecVen),v-listaMes) + '-' + STRING(YEAR(v-fecVen),">>>9") + '" '.        
        
    END.
    ELSE 
        v-enca = v-enca + 'Vencimiento="" '.
    
    
    RUN vtad1000.p(INPUT Factura.Id-Cliente, OUTPUT mfa_veri).
        
    v-enca = v-enca + 'Autorizacion="' + Factura.Id-Captura + ' ' + (IF Factura.CveAut > 0 THEN 'AUT-' + STRING(Factura.Autorizado-Por,"x(7)") ELSE '') + '" '.
    v-enca = v-enca + 'Pedido="' + SUBSTRING(Factura.Pedidos,1,25) + '" '.
    v-enca = v-enca + 'Requisicion="' + SUBSTRING(Factura.Requisicion,1,20) + '" '.
    v-enca = v-enca + 'cliente="' + STRING(Factura.Id-Cliente) + "-" + STRING(mfa_veri,'99') + '" '.
    v-enca = v-enca + 'Fecha="' + STRING(DAY(Factura.FecReg),">9") + '-' + ENTRY(MONTH(Factura.FecReg),v-listaMes) + '-' + STRING(YEAR(Factura.FecReg),">>>9") + '" '.
    v-enca = v-enca + 'Id_Factura="' + (IF v-digi > '0' THEN STRING(v-digi) + '-' + SUBSTRING(Factura.Id-Factura,2,6) ELSE Factura.Id-Factura) + '" '.
    v-enca = v-enca + 'Propietario="' + (IF LENGTH(TRIM(Factura.Propietario)) > 0 AND Cliente.Tipo = 1 THEN STRING(Factura.Propietario, "x(50)") ELSE '') + '" '.
    v-enca = v-enca + 'Tarimas="' + STRING(Factura.Tarimas,"zz9") + '" '.
    v-enca = v-enca + 'Bultos="' + STRING(Factura.Bultos,"zz9") + '" '.
    v-enca = v-enca + 'Entrega="' + STRING(IF AVAILABLE Entrega THEN Entrega.Descr ELSE '',"x(14)") + '" '.
    v-enca = v-enca + 'Vendedor="' + STRING(Factura.Id-Vendedor) + ' ' + (IF AVAILABLE Empleado THEN STRING(Empleado.Nombre, "x(14)") ELSE '') + '" '.
    v-enca = v-enca + 'Zona="' + (IF AVAILABLE Cliente THEN STRING(Cliente.Id-Zona) ELSE '') + '" '.
    v-enca = v-enca + 'Cobrador="' + (IF AVAILABLE Cliente THEN STRING(Cliente.Id-Cobrador) ELSE '') + '" '.
    
    /* 2.3 Datos de totales de la factura */
    ASSIGN l-MetodoDePago = "PUE"
           l-NumCtaPago   = "".


           /*
    IF Factura.FeFormaPago <> "" THEN DO:
       ASSIGN l-MetodoDePago = Factura.FeFormaPago
              l-NumCtaPago   = Factura.FeDigitosCuenta.
    END.
    ELSE IF AVAILABLE Cliente AND Cliente.FEFormaPago > "" THEN DO:
            ASSIGN l-MetodoDePago = TRIM(Cliente.FEFormaPago).
            IF Cliente.FEDigitosCuenta > "" THEN
               ASSIGN l-NumCtaPago   = TRIM(Cliente.FEDigitosCuenta).
    END.
    */
    l-LeyendaMetodo =  "METODO DE PAGO: " + l-MetodoDePago.
    v-enca = v-enca + 'RutaEmbarque="' + TRIM(l-LeyendaMetodo) + '" '.

    v-comprobante = v-comprobante + 'subTotal="' + STRING(Factura.SubTotal) + '" motivoDescuento="" '.
    v-comprobante = v-comprobante + 'total="' + TRIM(STRING(Factura.SubTotal + l-TDetIva,"zzzzzzzz9.99")) + '" Moneda="MXN" TipoCambio="1" metodoDePago="' + l-MetodoDePago + '" '.
    IF l-NumCtaPago <> '' THEN
       v-comprobante = v-comprobante + 'NumCtaPago="' + l-NumCtaPago + '" '.
    v-comprobante = v-comprobante + 'tipoDeComprobante="ingreso" ReferenciaCFD="' + Factura.Id-Factura + '" xmlns="http://www.sat.gob.mx/cfd/2">'.
    
    v-enca = v-enca + 'SubTotal="" Descuento="" ValorNeto="' + STRING(Factura.SubTotal, "$>>,>>>,>>9.99") + '" IVA="' + STRING(l-TDetIva,"$>>,>>>,>>9.99") + '" '.
    v-enca = v-enca + 'FleteEtiqueta="Maniobras" FleteValor="0" Total="' + STRING(Factura.SubTotal + l-TDetIva,"$>>,>>>,>>9.99") + '" '.
    
    v-letras = ''.
    RUN rtn0005.p(INPUT (Factura.SubTotal + l-TDetIva), OUTPUT v-letras).
    
    v-enca = v-enca + 'DiaPago="" CantLetras="' + v-letras + '" Interes="' + STRING(tasa) + '" Expedida="' + v-exped + '"'.
    v-enca = v-enca + '/>'.
    
    
    
    /* 3.5 Impuestos */
    v-impsto = "".
    IF Factura.IVA > 0 THEN DO:
        /* ************************************************************************************************************************************* */
        /* Cuidar que la tasa este correcta */
        /*v-impsto = '<Impuestos totalImpuestosTrasladados="' + STRING(Factura.iva) + '" ><Traslados>'.*/
        v-impsto = '<Impuestos totalImpuestosTrasladados="' + TRIM(STRING(l-TDetIva,"zzzzzzzz9.99")) + '" ><Traslados>'.
        FOR EACH w-Impuestos WHERE w-Impuestos.Base > 0 NO-LOCK:
            IF w-Impuestos.Importe > 0 THEN
               v-impsto = v-impsto + '<Traslado impuesto="IVA" TipoFactor="Tasa" tasa="' + STRING(w-Impuestos.Tasa * 100,"99") + '" importe="' + TRIM(STRING(ROUND(w-Impuestos.Importe,2),"zzzzzzzz9.99")) + '" Base="' + TRIM(STRING(ROUND(w-Impuestos.Base,2),"zzzzzzzzz9.99")) + '"/>'.
            ELSE v-impsto = v-impsto + '<Traslado impuesto="IVA" TipoFactor="Exento" tasa="0" importe="0" Base="' + TRIM(STRING(ROUND(w-Impuestos.Base,2),"zzzzzzzzz9.99")) + '"/>'.
        END.
        v-impsto = v-impsto + '</Traslados></Impuestos>'.
    END.
    ELSE v-impsto = v-impsto + '<Impuestos/>'.
    
    
    /* 4. Unir partes de la Factura */ /* Pruebas: AAA010101AAA Prod: AOF870529IU7 */
    v-comprobante = v-comprobante + '<Emisor rfc="AOF870529IU7" nombre="ABASTECEDORA DE OFICINAS">'.
    v-comprobante = v-comprobante + '<DomicilioFiscal calle="Zaragoza Norte" noExterior="435" noInterior="" colonia="Monterrey Centro" localidad="" referencia="" '.
    v-comprobante = v-comprobante + 'municipio="Monterrey" estado="Nuevo Le�n" pais="M�xico" codigoPostal="64000"/>'.
    v-comprobante = v-comprobante + v-expEn + ' <RegimenFiscal Regimen="601"/></Emisor>'.
    
    v-comprobante = v-comprobante + v-enca + v-deta + v-recep + v-dom + '</Receptor>'.
    v-comprobante = v-comprobante + '<Conceptos>' + v-concep + '</Conceptos>' + v-impsto.

    v-global = '<InformacionGlobal Periodicidad="01" Meses="' + STRING(MONTH(l-FecVenta),'99') + '" Ano="' + STRING(YEAR(l-FecVenta),'9999') + '"></InformacionGlobal>'.
    /*v-global = REPLACE(v-global,CHR(164),'&#241;').*/

    v-comprobante = v-comprobante + v-global + '</Comprobante>'.

    OUTPUT TO VALUE("/usr2/compartido/request/" + Factura.Id-Factura + ".xml").
    EXPORT v-comprobante.
    OUTPUT CLOSE.
    
    /* Realizar llamadas y operaciones con el webService de eDoc */
    
    CREATE SERVER hWS.
    vlconnect = hWS:CONNECT(servidor) NO-ERROR.
        
    IF vlconnect THEN DO:
        RUN wsEmiteCFDSoap SET hEmiteCFDSoap ON hws.
        /*
        OUTPUT TO '/usr3/tmp/factaut.txt' APPEND.
            EXPORT
                "        "
                "ENTRA AL IntentaCFD"
                SKIP.            
        OUTPUT CLOSE.
        */
        RUN IntentaCFD.
        vldisconnect = hWS:DISCONNECT().
    END.
    ELSE
        MESSAGE "No se pudo conectar al Web Service".
        
    PAUSE 2 NO-MESSAGE.
    
RETURN.


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
    
    DEF VAR v-cityName AS CHAR NO-UNDO.
    
    FIND Ciudad WHERE Ciudad.Id-Ciudad = v-city NO-LOCK NO-ERROR.
    
    IF AVAILABLE Ciudad THEN DO:
        
        IF CAN-DO('2400,3332,970,3381,3383,330,3201,1773,172,4064,3537,33717,29347,34290,34392',STRING(Factura.Id-Cliente)) THEN DO:
            CASE v-city:
                WHEN 77 THEN
                    v-cityName = 'Garc�a'.
                WHEN 78 THEN
                    v-cityName = 'San Pedro Garza Garc�a'.
                OTHERWISE
                    v-cityName = TRIM(Ciudad.Nombre).
            END.
        END.
        ELSE 
            v-cityName = TRIM(Ciudad.Nombre).
                        
        FIND Estado WHERE Estado.Id-Estado = Ciudad.Id-Estado NO-LOCK NO-ERROR.
        
        IF AVAILABLE Estado THEN DO:
        
            FIND Pais WHERE Pais.Id-Pais = Estado.Id-Pais NO-LOCK NO-ERROR.
            
            IF AVAILABLE Pais THEN DO:
                
                v-dom = v-dom + v-lab1 + '="' + v-cityName + '" '.
                IF CAN-DO('2400,3332,970,3381,3383,31131,330,3201,1773,172,4064,3537,33717,29347,34290,34392,34416',STRING(Factura.Id-Cliente)) THEN DO:
                    v-dom = v-dom + v-lab2 + '="' + (IF Estado.Id-Estado = '019' THEN 'Nuevo Le�n' ELSE TRIM(Estado.Nombre)) + '" '.
                    v-dom = v-dom + v-lab3 + '="' + (IF Pais.Id-Pais = '001' THEN 'M�xico' ELSE TRIM(Pais.Nombre)) + '" '.
                END.
                ELSE
                    ASSIGN 
                        v-dom = v-dom + v-lab2 + '="' + TRIM(Estado.Nombre) + '" '
                        v-dom = v-dom + v-lab3 + '="' + TRIM(Pais.Nombre) + '" '.                    
            END.
        
        END.
    
    END.
    
END.

PROCEDURE IntentaCFD.
   l-Intent = 1.
   DO WHILE l-Intent <= l-MaxInt:
      RUN EmiteCFD IN hEmiteCFDSoap(INPUT v-comprobante, OUTPUT v-respuesta).
      RUN RecuperaUUID IN hEmiteCFDSoap(INPUT Factura.Id-Factura, OUTPUT v-UUID).

      /*MESSAGE "Folio Generado: " + v-respuesta.
      PAUSE 2 NO-MESSAGE.*/
      
      /*
      OUTPUT TO '/usr3/tmp/factaut.txt' APPEND.
          EXPORT
              "            "
              "INTENTO"
              STRING(l-Intent)
              "FOLIO GENERADO"
              STRING(v-Respuesta)
              "COMPROBANTE"
              SKIP.            
      OUTPUT CLOSE.
      */
      
      /* Generar el archivo PDF para poder imprimir la factura */
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
      
      IF v-valores[1] <> "ERROR" AND v-valores[1] BEGINS 'A' THEN DO: 
      
    	  v-rfc = v-valores[2].
    	  v-serie = v-valores[3].
    	  v-folio = v-valores[4].
    	  /*
    	  IF (Factura.Id-Entrega >= 12 AND Factura.Id-Entrega <= 19) AND
    	      Factura.Id-Entrega <> 16 AND (PROGRAM-NAME(2) MATCHES "*vtaa05*" OR 
    					    PROGRAM-NAME(2) MATCHES "*vtaa04*" OR
    					    PROGRAM-NAME(2) MATCHES "*vtad0601*" ) AND
    	      PROGRAM-NAME(2) <> 'vtaa0550' THEN 
    		  RUN vtac0301.p(INPUT Factura.Id-Factura, 6,"").
    	  */
    	  DO TRANSACTION:
    	      FIND Factura WHERE RECID(Factura) = RECID(buffFactura) EXCLUSIVE-LOCK.
    	      ASSIGN
    	          Factura.Folioe = v-valores[1] + ',' + v-serie + ',' + v-folio
    	          Factura.Id-Fiscal = TRIM(v-serie) + TRIM(v-folio)
                  Factura.FeMetodoPago = l-MetodoDePago
                  Factura.FeFormaPago = "01"
                  Factura.FeDigitosCuenta = l-NumCtaPago
                  Factura.Version = "4.0"
                  Factura.RSocietario = 
          TRIM(STRING(ROUND(Factura.SubTotal + l-TDetIva,2),"zzzzzzzz9.99")).
                  Factura.UUID = SUBSTRING(STRING(v-UUID),1,INDEX(STRING(v-UUID),",") - 1).
    	  END.
          RELEASE Factura.
    	  LEAVE.
      END.
      ELSE DO:
          IF l-Intent < l-MaxInt THEN l-Intent = l-Intent + 1.
          ELSE DO:
    	     OUTPUT TO VALUE("/usr2/compartido/request/" + Factura.Id-Factura + ".err").
    	     EXPORT v-respuesta.
    	     OUTPUT CLOSE.
    	     MESSAGE "Ocurrio un error al generar la factura electronica".
             LEAVE.
          END.
      END.
      PAUSE 2 NO-MESSAGE.
   END.
END.

