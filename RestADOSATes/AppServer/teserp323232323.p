@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*------------------------------------------------------------------------
    File        : teserp001.p
    Purpose     : V2

    Syntax      :/TesDashboard

    Description : Programa Dashboard HU01  

    Author(s)   : sis10
    Created     : Thu Apr 03 12:32:24 CST 2025
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.   

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEF    VAR      l-fecvence  AS DATE.
DEF    VAR      l-ubic      AS CHAR .     
DEF    VAR      l-recmov    AS RECID.
DEFINE VAR      l-folio     LIKE Factura.id-factura.   
DEFINE VAR      l-descr     AS CHARACTER FORMAT 'X(10)'.
DEFINE VAR      l-opcion    AS CHAR      EXTENT 3 INITIAL ['TICKET','REMISION','ELECTRONICA'] FORMAT 'X(11)'.
DEFINE VAR      l-subtotal  AS DECIMAL   DECIMALS 2 FORMAT 'Z,ZZZ,ZZ9.99-'.
DEFINE VAR      l-ivaa      AS DECIMAL   DECIMALS 2 FORMAT 'Z,ZZZ,ZZ9.99-'.
DEFINE VAR      l-tot       AS DECIMAL   DECIMALS 2 FORMAT 'Z,ZZZ,ZZ9.99-'.
DEFINE VAR      l-redo      AS DECIMAL   DECIMALS 2 FORMAT 'Z,ZZZ,ZZ9.99-'.
DEFINE VAR      l-archredo  AS CHAR.
DEFINE VAR      l-impredo   AS LOGICAL. 
DEFINE VAR      l-descuento AS DECIMAL   DECIMALS 2.
DEFINE VAR      l-neto      AS DECIMAL   DECIMALS 2 FORMAT 'Z,ZZZ,ZZ9.99-'.
DEFINE VAR      l-reng      AS INTEGER.
DEFINE VAR      l-cont      AS INTEGER.       
DEFINE VAR      l-factura   AS CHAR.
DEFINE VAR      l-concepto  AS CHAR.
DEFINE VAR      l-vendedor  LIKE Vendedor.Id-Vendedor.
DEFINE VAR      l-iniciales LIKE Vendedor.Iniciales.
DEF    VAR      l-ListSuc   AS CHAR      NO-UNDO.

DEF    VAR      l-Part1     AS DECIMAL   NO-UNDO.
DEF    VAR      l-Part0     AS DECIMAL   NO-UNDO.
   
DEFINE VARIABLE l-Monto     LIKE DepBanco.Importe NO-UNDO INITIAL 0.  // RNPC - 2019-12-27


DEFINE VAR      l-teclas    AS CHAR      FORMAT "x(20)"
    INITIAL "F1,RETURN,ENTER,GO,TAB,CURSOR-RIGHT,CURSOR-LEFT,CURSOR-DOWN,CURSOR-UP"
    NO-UNDO.
DEFINE BUFFER b-Mov      FOR MovCaja.
DEFINE BUFFER b-Remision FOR Remision.
DEFINE BUFFER b-MovCaja  FOR MovCaja.



DEFINE TEMP-TABLE wIva
    FIELD Iva  LIKE Factura.Iva
    FIELD Porc LIKE DistIva.PorcIva.
  



DEFINE TEMP-TABLE ttfact 
    FIELD id         AS CHAR
    FIELD tipo       AS INTEGER
    FIELD nomcte     LIKE cliente.razonsocial
    FIELD subtotal   AS DECIMAL
    FIELD iva        AS DECIMAL
    FIELD tot        AS DECIMAL
    FIELD selec      AS LOGICAL 
    FIELD Descr      LIKE TFact.Descr
    FIELD Documento  LIKE TFact.Documento
    FIELD Flag       LIKE TFact.Flag
    FIELD IdCliente  LIKE TFact.Id-Cliente
    FIELD IDmc       LIKE TFact.Id-MC
    FIELD IdSuplente LIKE TFact.Id-Suplente
    FIELD tty        LIKE TFact.Tty
    INDEX ip-1 AS PRIMARY documento.

DEFINE TEMP-TABLE wMov
    FIELD id        AS CHAR
    FIELD idcaja    LIKE MovCaja.Id-Caja
    FIELD turno     AS INTEGER
    FIELD concepto  AS CHAR    FORMAT 'X(18)' COLUMN-LABEL 'Concepto'
    FIELD ImpVentas AS DECIMAL FORMAT 'ZZ,ZZZ,ZZ9.99'
    FIELD ImpNCred  AS DECIMAL FORMAT 'ZZ,ZZZ,ZZ9.99'
    FIELD Total     LIKE l-neto
    FIELD iva       AS DECIMAL FORMAT 'ZZ,ZZZ,ZZ9.99'
    FIELD Donativo  AS DECIMAL FORMAT 'ZZ,ZZZ,ZZ9.99'
    FIELD manual    AS LOGICAL
    FIELD selec     AS LOGICAL FORMAT 'Si/No'.

DEFINE TEMP-TABLE tttotal
    FIELD id     AS CHAR
    FIELD Subtot LIKE l-subtotal
    FIELD Iva    LIKE l-ivaa
    FIELD Tot    LIKE l-tot
    FIELD Don    LIKE l-redo .


DEFINE DATASET dsFactura FOR wMov ,tttotal,ttfact 
    DATA-RELATION dsFac1 FOR wMov ,tttotal
    RELATION-FIELDS (id,id)
    DATA-RELATION dsFac2 FOR wMov ,ttfact 
    RELATION-FIELDS (id,id).
/* **********************  Internal Procedures  *********************** */


@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE PostFactura:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/*
  Empresa  : Consultoria en Informatica Ejecutiva, S.A. de C.V.
  Modulo   : Tesoreria
  Programa : tesa0930.i
  Funcion  : Incluido del Proceso de Facturas Globales
  Autor    : MGP
  Fecha    : 06-06-1997
*/

    DEFINE INPUT PARAMETER DATASET FOR dsFactura.
    DEFINE INPUT  PARAMETER l-tipo      AS CHAR.
    DEFINE INPUT  PARAMETER l-cliente   AS INT.
    DEFINE INPUT  PARAMETER l-fecoper AS DATE.
    DEFINE INPUT  PARAMETER pIdUser    AS CHAR.
    DEFINE OUTPUT PARAMETER Respuesta  AS CHAR.     
    DEFINE OUTPUT PARAMETER IdError    AS LOGICAL.

IF l-tipo = ? THEN l-tipo = "".

    IF l-fecoper = ? THEN DO :   
        ASSIGN
            Respuesta = 'Colocar la fecha de Operacion'
            IdError   = TRUE.
        RETURN.
    END.
    IF l-cliente < 1 OR
        l-cliente > 10 OR
        l-cliente = 3 THEN 
    DO:
        ASSIGN
            Respuesta = 'Cliente no Valido para Factura Global...'
            IdError   = TRUE.
        RETURN.
    END.
    FIND Cliente WHERE Cliente.Id-Cliente = l-cliente NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Cliente THEN 
    DO:
        ASSIGN
            Respuesta = 'Cliente No Exite'
            IdError   = TRUE.
        RETURN.
    END.  
    IF l-tipo = "" OR NOT CAN-DO("TICKET,REMISION,ELECTRONICA", l-tipo) THEN 
    DO:
        ASSIGN
            Respuesta = "Tipo inválido: debe ser TICKET, REMISION o ELECTRONICA."
            IdError   = TRUE.
        RETURN.
    END.  
    
    FIND Usuario WHERE Usuario.Id-User = pIdUser NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Usuario THEN 
    DO:
        ASSIGN 
            Respuesta = "El usuario especificado no existe."
            IdError = TRUE.
        RETURN.
    END.
    
DEF VAR l-detalle AS CHAR.
   

DEFINE VAR g-iva LIKE SysGeneral.Porc-Iva.

FIND FIRST sysgeneral NO-LOCK NO-ERROR.
IF AVAILABLE sysgeneral THEN g-iva = SysGeneral.Porc-Iva.

ASSIGN l-descuento = 0.

IF l-tipo <> 'ticket' THEN DO:
   FOR EACH wMov WHERE wMov.manual AND wMov.selec :
       ASSIGN l-descuento = l-descuento + wMov.ImpNCred.
   END.
   ASSIGN l-subtotal = l-subtotal + l-descuento.
END.

/* sacar totales */

FOR EACH wMov WHERE wMov.selec :
   ASSIGN l-subtotal = l-subtotal + wMov.ImpVentas
          l-ivaa     = l-ivaa     + wMov.iva
          .
END.

IF l-subtotal  = 0 THEN DO:
   ASSIGN
   Respuesta = "No se permite facturar con valor de cero."
   IdError = TRUE.
    RETURN.
END.  

   
   
STATUS DEFAULT "Generando Factura Global ...".
IF l-folio = '' THEN DO TRANSACTION :
    IF l-cliente < 4 THEN DO:
        FIND VFolio WHERE VFolio.Id-Doc = "FAC"
                      AND VFolio.id-Alm = '' EXCLUSIVE-LOCK NO-ERROR.
   END.
   ELSE
       IF l-cliente = 4 THEN DO: 
           FIND VFolio WHERE VFolio.Id-Doc = "FAC" 
                         AND VFolio.id-Alm = '11' EXCLUSIVE-LOCK NO-ERROR.
       END.
       ELSE
           IF l-cliente = 5 THEN DO:
               FIND VFolio WHERE VFolio.Id-Doc = "FAC"
                             AND VFolio.id-Alm = '12' EXCLUSIVE-LOCK NO-ERROR.
           END.
           ELSE
               IF l-cliente > 5 THEN DO:
                   FIND VFolio WHERE VFolio.Id-Doc = "FAC"
                                 AND VFolio.id-Alm = TRIM(STRING(l-cliente)) EXCLUSIVE-LOCK NO-ERROR.
               END.
               
    ASSIGN l-folio = (IF VFolio.Prefijo = "" THEN STRING(VFolio.Folio,"9999999") ELSE TRIM(VFolio.Prefijo) + STRING(VFolio.Folio,"999999"))
           VFolio.Folio = VFolio.Folio + 1.
END.
RELEASE VFolio.

Proceso:
DO TRANSACTION ON ERROR UNDO Proceso, LEAVE Proceso:

    FIND cliente WHERE cliente.id-cliente = l-cliente NO-LOCK NO-ERROR.

   FOR EACH wIva :
       DELETE wIva.
   END.
   CREATE Factura.
   ASSIGN Factura.Id-Factura  = l-folio
          Factura.FecReg      = l-fecoper
          Factura.Id-Cliente  = l-cliente
          Factura.FecVence    = Factura.FecReg
          Factura.CalleNo     = Cliente.CalleNo
          Factura.Colonia     = Cliente.Colonia
          Factura.CP          = Cliente.CP
          Factura.Id-Ciudad   = Cliente.Id-Ciudad
          Factura.Plazo       = 0
          Factura.Razonsocial = Cliente.RazonSocial
          Factura.RFC         = Cliente.RFC
          Factura.Tel         = Cliente.Tel1
          l-factura           = Factura.Id-factura
          Factura.TipoPrecio  = "M"
          Factura.id-Entrega  = 4
          Factura.Id-Cond     = 0
          Factura.subtotal    = l-subtotal
          Factura.Descuento   = l-descuento
          Factura.IVA         = l-ivaa
          Factura.Tot         = l-subtotal - l-descuento + l-ivaa.

   ASSIGN l-vendedor  = ''
          l-iniciales = ''
          l-ubic      = ''.

   FOR EACH ttfact /*WHERE ttfact.tty = g-tty*/ NO-LOCK :

       FIND FIRST wMov WHERE wMov.idcaja = ttfact.IDmc AND
                  wMov.Turno = ttfact.IdSuplente  
                  NO-LOCK NO-ERROR.

       IF NOT AVAILABLE wMov THEN NEXT.
       IF NOT wMov.selec THEN NEXT.

       FIND Remision WHERE Remision.Id-Remision = ttfact.Documento
                           EXCLUSIVE-LOCK NO-ERROR.
       IF AVAILABLE Remision THEN DO:
          IF l-tipo = 'TICKET' AND Remision.TipoVenta <> 1 THEN NEXT.
          IF l-tipo <> 'TICKET' AND Remision.TipoVenta <> 2 THEN NEXT.
          IF l-tipo = 'REMISION' AND Remision.Folioe <> "" THEN NEXT.
          IF l-tipo = 'ELECTRONICA' AND Remision.Folioe = "" THEN NEXT.
          
          ASSIGN Remision.Facglobal = Factura.Id-Factura.
          IF l-vendedor = '' THEN
             ASSIGN l-vendedor  = Remision.Id-Vendedor
                    l-iniciales = Remision.Iniciales
                    l-ubic      = Remision.Id-Ubic.
          FOR EACH DistIva WHERE DistIva.Id-Factura = Remision.Id-Remision
                             AND DistIva.TipoVenta = Remision.TipoVenta NO-LOCK:
              FIND FIRST wIva WHERE wIva.Porc = DistIva.PorcIva NO-ERROR.
              IF NOT AVAILABLE wIva THEN DO:
                 CREATE wIva.
                 ASSIGN wIva.Porc = DistIva.PorcIva.
              END.
              ASSIGN wIva.Iva = wIva.Iva + DistIva.Iva.
          END.
       END.
       ELSE DO:
          FIND Ncr WHERE Ncr.Id-Ncr = ttfact.documento EXCLUSIVE-LOCK NO-ERROR.
          FIND FIRST DetNcr OF NCR NO-LOCK NO-ERROR.
          IF AVAILABLE DetNcr THEN DO:
             FIND Remision WHERE Remision.Id-Remision = DetNcr.Documento
                                 NO-LOCK NO-ERROR.
             IF AVAILABLE Remision THEN DO:
                 IF l-tipo = 'TICKET' THEN NEXT.
/*
                 IF l-tipo = 'TICKET' AND Remision.TipoVenta <> 1 THEN NEXT.
                 IF l-tipo <> 'TICKET' AND Remision.TipoVenta <> 2 THEN NEXT.
*/
                 IF Remision.TipoVenta = 2 THEN DO:
                    IF l-tipo = 'REMISION' AND Remision.Folioe <> "" THEN NEXT.
                    IF l-tipo = 'ELECTRONICA' AND Remision.Folioe = "" THEN NEXT.
                 END.
                 IF l-vendedor = '' THEN
                    ASSIGN l-vendedor  = Remision.Id-Vendedor
                           l-iniciales = Remision.Iniciales
                           l-ubic      = Remision.Id-Ubic.
             END.
             ASSIGN Ncr.FacGlobal = Factura.id-Factura.
          END.
          FOR EACH DistIva WHERE DistIva.Id-Factura = DetNcr.Documento
                             AND DistIva.TipoVenta = DetNcr.TipoVenta NO-LOCK :
              FIND FIRST wIva WHERE wIva.Porc = DistIva.PorcIva NO-ERROR.
              IF NOT AVAILABLE wIva THEN DO:
                 CREATE wIva.
                 ASSIGN wIva.Porc = DistIva.PorcIva.
              END.
              ASSIGN wIva.Iva = wIva.Iva -
                                 (Ncr.Iva * (DistIva.Participacion / 100)).
          END.
       END.
   END.

   ASSIGN Factura.Id-Vendedor = l-vendedor
          Factura.Iniciales   = l-iniciales
          Factura.Id-Ubic     = IF Factura.Id-Cliente = 4 THEN 'SAL'  ELSE
                                   (IF Factura.Id-Cliente >= 5 THEN l-Ubic ELSE 'MAS').

    /* afectar la cartera de clientes */
    {cxca0001.i &TipoMov      = 1
                &TipoPadre    = 1
                &FecReg       = Factura.FecReg
                &FecVence     = Factura.FecVence
                &Documento    = Factura.Id-Factura
                &RefSaldo     = Factura.Id-Factura
                &Importe      = "(Factura.Tot)"
                &Cliente      = Factura.Id-Cliente
                &Afectar      = " TRUE " }


   IF l-tipo = 'TICKET' THEN DO:
      FOR EACH wMov WHERE wMov.selec 
                     BY wMov.manual BY wMov.idcaja BY wMov.Turno
                     ON ERROR UNDO Proceso, LEAVE Proceso
                     ON ENDKEY UNDO Proceso, LEAVE Proceso:
          IF wMov.manual THEN NEXT.
          CREATE DetFactura.
          ASSIGN DetFactura.Id-Factura = Factura.Id-Factura
                 DetFactura.Tipo       = 2
                 DetFactura.Descr      = 'VARIAS NOTAS MOSTRADOR ' +
                                         wMov.concepto
                 DetFactura.Importe    = wMov.ImpVentas - wMov.ImpNCred
                 DetFactura.IVA        = wMov.iva
                 DetFactura.PorcIVA    = DetFactura.Iva / DetFactura.Importe
                 DetFactura.PrecUnit   = DetFactura.Importe
                 l-reng                = l-reng + 1
                 DetFactura.Reng       = l-reng
                 DetFactura.Cant       = 1.
      END.
      FOR EACH wMov WHERE wMov.manual AND wMov.selec:

          CREATE DetFactura.
          ASSIGN DetFactura.Id-Factura = Factura.Id-Factura
                 DetFactura.Tipo       = 2
                 DetFactura.Descr      = wMov.concepto
                 DetFactura.Importe    = wMov.ImpVentas - wMov.ImpNCred
                 DetFactura.IVA        = wMov.iva
                 DetFactura.PorcIVA    = DetFactura.Iva / DetFactura.Importe
                 DetFactura.PrecUnit   = DetFactura.Importe
                 l-reng                = l-reng + 1
                 DetFactura.Reng       = l-reng
                 DetFactura.Cant       = 1.
          
          FIND FIRST wIva WHERE wIva.Porc = g-iva NO-ERROR.
          IF NOT AVAILABLE wIva THEN DO:
             CREATE wIva.
             ASSIGN wIva.Porc = g-iva.
          END.
          ASSIGN wIva.Iva = wIva.Iva - wMov.iva.
       END.
   END.
   ELSE IF l-tipo = 'REMISION' OR l-tipo = 'ELECTRONICA' THEN DO:
      ASSIGN l-cont     = 0
             l-concepto = "".
      FIND FIRST Remision WHERE Remision.FacGlobal = Factura.Id-Factura
                            NO-LOCK NO-ERROR.
      IF l-tipo = 'REMISION' AND Remision.Folioe <> "" THEN NEXT.
      IF l-tipo = 'ELECTRONICA' AND Remision.Folioe = "" THEN NEXT.
      IF AVAILABLE Remision THEN DO:
         CREATE DetFactura.
         ASSIGN DetFactura.Id-Factura = Factura.Id-Factura
                DetFactura.Tipo       = 5
                DetFactura.Descr      = "NOTAS DE VENTA:"
                DetFactura.Importe    = 0
                DetFactura.IVA        = 0
                DetFactura.PorcIVA    = 0
                DetFactura.PrecUnit   = 0
                l-reng                = l-reng + 1
                DetFactura.Reng       = l-reng
                DetFactura.Cant       = 0.
      END.

      ASSIGN l-cont = 0
             l-detalle = ''.

      FOR EACH Remision WHERE Remision.FacGlobal = Factura.Id-Factura
                              NO-LOCK BREAK BY Remision.Id-Remision
                              ON ERROR UNDO Proceso, LEAVE Proceso
                              ON ENDKEY UNDO Proceso, LEAVE Proceso:
          
          IF l-tipo = 'REMISION' AND Remision.Folioe <> "" THEN NEXT.
          IF l-tipo = 'ELECTRONICA' AND Remision.Folioe = "" THEN NEXT.
          
          ASSIGN l-cont = l-cont + 1
                 l-detalle = l-detalle + "  " + Remision.Id-Remision.
      FIND MovCaja WHERE MovCaja.Refer = Remision.Id-Remision 
                                NO-LOCK NO-ERROR.
          IF AVAILABLE MovCaja THEN DO:
          FIND ChequePF WHERE MovCaja.Folio   = ChequePF.Folio AND
                  MovCaja.Id-Caja = ChequePF.Id-Caja 
                        EXCLUSIVE-LOCK NO-ERROR.
          IF AVAILABLE ChequePF THEN 
         ASSIGN ChequePF.Dep      = TRUE
            ChequePF.Id-Acuse = Remision.FacGlobal.
          END. /* del available movcaja */
          IF l-cont = 5 OR LAST(Remision.Id-Remision) THEN DO:
             CREATE DetFactura.
             ASSIGN DetFactura.Id-Factura = Factura.Id-Factura
                    DetFactura.Descr      = l-detalle
                    DetFactura.Tipo       = 5
                    l-reng                = l-reng + 1
                    DetFactura.Reng       = l-reng
                    l-cont                = 0
                    l-detalle             = ''.
          END.


      END.

      /* Devoluciones y Descuentos */
      FIND FIRST NCR WHERE NCR.FacGlobal = Factura.Id-Factura NO-LOCK NO-ERROR.
      IF AVAILABLE NCR THEN DO:
         CREATE DetFactura.
         ASSIGN DetFactura.Id-Factura = Factura.Id-Factura
                DetFactura.Tipo       = 5
                DetFactura.Descr      = ""
                DetFactura.Importe    = 0
                DetFactura.IVA        = 0
                DetFactura.PorcIVA    = 0
                DetFactura.PrecUnit   = 0
                l-reng                = l-reng + 1
                DetFactura.Reng       = l-reng
                DetFactura.Cant       = 0.
         CREATE DetFactura.
         ASSIGN DetFactura.Id-Factura = Factura.Id-Factura
                DetFactura.Tipo       = 5
                DetFactura.Descr      = "NOTAS DE CREDITO:"
                DetFactura.Importe    = 0
                DetFactura.IVA        = 0
                DetFactura.PorcIVA    = 0
                DetFactura.PrecUnit   = 0
                l-reng                = l-reng + 1
                DetFactura.Reng       = l-reng
                DetFactura.Cant       = 0.
      END.
      ASSIGN l-detalle = ''
             l-cont = 0.

      FOR EACH NCR WHERE NCR.FacGlobal = Factura.Id-Factura
                         NO-LOCK BREAK BY NCR.Id-Ncr
                         ON ERROR UNDO Proceso, LEAVE Proceso
                         ON ENDKEY UNDO Proceso, LEAVE Proceso:

          ASSIGN l-cont = l-cont + 1
                 l-detalle = l-detalle + "  " + NCR.ID-NCR.

          IF l-cont = 5 OR LAST(NCR.ID-NCR) THEN DO:
             FIND FIRST DetNcr WHERE DetNcr.Id-ncr = Ncr.Id-ncr NO-LOCK
                                NO-ERROR.
             CREATE DetFactura.
             ASSIGN DetFactura.Id-Factura = Factura.Id-Factura
                    DetFactura.Descr      = l-detalle
                    DetFactura.Tipo       = 5
                    l-reng                = l-reng + 1
                    DetFactura.Reng       = l-reng
                    l-cont                = 0
                    l-detalle             = ''.
          END.
      END.
      FOR EACH wMov WHERE wMov.manual AND wMov.selec :
          CREATE DetFactura.
          ASSIGN DetFactura.Id-Factura = Factura.Id-Factura
                 DetFactura.Tipo       = 5
                 DetFactura.Descr      = wMov.concepto
                 l-reng                = l-reng + 1
                 DetFactura.Reng       = l-reng.

          FIND FIRST wIva WHERE wIva.Porc = g-iva NO-ERROR.
          IF NOT AVAILABLE wIva THEN DO:
             CREATE wIva.
             ASSIGN wIva.Porc = g-iva.
          END.
          ASSIGN wIva.Iva = wIva.Iva - wMov.iva.
      END.
   END.
  
   /*
   Este calculo nuevo de distiva es para que sea exacto sin inconsistencias
   FLC 26 AGO 2016
   */
   l-Part1 = ROUND((Factura.Iva / (g-iva / 100)) / Factura.SubTotal * 100,2).
   IF l-Part1 <= 100 THEN
      l-Part0 = 100 - l-Part1.
   ELSE ASSIGN l-Part1 = 100
               l-Part0 = 0.
   
   IF l-Part1 > 0 THEN DO:
      CREATE DistIva.
      ASSIGN DistIva.Id-Factura    = Factura.Id-Factura
             DistIva.TipoVenta     = 3
             DistIva.PorcIva       = g-iva
             DistIva.Iva           = Factura.Iva
             DistIva.Participacion = l-Part1.
   END.
   IF l-Part0 > 0 THEN DO:
      CREATE DistIva.
      ASSIGN DistIva.Id-Factura    = Factura.Id-Factura
             DistIva.TipoVenta     = 3
             DistIva.PorcIva       = 0
             DistIva.Iva           = 0
             DistIva.Participacion = l-Part0.
   END.
  
   /*
   Se elimina el grabado de distiva en base a los calculos, esto es un error
   FLC 26 AGO 2016. Se cambia por el calculo directo para que sea exacto
   
   FOR EACH wIva ON ERROR UNDO Proceso, LEAVE Proceso
                  ON ENDKEY UNDO Proceso, LEAVE Proceso:
       CREATE DistIva.
       ASSIGN DistIva.Id-Factura    = Factura.Id-Factura
              DistIva.TipoVenta     = 3
              DistIva.Iva           = wIva.Iva
              DistIva.PorcIva       = wIva.Porc
              DistIva.Participacion = (wIva.Iva / Factura.Iva) * 100.
   END.
   */
   /*manda relacion de donativos*/
   DEF VAR g-Origen AS CHARACTER NO-UNDO.  /* Define the variable */
   FIND Usuario WHERE Usuario.Id-User = pIdUser NO-LOCK NO-ERROR.
   IF AVAILABLE Usuario THEN ASSIGN g-Origen = Usuario.Id-Ubicacion.
   /* pausa donacion
   ASSIGN l-archredo =  '/usr3/tmp/' + "Redo" +  g-tty + STRING(TIME) + ".lst"
          l-impredo = FALSE.
   OUTPUT STREAM s-Salidar TO VALUE(l-archredo) PAGED PAGE-SIZE 60.
       PUT STREAM s-Salidar CONTROL CHR(27) + CHR(15). 
       {cieheadr.i
        &Ancho = 100
        &AnchoM = 94
        &Titulo = "'Relacion de Donativos en Factura Global: ' + Factura.Id-Factura"
        &Subtitulo = "string(Factura.Id-Cliente) + ' ' +  SUBSTRING(Cliente.RazonSocial,1,40)"        
        &AVeces = "'Venta de: ' + l-tipo + ' FecOper: ' + string(l-fecoper) + ' FecFac: ' + string(Factura.FecReg) "
        &Stream = s-Salidar}
       FOR EACH wMov WHERE wMov.selec 
                         BY wMov.manual BY wMov.idcaja BY wMov.Turno:
           IF wMov.manual THEN NEXT.
           IF wMov.Donativo = 0 THEN NEXT.
           ASSIGN l-impredo = TRUE.
           DISPLAY STREAM s-Salidar
               wMov.concepto COLUMN-LABEL 'CONCEPTO'
               wMov.Donativo     COLUMN-LABEL 'DONATIVOS' 
           WITH FRAME f-reporte OVERLAY NO-BOX DOWN WIDTH 100. 
           ACCUMULATE wMov.Donativo (TOTAL).
       END.
       DISPLAY STREAM s-Salidar
           SKIP(1)
           "          TOTALES:   "  
            ACCUM TOTAL  wMov.Donativo
       WITH FRAME f-totredo OVERLAY NO-LABELS NO-BOX DOWN WIDTH 100.
   OUTPUT STREAM s-Salidar CLOSE.
   IF l-impredo THEN DO:
       IF g-Origen = '11' THEN 
           /*UNIX SILENT lpr5 VALUE(l-archredo).*/
           RUN cieimpr10.p (l-archredo,"6").
       ELSE
           /*UNIX SILENT lph2 VALUE(l-archredo).*/
           RUN cieimpr10.p (l-archredo,"h2").
   END. */ 
   FOR EACH wMov :
       DELETE wMov.
   END.
END.  /* Fin de Transaccion Proceso */
RELEASE DetFactura.
RELEASE MovCliente.
RELEASE Remision.
RELEASE Folio.
RELEASE EstCte.
STATUS DEFAULT "".
IF l-factura <> '' THEN DO:
    // RNPC - 2019-12-30 - Crea Acuse
    IF l-Monto > 0 THEN RUN p-creaAcuseCtdo(INPUT pIdUser).
    
   MESSAGE 'Se genero el folio ' + l-factura +
           '. Presione cualquier tecla para continuar...'.

   IF l-tipo = 'TICKET' THEN DO:
   
        /* Genera la factura de credito electronica <MECN 20101124> */
        RUN programas/vtac2068.p(INPUT l-factura, INPUT l-fecoper).  // Genera la factura electronica para globales de tickets (detallada)
     // RUN vtac2063.p(INPUT l-factura).  impresion de la factura se comenta igual que en refacturacion
   
   END.
   ELSE
       // RUN vtac0300.p (INPUT l-factura, INPUT l-factura, INPUT 6). // Imprime Factura de Credito
END.
IF l-factura <> '' THEN DO:
    ASSIGN
    Respuesta = "Se genero el folio " + l-factura 
            IdError   = FALSE.   
        RETURN. 
END.
/*
CLEAR FRAME f-cie-blk1 ALL.
HIDE FRAME f-cie-blk1 NO-PAUSE.
HIDE FRAME f-menu-opc NO-PAUSE.
NEXT _Ciclo. */


          

END PROCEDURE.

/*_ **************************************************
        PROCEDURE p-creaAcuseCtdo 
************************************************** _*/
PROCEDURE p-creaAcuseCtdo:
    DEFINE INPUT  PARAMETER pUsuario    AS CHAR.
    DEFINE INPUT  PARAMETER pFecha      AS DATE.
    DEF VAR l-cob       AS INTEGER                  NO-UNDO.
    DEF VAR meses       AS CHARACTER EXTENT 12 NO-UNDO INITIAL 
    ["ENE","FEB","MAR","ABR","MAY","JUN","JUL","AGO","SEP","OCT","NOV","DIC"].
    
    FIND Usuario WHERE Usuario.Id-User = pUsuario NO-LOCK NO-ERROR.
    FIND FIRST Caja WHERE Caja.Id-Caja = INTEGER(Usuario.Id-Caja) NO-LOCK NO-ERROR.
    IF AVAILABLE Caja THEN DO:
       FIND LAST CtlCaja WHERE CtlCaja.Id-Caja = Caja.Id-Caja
                           AND CtlCaja.FecCierre = ? NO-LOCK NO-ERROR.
    END.
    
    IF AVAILABLE CtlCaja THEN DO:
        FIND FIRST CorteCaja WHERE CorteCaja.Id-Caja = CtlCaja.Id-Caja
                               AND CorteCaja.Turno = CtlCaja.Turno
                               AND CorteCaja.FecOper = CtlCaja.FecOper
                               AND CorteCaja.Declaracion > 0 NO-LOCK NO-ERROR.
        IF AVAILABLE CorteCaja THEN DO:
            MESSAGE "El cajero ya hizo su declaracion.".
            PAUSE 2 NO-MESSAGE.
            RETURN.
        END.
    END.
    
    FIND Cajero OF Usuario NO-LOCK NO-ERROR.
    FIND FIRST Empleado WHERE Empleado.Iniciales = Cajero.Iniciales
                          AND Empleado.Activo NO-LOCK NO-ERROR.
    /*_ ************************************************************************* _*/
    FIND Cobrador WHERE Cobrador.Id-Cobrador = 25 NO-LOCK NO-ERROR.
    IF Cobrador.Iniciales <> '' THEN
        ASSIGN l-cob = 1.  /* Con Cobrador */
    ELSE
        ASSIGN l-cob = 2.  /* Sin Cobrador */
       
    FIND Folio WHERE Folio.Id-Doc = "ACUSEN" + 
                 (IF l-cob = 1 THEN Caja.ConCob ELSE Caja.Sincob)
                 EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE Folio THEN DO:
        MESSAGE "El Folio Para ACUSEN" + (IF l-cob = 1 THEN Caja.ConCob
                ELSE Caja.SinCob) + " en el consecutivo de folios".
        LEAVE.
    END.
    
    CREATE Acuse.
        ASSIGN Acuse.Id-Acuse      = SUBSTRING('0000000', 1 , 7 - (LENGTH(STRING(Folio.Folio)) + 1)) +
                                     STRING(Folio.Folio) + (IF l-cob = 1 THEN Caja.ConCob ELSE
                                     Caja.Sincob) 
               Acuse.Id-Caja       = CtlCaja.Id-Caja WHEN AVAILABLE CtlCaja
               Acuse.Turno         = CtlCaja.Turno WHEN AVAILABLE CtlCaja
               // Acuse.Fecoper    = CtlCaja.FecOper
               Acuse.Fecoper       = TODAY
               // Acuse.FecReg     = g-today
               Acuse.FecReg        = TODAY
               Acuse.FecDep        = TODAY   // 2019-10-04 - g-today
               Acuse.UsuarioReg    = CAPS(pUsuario)
               Acuse.Tipo          = "N"
               Acuse.Estatus       = 4
               Acuse.Iniciales     = Empleado.Iniciales WHEN AVAILABLE Empleado
               Acuse.Id-Cajero     = Usuario.Id-Cajero WHEN AVAILABLE Usuario
               Acuse.Id-Cliente    = 1      // Cliente Cedis, Matriz
               Acuse.Id-Cobrador   = 25   // Numero Fijo (Validado en tabla Cobrador) 
               Acuse.AcuseCobrador = "0000000"
               Acuse.Id-Origen     = 'MA'       // 2019-10-04
               Acuse.Comen[1]      = "Pagos aplicados de FecOper " + STRING(pFecha)
               Acuse.Comen[2]      = "Factura Global " + STRING(Factura.Id-Factura)
               Acuse.Comen[3]      = ""
               Folio.Folio         = Folio.Folio + 1.
               
        CREATE DocAcuse.
            ASSIGN
                DocAcuse.Id-Acuse   = Acuse.Id-Acuse
                DocAcuse.Documento  = Factura.Id-factura WHEN AVAILABLE Factura
                DocAcuse.FecDoc     = Factura.FecReg WHEN AVAILABLE Factura  
                DocAcuse.Id-MC      = 1
                DocAcuse.Sec        = 1
                DocAcuse.ImpPago    = l-Monto
                DocAcuse.Id-Moneda  = 1
                DocAcuse.TipoCambio = 1.
        
        // Registro el Pago
        CREATE PagoAcuse.
        ASSIGN
            PagoAcuse.Id-Acuse    = Acuse.Id-Acuse
            PagoAcuse.Sec         = 1 
            PagoAcuse.Id-Tp       = 57  // DEP LOCAL 
            PagoAcuse.ImpRecibido = l-Monto
            PagoAcuse.Importe     = l-Monto
            PagoAcuse.Id-Banco    = 25  
            PagoAcuse.CPFormaPago = '01'       // "01=Efectivo,02=Cheque,03=Transferencia"
            PagoAcuse.Id-Moneda   = 1
            PagoAcuse.TC          = 1     
            PagoAcuse.TipoCambio  = 1.
            
        // Afectaci�n de Clientes - EstCte
        {cxca0006.i
            &Cliente = Acuse.Id-Cliente
            &Importe = DocAcuse.ImpPago
            &renglon = 4
            &fecha   = Acuse.FecReg }
            
        // Afectar la cartera de clientes 
        {cxca0001.i
             &TipoMov      = PagoAcuse.Id-tp
             &TipoPadre    = 1
             &FecReg       = Acuse.FecDep
             &Documento    = Acuse.Id-Acuse
             &RefSaldo     = DocAcuse.Documento
             &Cliente      = Acuse.Id-Cliente
             &Afectar      = " TRUE "
             &Importe      = " (PagoAcuse.ImpRecibido * (-1)) " 
             &Moneda       = 1           
             &TipoCambio   = 1 }
    RELEASE Acuse.
    RELEASE DocAcuse.
    RELEASE PagoAcuse.
    RELEASE Folio.
END PROCEDURE.   