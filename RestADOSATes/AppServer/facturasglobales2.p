@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*------------------------------------------------------------------------
    File        : teserp001.p
    Purpose     : 

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

DEFINE BUFFER bf-MovNormales   FOR MovCaja.  /* Remisiones normales */
DEFINE BUFFER bf-MovPostfech   FOR MovCaja.  /* Remisiones postfechadas */
DEFINE BUFFER bf-MovDevol      FOR MovCaja.  /* Devoluciones */
      

DEFINE TEMP-TABLE wIva
    FIELD Iva  LIKE Factura.Iva
    FIELD Porc LIKE DistIva.PorcIva.
  



DEFINE TEMP-TABLE ttfact 
    FIELD id         AS CHAR
    FIELD Documento  LIKE TFact.Documento
    FIELD IdCliente  LIKE TFact.Id-Cliente
    FIELD nomcte     LIKE cliente.razonsocial
    FIELD subtotal   AS DECIMAL
    FIELD iva        AS DECIMAL
    FIELD tot        AS DECIMAL
    FIELD selec      AS LOGICAL 
    FIELD Descr      LIKE TFact.Descr
    FIELD tipo       AS INTEGER
    FIELD Flag       LIKE TFact.Flag
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

DEFINE TEMP-TABLE ttSucursales NO-UNDO
    FIELD IdSuc AS CHAR.

DEFINE DATASET dsFactura FOR wMov ,tttotal,ttfact 
    DATA-RELATION dsFac1 FOR wMov ,tttotal
    RELATION-FIELDS (id,id)
    DATA-RELATION dsFac2 FOR wMov ,ttfact
    RELATION-FIELDS (id,id).
/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetGlobal2:
    DEFINE INPUT  PARAMETER l-fecoper AS DATE.
    DEFINE INPUT  PARAMETER l-tipo      AS CHAR.
    DEFINE INPUT  PARAMETER l-cliente      AS INT.
    DEFINE OUTPUT PARAMETER Respuesta  AS CHAR. 
    DEFINE OUTPUT PARAMETER IdError    AS LOGICAL.
    DEFINE OUTPUT PARAMETER DATASET FOR dsFactura.
    
    IF l-tipo = ? THEN l-tipo = "".
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
    
    EMPTY TEMP-TABLE ttSucursales.
    EMPTY TEMP-TABLE ttfact.   
    EMPTY TEMP-TABLE tttotal.
    EMPTY TEMP-TABLE wIva.
    EMPTY TEMP-TABLE wMov.
    ASSIGN 
        l-subtotal = 0
        l-ivaa     = 0
        l-redo     = 0
        l-folio    = ''
        l-Monto    = 0.

    IF l-cliente = 4 THEN 
    DO:
        FOR EACH Caja WHERE Caja.Id-Depto = "506" NO-LOCK:
            CREATE ttSucursales.
            ttSucursales.IdSuc = STRING(Caja.Id-Caja).
        END.
    END.
    ELSE IF l-cliente = 5 THEN 
        DO: 
            FOR EACH Caja WHERE Caja.Id-Depto = "510" NO-LOCK:
                CREATE ttSucursales.
                ttSucursales.IdSuc = STRING(Caja.Id-Caja).
            END.
            
        END.
        ELSE IF l-cliente = 6 THEN 
            DO: /*
                FOR EACH Caja WHERE Caja.Id-Depto = "511" NO-LOCK:
                    l-ListSuc = l-ListSuc + MINIMUM(l-ListSuc,",") + STRING(Caja.Id-Caja).
                END. */
                FOR EACH Caja WHERE Caja.Id-Depto = "511" NO-LOCK:
                    CREATE ttSucursales.
                    ttSucursales.IdSuc = STRING(Caja.Id-Caja).
                END.
            END.
            ELSE IF l-cliente = 7 THEN 
                DO: /*
                    FOR EACH Caja WHERE Caja.Id-Depto = "512" NO-LOCK:
                        l-ListSuc = l-ListSuc + MINIMUM(l-ListSuc,",") + STRING(Caja.Id-Caja).
                    END. */
                    
                    FOR EACH Caja WHERE Caja.Id-Depto = "512" NO-LOCK:
                        CREATE ttSucursales.
                        ttSucursales.IdSuc = STRING(Caja.Id-Caja).
                    END. 
                END.
                ELSE IF l-cliente = 8 THEN 
                    DO: /*
                        FOR EACH Caja WHERE Caja.Id-Depto = "513" NO-LOCK:
                            l-ListSuc = l-ListSuc + MINIMUM(l-ListSuc,",") + STRING(Caja.Id-Caja).
                        END. */
                        FOR EACH Caja WHERE Caja.Id-Depto = "513" NO-LOCK:
                            CREATE ttSucursales.
                            ttSucursales.IdSuc = STRING(Caja.Id-Caja).
                        END.
                    END.
                    ELSE IF l-cliente = 9 THEN 
                        DO: /*
                            FOR EACH Caja WHERE Caja.Id-Depto = "514" NO-LOCK:
                                l-ListSuc = l-ListSuc + MINIMUM(l-ListSuc,",") + STRING(Caja.Id-Caja).
                            END. */
                            FOR EACH Caja WHERE Caja.Id-Depto = "514" NO-LOCK:
                                CREATE ttSucursales.
                                ttSucursales.IdSuc = STRING(Caja.Id-Caja).
                            END.
                        END.
                        ELSE IF l-cliente = 10 THEN 
                            DO: /*
                                FOR EACH Caja WHERE Caja.Id-Depto = "515" NO-LOCK:
                                    l-ListSuc = l-ListSuc + MINIMUM(l-ListSuc,",") + STRING(Caja.Id-Caja).
                                END. */
                                FOR EACH Caja WHERE Caja.Id-Depto = "515" NO-LOCK:
                                    CREATE ttSucursales.
                                    ttSucursales.IdSuc = STRING(Caja.Id-Caja).
                                END.
                            END.
                            ELSE 
                            DO: /*
                                FOR EACH Caja WHERE CAN-DO("506,510,511,512,513,514,515",Caja.Id-Depto) NO-LOCK:
                                    l-ListSuc = l-ListSuc + MINIMUM(l-ListSuc,",") + STRING(Caja.Id-Caja).
                                END. */
                                FOR EACH Caja WHERE CAN-DO("506,510,511,512,513,514,515",Caja.Id-Depto) NO-LOCK:
                                    CREATE ttSucursales.
                                    ttSucursales.IdSuc = STRING(Caja.Id-Caja).
                                END.
                            END.
   
    /* Busqueda de Remisiones NORMALES */
    FOR EACH bf-MovNormales WHERE bf-MovNormales.FecDep    <= l-fecoper  
        AND bf-MovNormales.Fecoper    = l-fecoper  
        AND bf-MovNormales.TipoVenta  = (IF l-tipo = 'Ticket' THEN 1 ELSE 2 )
        NO-LOCK 
        USE-INDEX Idx-Fec
        BREAK BY bf-MovNormales.Id-Caja 
        BY bf-MovNormales.Turno:
          
        /* --- Nueva condición usando ttSucursales (reemplazo del CAN-DO) --- */
        FIND FIRST ttSucursales WHERE ttSucursales.IdSuc = STRING(bf-MovNormales.Id-Caja) NO-ERROR.
    
        /* 
           - Si l-cliente < 3: QUEREMOS sucursales que NO estén en ttSucursales.
           - Si l-cliente >= 3: QUEREMOS sucursales que SÍ estén en ttSucursales.
        */
        IF (l-cliente < 3 AND AVAILABLE ttSucursales) THEN NEXT.     /* Excluye sucursales de la lista */
        IF (l-cliente >= 3 AND NOT AVAILABLE ttSucursales) THEN NEXT. /* Excluye sucursales fuera de la lista */
        IF FIRST-OF(bf-MovNormales.Turno) THEN 
        DO:
            
            FIND FIRST wMov WHERE wMov.IdCaja = bf-MovNormales.Id-Caja AND
                wMov.Turno = bf-MovNormales.Turno 
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE wMov THEN 
            DO:   
                CREATE wMov.
                ASSIGN 
                    wMov.id       = "T"
                    wMov.idcaja   = bf-MovNormales.id-Caja
                    wMov.Turno    = bf-MovNormales.Turno
                    wMov.Selec    = TRUE 
                    wMov.Concepto = "C" + STRING(bf-MovNormales.Id-Caja) + "  T" +
                                           STRING(bf-MovNormales.Turno).
            END.
        END.

        FIND Remision WHERE Remision.Id-Remision = bf-MovNormales.Referencia AND
            Remision.TipoVenta = bf-MovNormales.TipoVenta 
            NO-LOCK NO-ERROR.
        IF AVAILABLE Remision THEN 
        DO:

            IF Remision.FacGlobal <> "" THEN 
                NEXT.
            IF Remision.PorIdRemision <> "" /*AND Remision.SustIdRemision = ""*/ 
                THEN 
            DO:
                FIND b-Remision 
                    WHERE b-Remision.Id-Remision = Remision.PorIdRemision
                    NO-LOCK NO-ERROR.
                IF AVAILABLE b-Remision THEN 
                DO:
                    FIND FIRST b-MovCaja 
                        WHERE b-MovCaja.Refer = b-Remision.Id-Remision
                        AND b-MovCaja.TipoVenta = b-Remision.TipoVenta
                        NO-LOCK NO-ERROR.
                    IF AVAILABLE b-MovCaja AND b-MovCaja.FecOper <= bf-MovNormales.FecOper
                        THEN NEXT.
                END.
            END.
            ELSE IF Remision.SustIdRemision <> "" AND Remision.PorIdRemision = "" 
                    AND Remision.FecCanc = ?
                    THEN 
                DO:
                    FIND b-Remision 
                        WHERE b-Remision.Id-Remision = Remision.SustIdRemision
                        NO-LOCK NO-ERROR.
                    IF AVAILABLE b-Remision THEN 
                    DO:
                        FIND FIRST b-MovCaja 
                            WHERE b-MovCaja.Refer = b-Remision.Id-Remision
                            AND b-MovCaja.TipoVenta = b-Remision.TipoVenta
                            NO-LOCK NO-ERROR.
                        IF AVAILABLE b-MovCaja AND b-MovCaja.FecOper < bf-MovNormales.FecOper
                            THEN NEXT.
                    END.
                END.
                ELSE IF bf-MovNormales.Canc THEN NEXT.

            IF l-tipo = 'Remision' /*AND Remision.Folioe <> ""*/ THEN NEXT.
            /*IF l-tipo = 'Electronica' AND Remision.Folioe = "" THEN NEXT.*/
            
            // RNPC - 2019-12-27 - Busco si hay un deposito de Santander con esta remision
            FIND FIRST DepBanco WHERE DepBanco.Conciliado AND 
                DepBanco.Activo AND
                DepBanco.Id-Cliente = Remision.Id-Cliente AND 
                CAN-DO(DepBanco.Id-Remision,Remision.Id-Remision) 
                NO-LOCK NO-ERROR.                            
            IF AVAILABLE DepBanco THEN l-Monto = l-Monto + Remision.Tot.  

            /*
            CREATE ttfact.
            ASSIGN
                ttfact.id         = "T"
                ttfact.Documento  = Remision.Id-Remision
                ttfact.IdSuplente = bf-MovNormales.Turno
                ttfact.tty        = "J"
                ttfact.IDmc       = bf-MovNormales.Id-Caja. */

            ASSIGN 
                wMov.ImpVentas = wMov.ImpVentas + Remision.Tot - Remision.Iva
                l-subtotal     = l-subtotal + Remision.Tot - Remision.iva
                wMov.Iva       = wMov.Iva + Remision.Iva
                l-ivaa         = l-ivaa + Remision.Iva
                wMov.Donativo  = wMov.Donativo + Remision.Redo
                l-redo         = l-redo + Remision.Redo
                wMov.Total     = wMov.ImpVentas - wMov.ImpNCred.

         /*   FIND Cliente WHERE Cliente.Id-cliente = Remision.Id-cliente
                NO-LOCK NO-ERROR.
           
            ASSIGN 
                ttfact.IdCliente = Remision.Id-cliente
                ttfact.nomcte    = cliente.razonsocial
                ttfact.subtotal  = remision.Tot - remision.iva
                ttfact.iva       = remision.iva
                ttfact.tot       = Remision.Tot. */

            IF bf-MovNormales.Id-ncr <> "" THEN 
            DO:
                
                FIND Ncr WHERE Ncr.Id-ncr = bf-MovNormales.Id-Ncr 
                    NO-LOCK NO-ERROR.
                IF AVAILABLE Ncr AND Ncr.FacGlobal = "" AND 
                    NCR.FecCanc = ? THEN 
                DO:
                    /*
                    CREATE ttfact.
                    ASSIGN 
                        ttfact.id         = "T"
                        ttfact.Documento  = Ncr.Id-ncr
                        ttfact.IdSuplente = bf-MovNormales.Turno
                        ttfact.tty        = "J"
                        ttfact.IDmc       = bf-MovNormales.Id-Caja. */

                    ASSIGN 
                        wMov.ImpNCred = wMov.ImpNCred + NCR.Subtotal
                        l-subtotal    = l-subtotal - NCR.Subtotal
                        wMov.Iva      = wMov.Iva - Ncr.Iva 
                        l-ivaa        = l-ivaa - Ncr.Iva
                        wMov.Total    = wMov.ImpVentas - wMov.ImpNCred.

                 /*   FIND Cliente WHERE Cliente.Id-cliente = Ncr.Id-cliente
                        NO-LOCK NO-ERROR.
                    ASSIGN 
                        ttfact.IdCliente = ncr.id-cliente
                        ttfact.nomcte    = Cliente.RazonSoc
                        ttfact.subtotal  = ncr.subtotal
                        ttfact.iva       = ncr.iva
                        ttfact.tot       = ncr.tot. */
                END.

            END. 
            
            FIND FIRST DetMovC WHERE DetMovC.Id-Caja = bf-MovNormales.Id-Caja AND
                DetMovC.Folio   = bf-MovNormales.Folio   AND
                DetMovC.Id-TP   = 65
                NO-LOCK NO-ERROR.
            
            IF AVAILABLE DetMovC THEN 
            DO: /*
                CREATE ttfact.
                ASSIGN 
                    ttfact.id         = "T"
                    ttfact.Documento  = bf-MovNormales.Refer
                    ttfact.IdSuplente = bf-MovNormales.Turno
                    ttfact.tty        = "J"
                    ttfact.IDmc       = bf-MovNormales.Id-Caja. */
                  
                ASSIGN 
                    wMov.ImpNCred = wMov.ImpNCred + DetMovC.MontoPago
                    l-subtotal    = l-subtotal - DetMovC.MontoPago
                    wMov.Iva      = wMov.Iva - (DetMovC.MontoPago * 0.15)
                    l-ivaa        = l-ivaa - (DetMovC.MontoPago * 0.15)
                    wMov.Total    = wMov.ImpVentas - wMov.ImpNCred.
            END.
        END.
    END.

    /*Busqueda de Remisiones PostFechadas para Hoy */
    FOR EACH bf-MovPostfech WHERE bf-MovPostfech.FecOper  <> l-fecOper AND
        bf-MovPostfech.FecDep     =  l-fecOper    AND
        bf-MovPostfech.Canc       =  FALSE        AND
        bf-MovPostfech.TipoVenta  =  (IF l-tipo = 'Ticket' THEN 1 ELSE 2) 
        NO-LOCK 
        USE-INDEX Idx-Post
        BREAK BY bf-MovPostfech.Id-caja 
        BY bf-MovPostfech.Turno :
            
          /* --- Nueva condición usando ttSucursales (reemplazo del CAN-DO) --- */
        FIND FIRST ttSucursales WHERE ttSucursales.IdSuc = STRING(bf-MovPostfech.Id-Caja) NO-ERROR.
    
        /* 
           - Si l-cliente < 3: QUEREMOS sucursales que NO estén en ttSucursales.
           - Si l-cliente >= 3: QUEREMOS sucursales que SÍ estén en ttSucursales.
        */
        IF (l-cliente < 3 AND AVAILABLE ttSucursales) THEN NEXT.     /* Excluye sucursales de la lista */
        IF (l-cliente >= 3 AND NOT AVAILABLE ttSucursales) THEN NEXT. /* Excluye sucursales fuera de la lista */    

        IF FIRST-OF(bf-MovPostfech.Turno) THEN 
        DO:
            
            FIND FIRST wMov WHERE wMov.idcaja = bf-MovPostfech.Id-Caja AND
                wMov.Turno = bf-MovPostfech.Turno 
                NO-LOCK NO-ERROR.
           
            IF NOT AVAILABLE wMov THEN 
            DO:
                
                CREATE wMov.
                ASSIGN 
                    wMov.id       = "T"
                    wMov.IdCaja   = bf-MovPostfech.id-Caja
                    wMov.Turno    = bf-MovPostfech.Turno
                    wMov.Selec    = TRUE 
                    wMov.Concepto = "C" + STRING(bf-MovPostfech.Id-Caja) + "  T" +
                                           STRING(bf-MovPostfech.Turno).
            END.
        END.

        FIND Remision WHERE Remision.Id-Remision = bf-MovPostfech.Referencia AND
            Remision.TipoVenta = bf-MovPostfech.TipoVenta 
            NO-LOCK NO-ERROR.
        IF AVAILABLE Remision THEN 
        DO:
            
            IF bf-MovPostfech.FecDep <= bf-MovPostfech.FecOper THEN 
                NEXT.
           
            IF Remision.FacGlobal <> "" THEN 
                NEXT.
                
            IF l-tipo = 'Remision' /*AND Remision.Folioe <> ""*/ THEN NEXT.
            /*IF l-tipo = 'Electronica' AND Remision.Folioe = "" THEN NEXT.*/
           
           /* CREATE ttfact.
           
            ASSIGN 
                ttfact.id         = "T"
                ttfact.Documento  = Remision.Id-Remision
                ttfact.IdSuplente = bf-MovPostfech.Turno
                ttfact.tty        = "J"
                ttfact.IDmc       = bf-MovPostfech.Id-Caja. */

            ASSIGN 
                wMov.ImpVentas = wMov.ImpVentas + Remision.Tot - Remision.Iva
                l-subtotal     = l-subtotal + Remision.Tot - Remision.iva
                wMov.Iva       = wMov.Iva + Remision.Iva
                l-ivaa         = l-ivaa + Remision.Iva
                wMov.Donativo  = wMov.Donativo + Remision.Redo
                l-redo         = l-redo + Remision.Redo
                wMov.Total     = wMov.ImpVentas - wMov.ImpNCred.
           

         /*   FIND Cliente WHERE Cliente.Id-cliente = Remision.Id-cliente
                NO-LOCK NO-ERROR.
            ASSIGN 
                ttfact.IdCliente = Remision.Id-cliente
                ttfact.nomcte    = cliente.razonsocial
                ttfact.subtotal  = remision.Tot - remision.iva
                ttfact.iva       = remision.iva
                ttfact.tot       = Remision.Tot. */
     
            IF bf-MovPostfech.Id-ncr <> "" THEN 
            DO:
                
                FIND Ncr WHERE Ncr.Id-ncr = bf-MovPostfech.Id-Ncr 
                    NO-LOCK NO-ERROR.
                IF AVAILABLE Ncr AND 
                    Ncr.FacGlobal = "" AND 
                    NCR.FecCanc = ? THEN 
                DO:
                    /*
                    CREATE ttfact.
                    ASSIGN 
                        ttfact.id         = "T"
                        ttfact.Documento  = Ncr.Id-ncr
                        ttfact.IdSuplente = bf-MovPostfech.Turno
                        ttfact.tty        = "J"
                        ttfact.IDmc       = bf-MovPostfech.Id-Caja. */
                    
                    ASSIGN 
                        wMov.ImpNCred = wMov.ImpNCred + NCR.Subtotal
                        l-subtotal    = l-subtotal - NCR.Subtotal
                        wMov.Iva      = wMov.Iva - Ncr.Iva 
                        l-ivaa        = l-ivaa - Ncr.Iva
                        wMov.Total    = wMov.ImpVentas - wMov.ImpNCred.

                /*    FIND Cliente WHERE Cliente.Id-cliente = Ncr.Id-cliente
                        NO-LOCK NO-ERROR.
                    ASSIGN 
                        ttfact.id        = "T"
                        ttfact.IdCliente = ncr.id-cliente
                        ttfact.nomcte    = Cliente.RazonSoc
                        ttfact.subtotal  = ncr.subtotal
                        ttfact.iva       = ncr.iva
                        ttfact.tot       = ncr.tot. */
                END.
            END.   
        END.
    END.
   
    /* Busqueda de Devoluciones de Efectivo */
    FOR EACH bf-MovDevol WHERE bf-MovDevol.FecOper   = l-fecoper AND
        bf-MovDevol.Canc      = FALSE     AND 
        (bf-MovDevol.TipoVenta = 4 OR bf-MovDevol.TipoVenta = 8 OR bf-MovDevol.TipoVenta = 9)
        NO-LOCK 
        USE-INDEX Idx-Fec
        BREAK BY bf-MovDevol.Id-Caja 
        BY bf-MovDevol.Turno:   
            
          /* --- Nueva condición usando ttSucursales (reemplazo del CAN-DO) --- */
        FIND FIRST ttSucursales WHERE ttSucursales.IdSuc = STRING(bf-MovDevol.Id-Caja) NO-ERROR.
    
        /* 
           - Si l-cliente < 3: QUEREMOS sucursales que NO estén en ttSucursales.
           - Si l-cliente >= 3: QUEREMOS sucursales que SÍ estén en ttSucursales.
        */
        IF (l-cliente < 3 AND AVAILABLE ttSucursales) THEN NEXT.     /* Excluye sucursales de la lista */
        IF (l-cliente >= 3 AND NOT AVAILABLE ttSucursales) THEN NEXT. /* Excluye sucursales fuera de la lista */    

        IF FIRST-OF(bf-MovDevol.Turno) THEN 
        DO:
           
            FIND FIRST wMov WHERE wMov.IdCaja = bf-MovDevol.Id-Caja AND
                wMov.Turno = bf-MovDevol.Turno 
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE wMov THEN 
            DO:
              
                CREATE wMov.
                ASSIGN 
                    wMov.id       = "T"
                    wMov.IdCaja   = bf-MovDevol.id-Caja
                    wMov.Turno    = bf-MovDevol.Turno
                    wMov.Concepto = "C" + STRING(bf-MovDevol.Id-Caja) + "  T" +
                                           STRING(bf-MovDevol.Turno).
            END.
        END.
        
        IF bf-MovDevol.tipoventa = 4 THEN 
        DO:
            
            FIND Devolucion WHERE Devolucion.Id-Dev = INT(bf-MovDevol.Referencia)
                NO-LOCK NO-ERROR.
            
            IF AVAILABLE Devolucion THEN 
            DO:
                
                IF Devolucion.TipoVenta <> (IF l-tipo = 'ticket' THEN 1 
                ELSE 2) THEN
                    NEXT.
           
                FIND Ncr WHERE Ncr.Id-Ncr = bf-MovDevol.Id-ncr 
                    NO-LOCK NO-ERROR.
           
                IF Ncr.facGlobal <> '' THEN 
                    NEXT.
           


                FIND FIRST ttfact WHERE ttfact.Documento = Ncr.Id-ncr AND 
                    ttfact.tty = "J" 
                    NO-LOCK NO-ERROR.

                IF NOT AVAILABLE ttfact THEN 
                DO:
                    /*
                    CREATE ttfact.
                    ASSIGN 
                        ttfact.id         = "T"
                        ttfact.Documento  = Ncr.Id-ncr
                        ttfact.IdSuplente = bf-MovDevol.Turno
                        ttfact.tty        = "J"
                        ttfact.IDmc       = bf-MovDevol.Id-Caja. */

                    ASSIGN 
                        wMov.ImpNCred = wMov.ImpNCred + Devolucion.Tot - Devolucion.Iva
                        l-subtotal    = l-subtotal - (Devolucion.Tot -                                    Devolucion.iva)
                        wMov.Iva      = wMov.Iva - Devolucion.Iva
                        l-ivaa        = l-ivaa - Devolucion.Iva
                        wMov.Total    = wMov.ImpVentas - wMov.ImpNCred.


                 /*   FIND Cliente WHERE Cliente.Id-cliente = Ncr.Id-cliente
                        NO-LOCK NO-ERROR.
                    ASSIGN 
                        ttfact.IdCliente = ncr.id-cliente
                        ttfact.nomcte    = Cliente.RazonSoc
                        ttfact.subtotal  = ncr.subtotal
                        ttfact.iva       = ncr.iva
                        ttfact.tot       = ncr.tot. */  
                END.
            END.
        END.
        ELSE 
        DO:
            
            FIND NCR WHERE NCR.Id-ncr = bf-MovDevol.Referencia 
                NO-LOCK NO-ERROR.
        
            IF AVAILABLE NCR THEN 
            DO:
                
                FIND FIRST DetNcr OF Ncr 
                    NO-LOCK NO-ERROR.
                IF l-tipo = 'ticket' THEN NEXT.
                /*
                IF DetNcr.TipoVenta <> (IF l-tipo = 'ticket' THEN 1 ELSE 2) THEN
                    NEXT.
                */
            
                IF NCR.FacGlobal <> '' THEN 
                    NEXT.
           
                FIND Remision WHERE Remision.Id-Remision = DetNcr.Documento AND
                    Remision.TipoVenta = 2 
                    NO-LOCK NO-ERROR.
                IF AVAILABLE Remision THEN 
                DO:
                    IF l-tipo = 'Remision' /*AND Remision.Folioe <> ""*/ THEN NEXT.
                    /*IF l-tipo = 'Electronica' AND Remision.Folioe = "" THEN NEXT.*/
                    
                    FIND FIRST B-Mov WHERE B-Mov.Referencia = Remision.Id-Remision AND
                        B-Mov.TipoVenta = (IF l-tipo = 'ticket' THEN 1 ELSE 2)
                        NO-LOCK NO-ERROR.
               
                    IF AVAILABLE B-Mov AND B-Mov.FecOper >= B-Mov.FecDep OR
                        bf-MovDevol.Referencia = '001570M' THEN 
                    DO:
                  

                  
                        FIND FIRST ttfact WHERE ttfact.Documento = Ncr.Id-Ncr AND
                            ttfact.tty = "J" 
                            NO-LOCK NO-ERROR.
                        
                        IF NOT AVAILABLE ttfact THEN 
                        DO: /*
                            CREATE ttfact.
                        
                            ASSIGN 
                                ttfact.id         = "T"
                                ttfact.Documento  = Ncr.Id-ncr
                                ttfact.IdSuplente = bf-MovDevol.Turno
                                ttfact.tty        = "J"
                                ttfact.IDmc       = bf-MovDevol.Id-Caja. */
                        
                            ASSIGN 
                                wMov.ImpNCred = wMov.ImpNCred + NCR.Subtotal
                                l-subtotal    = l-subtotal - NCR.Subtotal
                                wMov.Iva      = wMov.Iva - Ncr.Iva
                                l-ivaa        = l-ivaa - NCR.Iva
                                wMov.Total    = wMov.ImpVentas - wMov.ImpNCred.
    
                         /*   FIND Cliente WHERE Cliente.Id-cliente = Ncr.Id-cliente
                                NO-LOCK NO-ERROR.
                            ASSIGN 
                                ttfact.IdCliente = ncr.id-cliente
                                ttfact.nomcte    = Cliente.RazonSoc
                                ttfact.subtotal  = ncr.subtotal
                                ttfact.iva       = ncr.iva
                                ttfact.tot       = ncr.tot. */
                        END.
                    END.
                END.
                ELSE 
                DO:
                    IF l-tipo <> 'Electronica' THEN NEXT.
                    FIND FIRST ttfact WHERE ttfact.Documento = Ncr.Id-Ncr AND
                        ttfact.tty = "J" 
                        NO-LOCK NO-ERROR.
                    
                    IF NOT AVAILABLE ttfact THEN 
                    DO: /*
                        CREATE ttfact.   
                    
                        ASSIGN 
                            ttfact.id         = "T"
                            ttfact.Documento  = Ncr.Id-ncr
                            ttfact.IdSuplente = bf-MovDevol.Turno
                            ttfact.tty        = "J"
                            ttfact.IDmc       = bf-MovDevol.Id-Caja. */
                    
                        ASSIGN 
                            wMov.ImpNCred = wMov.ImpNCred + NCR.Subtotal
                            l-subtotal    = l-subtotal - NCR.Subtotal
                            wMov.Iva      = wMov.Iva - Ncr.Iva
                            l-ivaa        = l-ivaa - NCR.Iva
                            wMov.Total    = wMov.ImpVentas - wMov.ImpNCred.

                      /*  FIND Cliente WHERE Cliente.Id-cliente = Ncr.Id-cliente
                            NO-LOCK NO-ERROR.
                        ASSIGN 
                            ttfact.IdCliente = ncr.id-cliente
                            ttfact.nomcte    = Cliente.RazonSoc
                            ttfact.subtotal  = ncr.subtotal
                            ttfact.iva       = ncr.iva
                            ttfact.tot       = ncr.tot. */
                    END.
                END.
            END.
        END.
    END.     
   
   
    FOR EACH wMov WHERE wMov.ImpVentas = 0 AND wMov.ImpNCred = 0 :
        DELETE wMov.
    END.

    ASSIGN 
        l-tot = l-subtotal + l-ivaa.
    
    CREATE  tttotal.
    ASSIGN 
        tttotal.id     = "T"
        tttotal.Subtot = l-subtotal
        tttotal.Iva    = l-ivaa
        tttotal.Tot    = l-tot
        tttotal.Don    = l-redo.  

    /*
        l-subtotal LABEL 'Subtot'
        l-ivaa     LABEL 'Iva'
        l-tot      LABEL 'Tot'
        l-redo     LABEL 'Don' */   
        
    RETURN.                         
END PROCEDURE.   
