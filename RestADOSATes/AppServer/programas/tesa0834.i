/*
  Empresa    : Consultoria en Informatica Ejecutiva S.A. de C.V.
  Modulo     : tesoreria
  Programa   : tesa0834.i
  Funcion    : Declaracion de Variabes y formas
  Usado por  : tesa0831.p
  Autor      : LUIS
  Fecha      : 25/03/97
    
  Modificacion: ALEX  | 24AGO2016 | Agregar al corte expreso los vales, vouchers y cheques
                ALEX  | 28JUL2017 | Agregar devoluciones de TC y separarlas de las devoluciones de efectivo
*/

DEF STREAM s-salida .
DEF VAR l-time       AS CHAR                                          NO-UNDO.
DEF VAR l-tipo       AS CHAR FORMAT "x(3)"                            NO-UNDO.
DEF VAR l-reporte    AS CHAR FORMAT "x(12)"                           NO-UNDO.
DEF VAR l-bytes      AS CHAR                                          NO-UNDO.
DEF VAR l-enca       AS CHAR FORMAT "x(20)"                           NO-UNDO.
DEF VAR l-tam        AS INTE                                          NO-UNDO.
DEF VAR l-entro      AS LOGI                                          NO-UNDO.
DEF VAR l-ventas     AS DECI FORMAT "$zzzz,zz9.99-" LABEL " CONTADO"  NO-UNDO.
DEF VAR l-ticket     AS DECI FORMAT "$zzzz,zz9.99-" LABEL "  TICKET"  NO-UNDO.
DEF VAR l-contventas AS INTE FORMAT "zz,zz9"                          NO-UNDO.
DEF VAR l-contticket AS INTE FORMAT "zz,zz9"                          NO-UNDO.
DEF VAR l-hora       AS CHAR FORMAT "x(5)" COLUMN-LABEL "Hora" 	      NO-UNDO.
DEF VAR l-ctrl       AS INTE                                          NO-UNDO.
DEF VAR l-contfacturas AS INTE FORMAT "zz,zz9"                        NO-UNDO.
DEF VAR l-contdev    AS INTE FORMAT "zz,zz9"                          NO-UNDO.
DEF VAR l-ticketnormales AS DECI FORMAT "zzz,zzz,zz9.99"              NO-UNDO.
DEF VAR l-ticketpostdia  AS DECI FORMAT "zzz,zzz,zz9.99"              NO-UNDO.
DEF VAR l-ticketpostant  AS DECI FORMAT "zzz,zzz,zz9.99"              NO-UNDO.
DEF VAR l-reminormales   AS DECI FORMAT "zzz,zzz,zz9.99"              NO-UNDO.
DEF VAR l-remipostdia    AS DECI FORMAT "zzz,zzz,zz9.99"              NO-UNDO.
DEF VAR l-remipostant    AS DECI FORMAT "zzz,zzz,zz9.99"              NO-UNDO.
DEF VAR l-dev      LIKE l-ventas LABEL "DEVOLUCION"                   NO-UNDO.
DEF VAR l-facturas LIKE l-ventas LABEL "FACTURAS"                     NO-UNDO.
DEF VAR l-retiros        LIKE CorteCaja.Declaracion FORMAT '$ZZ,ZZZ,ZZ9.99'.
DEF VAR l-RetiroVale     LIKE CorteCaja.Declaracion FORMAT '$ZZ,ZZZ,ZZ9.99'.
DEF VAR l-RetiroVoucher  LIKE CorteCaja.Declaracion FORMAT '$ZZ,ZZZ,ZZ9.99'.
DEF VAR l-RetiroCheque   LIKE CorteCaja.Declaracion FORMAT '$ZZ,ZZZ,ZZ9.99'.
DEF VAR l-efectivo       LIKE CorteCaja.Declaracion FORMAT '$ZZ,ZZZ,ZZ9.99'.
DEF VAR l-pago           LIKE CorteCaja.Declaracion FORMAT '$ZZ,ZZZ,ZZ9.99'.
DEF VAR l-ncr            LIKE CorteCaja.Declaracion FORMAT '$ZZ,ZZZ,ZZ9.99'.
DEF VAR l-declaracion    LIKE CorteCaja.Declaracion FORMAT '$ZZ,ZZZ,ZZ9.99'.
DEF VAR l-tot            LIKE CorteCaja.Declaracion.
DEF VAR l-iva            LIKE CorteCaja.Declaracion.
DEF VAR l-encontro       AS LOGICAL.
DEF VAR l-eti            AS CHAR FORMAT 'X(5)'.
DEFINE VARIABLE l-Gonvill AS INTEGER NO-UNDO FORMAT 'ZZZ,ZZ9'.
DEFINE VARIABLE l-EncaGonvill AS CHARACTER NO-UNDO  FORMAT 'x(26)'  INITIAL "VALES GONVILL:            ".
DEFINE VARIABLE l-redo   LIKE remision.redo NO-UNDO.

DEF NEW SHARED VAR s-comando  AS CHAR  NO-UNDO.

DEF WORKFILE w-dev
       FIELD Tipo      AS INTEGER
       FIELD Id-Dev    LIKE Devolucion.Id-Dev
       FIELD Factura   LIKE Factura.Id-Factura
       FIELD Ncr       LIKE Ncr.id-ncr
       FIELD Monto     LIKE Factura.Tot
       FIELD Usuario   AS CHAR
       FIELD Esp       AS LOGICAL.

DEF WORKFILE w-Tipo
    FIELD Id-tp  LIKE TipoPago.Id-tp
    FIELD Monto  AS DECIMAL.

FORM
    "ABASTECEDORA DE OFICINAS, SA DE CV"                SKIP
    CtlCaja.Id-Caja LABEL "CAJA NO."  SPACE(6)
    CtlCaja.Turno   LABEL "TURNO"               SKIP
    "CORTE DE CAJA DEL"
    CtlCaja.FecOper NO-LABEL
    l-time NO-LABEL SKIP
    "===================================="
WITH FRAME f-uno OVERLAY NO-BOX SIDE-LABEL WIDTH 38 COLUMN 2.

FORM
    TipoPago.Descr  FORMAT "x(11)" COLUMN-LABEL " "
    l-pago          COLUMN-LABEL "MAQ."         FORMAT "$-zzz,zz9.99"
    l-declaracion   COLUMN-LABEL "CAJERO(A)" FORMAT "$-zzz,zz9.99"
WITH FRAME f-tres OVERLAY NO-BOX WIDTH 38 DOWN.

FORM
    SKIP(10)
    "ABASTECEDORA DE OFICINAS, SA DE CV"                SKIP
    CtlCaja.FecOper NO-LABEL
    l-time NO-LABEL
    "C" CtlCaja.Id-Caja FORMAT ">>9" NO-LABEL
    "T" CtlCaja.Turno   FORMAT "9"   NO-LABEL           SKIP
    l-enca NO-LABEL AT 1
    "===================================="
WITH FRAME f-enca2 OVERLAY SIDE-LABEL WIDTH 38 NO-BOX COLUMN 2.

FORM
    MovCaja.Folio         COLUMN-LABEL "OP."
    /* l-eti                 NO-LABEL */
    l-hora                
    MovCaja.Referencia    COLUMN-LABEL "FOLIO"
    Remision.Id-Vendedor  COLUMN-LABEL "VEN"
    MovCaja.TotVenta      COLUMN-LABEL "TOTAL" FORMAT "-zzz,zz9.99"
WITH FRAME f-cinco OVERLAY WIDTH 38 NO-BOX DOWN COLUMN 2.

FORM
    MovCaja.Folio         COLUMN-LABEL "OP."
    /* "TIC" */
    l-hora
    MovCaja.Referencia    COLUMN-LABEL "FOLIO"
    Remision.Id-Vendedor  COLUMN-LABEL "VEN"
    MovCaja.TotVenta      COLUMN-LABEL "TOTAL" FORMAT "zzzz,zz9.99"
WITH FRAME f-ticketpost OVERLAY WIDTH 38 NO-BOX DOWN COLUMN 2.

FORM
    MovCaja.Folio         COLUMN-LABEL "OP."
    /* "TIC" */
    l-hora              
    MovCaja.Referencia    COLUMN-LABEL "FOLIO"
    Remision.Id-Vendedor  COLUMN-LABEL "VEN"
    MovCaja.TotVenta      COLUMN-LABEL "TOTAL" FORMAT "zzzz,zz9.99"
WITH FRAME f-ticketpostAnt OVERLAY WIDTH 38 NO-BOX DOWN COLUMN 2.

FORM
    MovCaja.Folio         COLUMN-LABEL "OP."
    /* "FAC" */
    l-hora
    MovCaja.Referencia    COLUMN-LABEL "FOLIO"
    Factura.Id-Vendedor  COLUMN-LABEL "VEN"
    MovCaja.TotVenta      COLUMN-LABEL "TOTAL" FORMAT "zzzz,zz9.99"
WITH FRAME f-seis OVERLAY WIDTH 38 NO-BOX DOWN COLUMN 2.

FORM
    MovCaja.Folio        COLUMN-LABEL 'FOLIO'
    /* l-eti                NO-LABEL */
    l-hora
    MovCaja.Referencia   COLUMN-LABEL "REFER"
    Remision.Id-Vendedor COLUMN-LABEL 'VEN'
    MovCaja.TotVenta     COLUMN-LABEL 'TOTAL' FORMAT "-zzz,zz9.99"
WITH FRAME f-siete OVERLAY WIDTH 38 NO-BOX DOWN COLUMN 2.

FORM
    MovCaja.Folio        COLUMN-LABEL 'FOLIO'
    /* l-eti                NO-LABEL */
    l-hora
    MovCaja.Referencia   COLUMN-LABEL "REFER"
    Remision.Id-Vendedor COLUMN-LABEL 'VEN'
    MovCaja.TotVenta     COLUMN-LABEL 'TOTAL' FORMAT "-zzz,zz9.99"
WITH FRAME f-remision OVERLAY WIDTH 38 NO-BOX DOWN COLUMN 2.

FORM
    MovCaja.Folio        COLUMN-LABEL 'FOLIO'
    /* l-eti                NO-LABEL */
    l-hora
    MovCaja.Referencia   COLUMN-LABEL "REFER"
    Remision.Id-Vendedor COLUMN-LABEL 'VEN'
    MovCaja.TotVenta     COLUMN-LABEL 'TOTAL' FORMAT "-zzz,zz9.99"
WITH FRAME f-remipostant OVERLAY WIDTH 38 NO-BOX DOWN COLUMN 2.

FORM
    w-dev.Id-Dev                COLUMN-LABEL 'FOLIO' FORMAT '999999'
    w-dev.factura               COLUMN-LABEL 'REF'   FORMAT 'X(7)'
    w-dev.ncr                   COLUMN-LABEL 'NCR'   FORMAT 'X(7)'
    w-dev.Monto                 COLUMN-LABEL 'MONTO' FORMAT 'ZZ,ZZ9.99'
    WITH FRAME f-ocho OVERLAY WIDTH 38 NO-BOX DOWN COLUMN 2 NO-LABELS.

FORM
    w-dev.Usuario               LABEL 'AUT'  FORMAT 'X(8)' AT 8
    WITH FRAME f-autoriza WIDTH 30 NO-BOX DOWN COLUMN 2 SIDE-LABELS.
    
FORM    
    MovCaja.Folio         COLUMN-LABEL "OP."
    l-Hora                COLUMN-LABEL "Hora"
    MovCaja.Referencia    COLUMN-LABEL "FOLIO" FORMAT 'x(6)'
    MovCaja.Id-Cajero     COLUMN-LABEL "VEN" FORMAT '9999' 
    MovCaja.TotVenta      COLUMN-LABEL "TOTAL" FORMAT "zzzz,zz9.99"
WITH FRAME f-dconcentra OVERLAY WIDTH 38 NO-BOX DOWN COLUMN 2.    
