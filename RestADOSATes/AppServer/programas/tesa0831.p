/*
  Empresa    : Consultoria en Informatica Ejecutiva S.A. de C.V.
  Modulo     : tesoreria
  Programa   : tesa0831.p
  Funcion    : Imprime un Corte de Caja
  Usado por  : tesa0830.p
  Autor      : LUIS
  Fecha      : 13/02/97
    
  Modificacion: ALEX  | 24AGO2016 | Agregar al corte expreso los vales, vouchers y cheques
                ALEX  | 28JUL2017 | Agregar devoluciones de TC y separarlas de las devoluciones de efectivo
*/

//{sia00000.var}
DEF INPUT PARAMETER l-turno LIKE CtlCaja.Turno                          NO-UNDO.
DEF INPUT PARAMETER l-caja  LIKE CtlCaja.Id-Caja                        NO-UNDO.
DEF INPUT PARAMETER l-fecoper LIKE CtlCaja.FecOper                      NO-UNDO.
DEF INPUT PARAMETER l-usuario AS CHAR NO-UNDO.
DEF INPUT PARAMETER l-impresora AS CHAR                                 NO-UNDO.
DEFINE OUTPUT PARAMETER l-Respuesta AS CHAR NO-UNDO.
DEF BUFFER b-Mov FOR MovCaja.
DEF BUFFER b-MovCaja FOR MovCaja.
DEF BUFFER b-Remision FOR Remision.
DEF VAR l-anticipo AS DECIMAL.

DEF VAR v-nomimp    AS  CHARACTER                                       NO-UNDO.

DEF VAR l-ValCtes AS DECIMAL NO-UNDO.
DEF VAR l-ValEmpEf AS DECIMAL NO-UNDO.
DEF VAR l-ValEmpNo AS DECIMAL NO-UNDO.
DEF VAR l-MontoEf AS DECIMAL NO-UNDO.

DEFINE VARIABLE l-TotVale AS DECIMAL NO-UNDO.
DEFINE VARIABLE l-TotVoucher AS DECIMAL NO-UNDO.  
DEFINE VARIABLE l-TotCheque AS DECIMAL NO-UNDO.
DEFINE VARIABLE l-TotDevTC AS DECIMAL NO-UNDO.

DEFINE VARIABLE l-SiVale AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE g-Origen AS CHARACTER NO-UNDO.

/* declaracion de variables y formas */
{programas/tesa0834.i}  
 MESSAGE "Tablas liberadas para acuse:" g-Origen VIEW-AS ALERT-BOX.

ASSIGN l-reporte = "te" + STRING(TIME,"HH:MM:SS") + ".lst" .
DO ON ENDKEY UNDO,LEAVE ON ERROR UNDO,LEAVE:
   UNIX SILENT VALUE("umask 000").
   OUTPUT STREAM s-salida TO VALUE(l-reporte).
   /*
   PUT STREAM s-salida CONTROL CHR(27) + "@".

   PUT STREAM s-salida CONTROL CHR(27) + "d#".
   PUT STREAM s-salida CONTROL CHR(27) + CHR(15).
   */
   
   PUT STREAM s-salida CONTROL CHR(27) + CHR(64).        /*  Inicializa */
   PUT STREAM s-salida CONTROL CHR(27) + CHR(77) + "0".
   
    
   FIND FIRST CtlCaja where CtlCaja.Turno    = l-turno    AND
                            CtlCaja.Id-Caja  = l-caja     AND
                            CtlCaja.FecOper  = l-fecOper NO-LOCK NO-ERROR.
   ASSIGN l-time = STRING(TIME,"HH:MM:SS").
   DISPLAY STREAM s-salida
        CtlCaja.Turno
        CtlCaja.Id-Caja
        l-time
        CtlCaja.FecOper
   WITH FRAME f-uno.
   DISPLAY STREAM s-salida
        CtlCaja.FolioIni LABEL "FOLIO INICIAL" SKIP
        CtlCaja.FolioFin LABEL "  FOLIO FINAL" SKIP(1)
   WITH FRAME f-dos OVERLAY SIDE-LABEL COL 3 WIDTH 38.

   l-ValCtes = 0.
   l-ValEmpEf = 0.
   l-ValEmpNo = 0.
   l-TotVale = 0.
   l-TotVoucher = 0.
   l-TotCheque = 0.
   
   FOR EACH MovCaja WHERE MovCaja.Turno = CtlCaja.Turno AND
                          MovCaja.Id-Caja = CtlCaja.Id-caja AND
                          MovCaja.Fecoper = CtlCaja.Fecoper NO-LOCK:
       l-SiVale = FALSE.
       IF MovCaja.TipoVenta = 2 THEN DO:
          FIND Remision WHERE Remision.Id-Remision = MovCaja.Referencia 
               NO-LOCK NO-ERROR.
          IF AVAILABLE Remision AND Remision.SustIdRemision <> "" THEN DO:
             FIND b-Remision 
                  WHERE b-Remision.Id-Remision = Remision.SustIdRemision
                  NO-LOCK NO-ERROR.
             IF AVAILABLE b-Remision THEN DO:
                FIND b-MovCaja WHERE b-MovCaja.Refer = b-Remision.Id-Remision
                                 AND b-MovCaja.TipoVenta = b-Remision.TipoVenta
                                 NO-LOCK NO-ERROR.
                IF AVAILABLE b-MovCaja AND b-MovCaja.FecOper <> MovCaja.FecOper
                THEN NEXT.
                /*ELSE IF NOT AVAILABLE b-MovCaja AND b-Remision.FecReg <> MovCaja.FecOper THEN NEXT.*/
             END.
          END.
          ELSE IF AVAILABLE Remision AND Remision.PorIdRemision <> "" THEN DO:
             FIND b-Remision 
                  WHERE b-Remision.Id-Remision = Remision.PorIdRemision
                  NO-LOCK NO-ERROR.
             IF AVAILABLE b-Remision THEN DO:
                FIND b-MovCaja WHERE b-MovCaja.Refer = b-Remision.Id-Remision
                                 AND b-MovCaja.TipoVenta = b-Remision.TipoVenta
                                 NO-LOCK NO-ERROR.
                IF AVAILABLE b-MovCaja AND b-MovCaja.FecOper <> MovCaja.FecOper
                THEN l-SiVale = TRUE.
             END.
          END.
       END.
       IF (MovCaja.Canc AND NOT l-SiVale) THEN NEXT.
    
       IF MovCaja.TipoVenta < 5 OR MovCaja.TipoVenta = 7 THEN
          FOR EACH DetMovC WHERE DetMovC.Id-caja = MovCaja.Id-Caja AND
                   DetMovC.Folio = MovCaja.Folio NO-LOCK:
              IF DetMovC.Mov = 'P' THEN DO:
                 FIND FIRST w-Tipo WHERE w-Tipo.Id-tp = DetMovC.Id-tp
                      NO-ERROR.
                 IF NOT AVAILABLE w-Tipo THEN DO:
                    CREATE w-Tipo.
                    ASSIGN w-Tipo.Id-Tp = DetMovC.Id-tp.
                 END.
                 
                 ASSIGN w-Tipo.Monto = w-Tipo.Monto + DetMovC.MontoPAGO.
                 IF DetMovC.Id-tp = 52 OR DetMovC.Id-tp = 62 THEN
                     ASSIGN
                         l-TotVoucher = l-TotVoucher + DetMovC.MontoPago.
                 IF DetMovC.Id-tp = 61 THEN
                     ASSIGN
                         l-TotCheque = l-TotCheque + DetMovC.MontoPago.                         
              END.
          END.
       IF MovCaja.TipoVenta < 3 THEN DO:
           FIND Remision WHERE Remision.Id-Remision = MovCaja.Referencia NO-LOCK NO-ERROR.
           IF AVAILABLE Remision THEN
               FOR EACH DetRemis WHERE DetRemis.Id-Remision = MovCaja.Refer
                                   AND DetRemis.Tipo = 6
                                   NO-LOCK:
                   IF DetRemis.Descr BEGINS 'VALE' THEN DO:
                      IF Remision.Id-Nomina = 0 THEN
                         l-ValCtes = l-ValCtes + ABS(ROUND(DetRemis.Importe + DetRemis.Iva,2)).
                      ELSE IF DetRemis.Importe = 0 THEN DO:
                         FIND Vale WHERE Vale.Id-Vale = SUBSTRING(DetRemis.Descr,8,12) NO-LOCK NO-ERROR.
                         IF AVAILABLE Vale THEN
                            l-ValEmpEf = l-ValEmpEf + Vale.Importe.
                      END.
                      ELSE l-ValEmpNo = l-ValEmpNo + ABS(DetRemis.Importe + DetRemis.Iva).
                   END.
               END.
       END.
   END.
   l-TotVale = l-ValCtes + l-ValEmpEf + l-ValEmpNo.

   l-MontoEf = 0.
   FOR EACH TipoPago WHERE TipoPago.Id-TP = 60 NO-LOCK BY TipoPago.Id-tp  :
       FIND FIRST w-Tipo WHERE w-Tipo.Id-Tp = TipoPago.Id-Tp NO-ERROR.
       FIND FIRST CorteCaja WHERE
                  CorteCaja.Id-Caja = CtlCaja.Id-Caja AND
                  CorteCaja.Turno   = CtlCaja.Turno   AND
                  CorteCaja.FecOper = CtlCaja.FecOper AND
                  CorteCaja.Id-Tp   = TipoPago.id-Tp  NO-LOCK NO-ERROR.

        /* Calcula los pagos de contado por forma de pago */
        ASSIGN l-pago = IF AVAILABLE w-Tipo THEN w-Tipo.Monto ELSE 0.
        IF l-Pago = 0 THEN NEXT.
        IF TipoPago.Descr MATCHES '*EFECTIVO*' AND AVAILABLE CorteCaja THEN
            ASSIGN l-declaracion = CorteCaja.Declaracion.
        IF NOT TipoPago.Descr MATCHES '*EFECTIVO*' THEN
            ASSIGN l-declaracion = l-pago.

        DISPLAY STREAM s-salida
              TipoPago.Descr WHEN AVAILABLE TipoPago
              l-pago
              l-declaracion WITH FRAME f-tres.
        DOWN STREAM s-salida WITH FRAME f-tres.
   END.
   FOR EACH TipoPago NO-LOCK BY TipoPago.Id-tp  :
       FIND FIRST w-Tipo WHERE w-Tipo.Id-Tp = TipoPago.Id-Tp NO-ERROR.
       FIND FIRST CorteCaja WHERE
                  CorteCaja.Id-Caja = CtlCaja.Id-Caja AND
                  CorteCaja.Turno   = CtlCaja.Turno   AND
                  CorteCaja.FecOper = CtlCaja.FecOper AND
                  CorteCaja.Id-Tp   = TipoPago.id-Tp  NO-LOCK NO-ERROR.

        /* Calcula los pagos de contado por forma de pago */
        ASSIGN l-pago = IF AVAILABLE w-Tipo THEN w-Tipo.Monto ELSE 0.
        IF l-Pago = 0 THEN NEXT.
        IF TipoPago.Descr MATCHES '*EFECTIVO*' THEN
           l-MontoEf = l-MontoEf + l-Pago.
        IF TipoPago.Descr MATCHES '*EFECTIVO*' AND AVAILABLE CorteCaja THEN
            ASSIGN l-declaracion = CorteCaja.Declaracion.
        IF NOT TipoPago.Descr MATCHES '*EFECTIVO*' THEN
            ASSIGN l-declaracion = l-pago.

        ACCUMULATE l-pago (TOTAL).
        ACCUMULATE l-declaracion (TOTAL).

        IF TipoPago.Id-TP = 60 THEN NEXT.
        DISPLAY STREAM s-salida
              TipoPago.Descr WHEN AVAILABLE TipoPago
              l-pago
              l-declaracion WITH FRAME f-tres.
        DOWN STREAM s-salida WITH FRAME f-tres.
   END.

   /*****************************************/
   /* DEVOLUCIONES DE EFECTIVO              */
   /*****************************************/
   ASSIGN 
       l-TotDevTC = 0.
   FOR EACH MovCaja WHERE
            MovCaja.id-Caja = CtlCaja.Id-Caja AND
            MovCaja.Turno   = CtlCaja.Turno   AND
            MovCaja.FecOper = CtlCaja.Fecoper AND
            (MovCaja.tipoVenta = 4 OR
             MovCaja.tipoVenta = 8) 
            NO-LOCK:
        IF MovCaja.Canc THEN NEXT.
        FIND FIRST DetMovC WHERE DetMovC.Id-Caja = MovCaja.Id-Caja
                             AND DetMovC.Folio = MovCaja.Folio
                             AND DetMovC.Mov = "D"
                             AND DetMovC.Plazo = 1
                             AND DetMovC.TC = 1 NO-LOCK NO-ERROR.
        IF NOT AVAILABLE DetMovC THEN 
            ACCUMULATE MovCaja.TotVenta (TOTAL).
        ELSE 
            ASSIGN 
                l-TotDevTC = l-TotDevTC + MovCaja.TotVenta.
   END.
   FOR EACH MovCaja WHERE
            MovCaja.id-Caja = CtlCaja.Id-Caja AND
            MovCaja.Turno   = CtlCaja.Turno   AND
            MovCaja.FecOper = CtlCaja.Fecoper AND
            MovCaja.tipoVenta = 10 NO-LOCK:
        IF MovCaja.Canc THEN NEXT.
        ASSIGN 
            l-TotDevTC = l-TotDevTC + MovCaja.TotVenta.
   END.

   ASSIGN l-ncr        = (ACCUM TOTAL MovCaja.TotVenta)
          l-MontoEf    = l-MontoEf - l-ncr
          l-declaracion = l-pago.

   IF l-ncr <> 0 THEN DO:
       DISPLAY STREAM s-salida
            'DEV.EFECTIVO' @ TipoPago.Descr
             l-ncr @ l-pago
             WITH FRAME f-tres.
       DOWN STREAM s-salida WITH FRAME f-tres.
   END.
    IF l-TotDevTC <> 0 THEN DO:
        DISPLAY STREAM s-salida
            'DEV. TC.' @ TipoPago.Descr
            l-TotDevTC @ l-pago
        WITH FRAME f-tres.
        DOWN STREAM s-salida WITH FRAME f-tres.
    END.
   IF l-ValEmpEf > 0 THEN DO:
       DISPLAY STREAM s-salida
            'VALE EMP EFE' @ TipoPago.Descr
             l-ValEmpEf @ l-pago
             WITH FRAME f-tres.
       DOWN STREAM s-salida WITH FRAME f-tres.
       l-MontoEf = l-MontoEf - l-ValEmpEf.
   END.
   IF l-ValEmpNo > 0 THEN DO:
       DISPLAY STREAM s-salida
            'VALE EMP SIN' @ TipoPago.Descr
             l-ValEmpNo @ l-pago
             WITH FRAME f-tres.
       DOWN STREAM s-salida WITH FRAME f-tres.
   END.
   IF l-ValCtes > 0 THEN DO:
       DISPLAY STREAM s-salida
            'VALE CLIENTE' @ TipoPago.Descr
             l-ValCtes @ l-pago
             WITH FRAME f-tres.
       DOWN STREAM s-salida WITH FRAME f-tres.
   END.


   /*****************************************/
   /* OTRAS NCR                             */
   /*****************************************/
/*
   FOR EACH MovCaja WHERE
            MovCaja.id-Caja = CtlCaja.Id-Caja AND
            MovCaja.Turno   = CtlCaja.Turno   AND
            MovCaja.FecOper = CtlCaja.Fecoper AND
            MovCaja.tipoVenta = 8 NO-LOCK:
        IF MovCaja.Canc THEN NEXT.
        ACCUMULATE MovCaja.TotVenta (TOTAL).
   END.
   ASSIGN l-ncr        = (ACCUM TOTAL MovCaja.TotVenta)
          l-declaracion = l-ncr.

   DISPLAY STREAM s-salida
        'OTROS' @ TipoPago.Descr
         l-ncr @ l-pago
         l-declaracion WITH FRAME f-tres.
   DOWN STREAM s-salida WITH FRAME f-tres.
*/

   UNDERLINE STREAM s-salida
        l-pago
        l-declaracion WITH FRAME f-tres.

   DISPLAY STREAM s-salida
        ((ACCUM TOTAL l-pago) - l-ncr - l-TotDevTC + l-ValEmpEf + l-ValEmpNo + l-ValCtes) @ l-pago
        (ACCUM TOTAL l-declaracion) - l-ncr @ l-declaracion
        WITH FRAME f-tres.
   
   ASSIGN
       l-retiros = 0
       l-RetiroVale = 0
       l-RetiroVoucher = 0
       l-RetiroCheque = 0.
             
   DISPLAY STREAM s-salida
           SKIP
   WITH FRAME f-r.
   
   /* RETIROS DE EFECTIVO Y DOCUMENTOS */
   /* EFECTIVO */
   FOR EACH MovCaja WHERE MovCaja.turno = CtlCaja.turno
                      AND MovCaja.id-caja = CtlCaja.Id-Caja
                      AND MovCaja.FecOper = CtlCaja.FecOper 
                      AND MovCaja.TipoVenta = 6
                      AND MovCaja.FolioAut = 0 NO-LOCK:
        IF MovCaja.Canc THEN NEXT.
        ASSIGN l-efectivo = 0.
        FOR EACH b-Mov WHERE b-Mov.turno = MovCaja.Turno AND
                             b-Mov.id-caja = MovCaja.Id-caja AND
                             b-Mov.FecOper = MovCaja.FecOper AND
                             b-Mov.Folio   < MovCaja.Folio NO-LOCK.
            IF b-Mov.Canc THEN NEXT.                           
            IF b-Mov.TipoVenta = 1 OR b-Mov.TipoVenta = 2 THEN 
                FOR EACH detMovC WHERE detMovC.Id-Caja = b-Mov.Id-Caja
                                   AND detMovC.Folio = b-Mov.Folio 
                                   AND detMovC.Id-TP = 60 NO-LOCK:
                    ASSIGN l-efectivo = l-efectivo + detMovC.MontoPAGO. 
                END.
            IF b-Mov.TipoVenta = 6 AND b-Mov.FolioAut = 0 THEN
                ASSIGN l-efectivo = l-efectivo - b-Mov.TotVenta.                
        END.                                
        ASSIGN l-retiros = l-retiros + MovCaja.TotVenta.       
        DISPLAY STREAM s-salida            
            "CONC. EFE" MovCaja.Referencia FORMAT '99'     
            MovCaja.TotVenta     FORMAT "$-zzz,zz9.99"
            (l-efectivo /*- l-retiros*/ )   FORMAT "$-zzz,zz9.99"
        WITH FRAME f-retiros NO-LABELS OVERLAY NO-BOX WIDTH 38 DOWN.          
   END.
   DISPLAY STREAM s-salida         
       (l-MontoEf - l-retiros) FORMAT "$-ZZZZ,ZZ9.99" LABEL "EF FALTANTE"
       SKIP(1)
   WITH FRAME f-faltantes OVERLAY SIDE-LABEL.
   /* VALES */
   l-RetiroVale = 0.
   FOR EACH MovCaja WHERE MovCaja.turno = CtlCaja.turno
                      AND MovCaja.id-caja = CtlCaja.Id-Caja
                      AND MovCaja.FecOper = CtlCaja.FecOper 
                      AND MovCaja.TipoVenta = 6
                      AND MovCaja.FolioAut = 1 NO-LOCK:
        IF MovCaja.Canc THEN NEXT.
        ASSIGN l-RetiroVale = l-RetiroVale + MovCaja.TotVenta.       
        DISPLAY STREAM s-salida            
            "CONC. VAL"
            MovCaja.Referencia              FORMAT '99'     
            MovCaja.TotVenta           FORMAT "$-zzz,zz9.99"
            (l-TotVale - l-RetiroVale + MovCaja.TotVenta) FORMAT "$-zzz,zz9.99"
        WITH FRAME f-RetirosVale NO-LABELS OVERLAY NO-BOX WIDTH 38 DOWN.          
   END.
   DISPLAY STREAM s-salida
       (l-TotVale - l-RetiroVale) FORMAT "$-ZZZZ,ZZ9.99" LABEL "VA FALTANTE"
       SKIP(1)
   WITH FRAME f-faltantesVale OVERLAY SIDE-LABEL.
   /* VOUCHERS */
   l-RetiroVoucher = 0.
   FOR EACH MovCaja WHERE MovCaja.turno = CtlCaja.turno
                      AND MovCaja.id-caja = CtlCaja.Id-Caja
                      AND MovCaja.FecOper = CtlCaja.FecOper 
                      AND MovCaja.TipoVenta = 6
                      AND MovCaja.FolioAut = 2 NO-LOCK:
        IF MovCaja.Canc THEN NEXT.
        ASSIGN l-RetiroVoucher = l-RetiroVoucher + MovCaja.TotVenta.       
        DISPLAY STREAM s-salida            
            "CONC. VOU"
            MovCaja.Referencia              FORMAT '99'     
            MovCaja.TotVenta           FORMAT "$-zzz,zz9.99"
            (l-TotVoucher - l-RetiroVoucher + MovCaja.TotVenta) FORMAT "$-zzz,zz9.99"
        WITH FRAME f-RetirosVoucher NO-LABELS OVERLAY NO-BOX WIDTH 38 DOWN.          
   END.
   DISPLAY STREAM s-salida
       (l-TotVoucher - l-RetiroVoucher) FORMAT "$-ZZZZ,ZZ9.99" LABEL "VO FALTANTE"
       SKIP(1)
   WITH FRAME f-faltantesVoucher OVERLAY SIDE-LABEL.
   /* CHEQUES */
   l-RetiroCheque = 0.
   FOR EACH MovCaja WHERE MovCaja.turno = CtlCaja.turno
                      AND MovCaja.id-caja = CtlCaja.Id-Caja
                      AND MovCaja.FecOper = CtlCaja.FecOper 
                      AND MovCaja.TipoVenta = 6
                      AND MovCaja.FolioAut = 3 NO-LOCK:
        IF MovCaja.Canc THEN NEXT.
        ASSIGN l-RetiroCheque = l-RetiroCheque + MovCaja.TotVenta.       
        DISPLAY STREAM s-salida            
            "CONC. CHE"
            MovCaja.Referencia              FORMAT '99'     
            MovCaja.TotVenta           FORMAT "$-zzz,zz9.99"
            (l-TotCheque - l-RetiroCheque + MovCaja.TotVenta) FORMAT "$-zzz,zz9.99"
        WITH FRAME f-RetirosCheque NO-LABELS OVERLAY NO-BOX WIDTH 38 DOWN.          
   END.
   DISPLAY STREAM s-salida
       (l-TotCheque - l-RetiroCheque) FORMAT "$-ZZZZ,ZZ9.99" LABEL "CH FALTANTE"
       SKIP(4)
   WITH FRAME f-faltantesCheque OVERLAY SIDE-LABEL.
   /**/
    
   ASSIGN l-ticket   = 0 l-contticket   = 0
          l-facturas = 0 l-contfacturas = 0
          l-ventas   = 0 l-contventas   = 0
          l-dev      = 0 l-contdev      = 0.

   FOR EACH MovCaja WHERE MovCaja.Id-Caja = CtlCaja.Id-Caja AND
                          MovCaja.Turno   = CtlCaja.Turno   AND
                          MovCaja.FecOper = CtlCaja.FecOper NO-LOCK:
       l-SiVale = FALSE.
       IF MovCaja.TipoVenta = 2 THEN DO:
          FIND Remision WHERE Remision.Id-Remision = MovCaja.Referencia 
               NO-LOCK NO-ERROR.
          IF AVAILABLE Remision AND Remision.SustIdRemision <> "" THEN DO:
             FIND b-Remision 
                  WHERE b-Remision.Id-Remision = Remision.SustIdRemision
                  NO-LOCK NO-ERROR.
             IF AVAILABLE b-Remision THEN DO:
                FIND b-MovCaja WHERE b-MovCaja.Refer = b-Remision.Id-Remision
                                 AND b-MovCaja.TipoVenta = b-Remision.TipoVenta
                                 NO-LOCK NO-ERROR.
                IF AVAILABLE b-MovCaja AND b-MovCaja.FecOper <> MovCaja.FecOper
                THEN NEXT.
                /*ELSE IF NOT AVAILABLE b-MovCaja AND b-Remision.FecReg <> MovCaja.FecOper THEN NEXT.*/
             END.
          END.
          ELSE IF AVAILABLE Remision AND Remision.PorIdRemision <> "" THEN DO:
             FIND b-Remision 
                  WHERE b-Remision.Id-Remision = Remision.PorIdRemision
                  NO-LOCK NO-ERROR.
             IF AVAILABLE b-Remision THEN DO:
                FIND b-MovCaja WHERE b-MovCaja.Refer = b-Remision.Id-Remision
                                 AND b-MovCaja.TipoVenta = b-Remision.TipoVenta
                                 NO-LOCK NO-ERROR.
                IF AVAILABLE b-MovCaja AND b-MovCaja.FecOper <> MovCaja.FecOper
                THEN l-SiVale = TRUE.
             END.
          END.
       END.
       IF MovCaja.Canc AND NOT l-SiVale THEN NEXT.
       IF MovCaja.TipoVenta = 2 THEN
           ASSIGN l-ventas     = l-ventas + MovCaja.TotVenta
           l-contventas = l-contventas + 1.
       ELSE
           IF MovCaja.TipoVenta = 1 THEN
               ASSIGN l-ticket = l-ticket + MovCaja.TotVenta
               l-contticket = l-contticket + 1.
           ELSE
               IF MovCaja.TipoVenta = 3 THEN
                   ASSIGN l-facturas     = l-facturas + MovCaja.TotVenta
                   l-contfacturas = l-contfacturas + 1.
               ELSE
                   IF (MovCaja.TipoVenta = 9 OR MovCaja.TipoVenta = 8) THEN DO:
                       ASSIGN
                           l-dev = l-dev + MovCaja.TotVenta
                           l-contdev = l-contdev + 1.
                       
                       FIND NCR WHERE NCR.Id-Ncr = MovCaja.Referencia NO-LOCK NO-ERROR.
                       IF NOT AVAILABLE Ncr THEN NEXT.
                       FIND FIRST DetNcr OF NCR NO-LOCK NO-ERROR.
                       IF DetNCR.TipoVenta = 2 THEN
                           ASSIGN
                               l-Ventas = l-Ventas - MovCaja.TotVenta.
                       ELSE
                           ASSIGN
                               l-ticket = l-ticket - MovCaja.TotVenta.
                   END.
                   ELSE NEXT.

       IF g-Origen = '11' THEN DO:  
           ASSIGN l-Gonvill = 0.
           IF movcaja.tipoventa < 3 THEN DO:
               FOR EACH detremis WHERE detremis.id-remision = MovCaja.Referencia NO-LOCK,
                   FIRST articulo WHERE articulo.id-articulo = detremis.id-articulo
                                    AND articulo.id-marca = 2939 /*gonvill*/ NO-LOCK:
                   ASSIGN l-gonvill = l-gonvill + DetRemis.Cant.
               END.
           END.
           ELSE DO:
               FOR EACH detfactura WHERE detfactura.id-factura = MovCaja.Referencia NO-LOCK,
                   FIRST articulo WHERE articulo.id-articulo = detfactura.id-articulo
                                    AND articulo.id-marca = 2939 /*gonvill*/ NO-LOCK:
                   ASSIGN l-gonvill = l-gonvill + DetRemis.cant.
               END.
           END.
       END.
   END.

   DISPLAY STREAM s-salida
        CtlCaja.Turno
        CtlCaja.Id-Caja
        l-time
        CtlCaja.FecOper WITH FRAME f-uno.
    DISPLAY STREAM s-salida
    "                             No. OP " SKIP
    "====================================" SKIP
          l-ticket  SPACE(6)
          l-contticket NO-LABEL SKIP
          l-ventas  SPACE(6)
          l-contventas NO-LABEL SKIP
          l-facturas  SPACE(6)
          l-contfacturas NO-LABEL SKIP
          "          ============="
          SPACE(6)
          "======"
          (l-ticket + l-ventas + l-facturas) LABEL "   TOTAL" FORMAT "$zzzz,zz9.99-" SPACE(6)
          (l-contticket + l-contventas + l-contfacturas) NO-LABELS FORMAT "zz,zz9"
          SKIP(1)
          l-EncaGonvill NO-LABEL WHEN g-Origen = "11"
          l-Gonvill NO-LABEL WHEN g-Origen = "11"
    WITH FRAME f-cuatro SIDE-LABEL OVERLAY WIDTH 38 NO-BOX COLUMN 2.

    {programas/tesa0831.i &Mensaje = "DIARIO DE TICKETS"}
    ASSIGN l-encontro = FALSE
           l-tot      = 0
           l-iva      = 0
           l-redo     = 0.

    FOR EACH MovCaja WHERE MovCaja.Id-Caja   = CtlCaja.Id-Caja     AND
                           MovCaja.Turno     = CtlCaja.Turno       AND
                           MovCaja.FecOper   = CtlCaja.FecOper     NO-LOCK
                           BY (IF MovCaja.Id-caja = 66 OR MovCaja.Id-caja = 88 THEN MovCaja.Referencia
                               ELSE STRING(MovCaja.Folio,'999999')):

          IF MovCaja.TipoVenta = 1 OR MovCaja.TipoVenta = 4 OR MovCaja.TipoVenta = 8 OR
             MovCaja.TipoVenta = 9 OR MovCaja.TipoVenta = 6 /*GRACIELA PIDIO AGREGAR EL TIPO 6 JULIO/09*/ THEN DO:

             IF MovCaja.TipoVenta = 4 THEN DO:
                FIND Devolucion WHERE Devolucion.Id-Dev =
                                INT(MovCaja.Referencia) NO-LOCK NO-ERROR.
                IF Devolucion.TipoVenta <> 1 THEN NEXT.
                FIND NCR WHERE NCR.ID-NCR = Devolucion.Id-Ncr NO-LOCK NO-ERROR.
                FIND Remision WHERE Remision.Id-Remision =
                             Devolucion.Id-Factura AND Remision.TipoVenta = 1
                             NO-LOCK NO-ERROR.
             END.

             ELSE IF MovCaja.TipoVenta = 9 OR MovCaja.TipoVenta = 8 THEN DO:
                FIND NCR WHERE NCR.Id-Ncr = MovCaja.Referencia NO-LOCK NO-ERROR.
                IF NOT AVAILABLE Ncr THEN NEXT.
                FIND FIRST DetNcr OF NCR NO-LOCK NO-ERROR.
                IF DetNCR.TipoVenta <> 1 THEN NEXT.
                FIND Remision WHERE Remision.Id-Remision = DetNcr.Documento
                            AND Remision.TipoVenta = 1 NO-LOCK NO-ERROR.
             END.
             
             ELSE IF MovCaja.TipoVenta = 6 THEN DO:
                 
             END.
             ELSE
                FIND Remision WHERE Remision.Id-Remision = MovCaja.Referencia
                                AND Remision.TipoVenta = 1 NO-LOCK NO-ERROR.
             ASSIGN l-encontro = TRUE.
             {programas/tesa0832.i &Frame = "f-cinco"}
          END.
    END.

    {programas/tesa0833.i
       &Frame    = f-cincotot
       &Framepri = "f-cinco"}

    /***********************************************************************/
    /* DIARIO DE FACTURAS                                                  */
    /***********************************************************************/

    {programas/tesa0831.i &Mensaje = "DIARIO DE FACTURAS"}
    ASSIGN l-encontro = FALSE.

    FOR EACH MovCaja WHERE MovCaja.Id-Caja   = CtlCaja.Id-Caja     AND
                           MovCaja.Turno     = CtlCaja.Turno       AND
                           MovCaja.FecOper   = CtlCaja.FecOper   NO-LOCK
                           BY MovCaja.Referencia :

          IF MovCaja.TipoVenta <> 3 THEN NEXT.
          FIND Factura WHERE Factura.Id-Factura = MovCaja.Referencia
                            NO-LOCK NO-ERROR.
          ASSIGN l-hora = STRING(Factura.HorReg,"HH:MM").
          DISPLAY STREAM s-salida
                     MovCaja.Folio
                     MovCaja.Referencia
                     l-hora
                     Factura.Id-Vendedor WHEN AVAILABLE Factura
                     IF MovCaja.Canc THEN 0 ELSE Factura.Tot @ MovCaja.TotVenta
          WITH FRAME f-seis.
          ASSIGN l-encontro = TRUE.
          DOWN STREAM s-salida WITH FRAME f-seis.
          ACCUMULATE MovCaja.Folio (COUNT).
          IF MovCaja.Canc THEN NEXT.
          ACCUMULATE Factura.Tot (TOTAL).
          ACCUMULATE Factura.IVA (TOTAL).
    END.

    IF l-encontro THEN DO:
        DISPLAY STREAM s-salida
            (ACCUM TOTAL Factura.Tot) - (ACCUM TOTAL Factura.IVA)
                        LABEL "SUBTOTAL" FORMAT "zzz,zzz,zz9.99" AT 2 SKIP
            ACCUM TOTAL Factura.IVA
                        LABEL "     IVA" FORMAT "zzz,zzz,zz9.99"  AT 2
            ACCUM TOTAL Factura.Tot
                        LABEL "   TOTAL" FORMAT "zzz,zzz,zz9.99"
                        AT 2 SKIP(1)
            (ACCUM COUNT MovCaja.Folio) LABEL "CANT NOTAS"
        WITH FRAME f-seistot OVERLAY SIDE-LABEL COLUMN 5 WIDTH 38.
    END.
    ELSE DO:
        VIEW STREAM s-salida FRAME f-seis.
        DISPLAY STREAM s-salida
              "SUBTOTAL:       0.00" AT 2 SKIP
              "     IVA:       0.00" AT 2 SKIP
              "   TOTAL:       0.00" AT 2   SKIP
        WITH FRAME f-seistot2 OVERLAY SIDE-LABEL COLUMN 5 WIDTH 38.
    END.

    /***********************************************************************/
    /* DIARIO DE VENTAS DE CONTADO                                         */
    /***********************************************************************/
    {programas/tesa0831.i &mensaje = "DIARIO DE VENTAS DE CONTADO"}
    ASSIGN l-encontro     = false
           l-tot          = 0
           l-iva          = 0
           l-reminormales = 0
           l-remipostdia  = 0
           l-remipostant  = 0
           l-anticipo     = 0
           l-redo         = 0.

    FOR EACH MovCaja WHERE MovCaja.Id-Caja   = CtlCaja.Id-Caja     AND
                           MovCaja.Turno     = CtlCaja.Turno       AND
                           MovCaja.FecOper   = CtlCaja.FecOper NO-LOCK                           
                           BY (IF MovCaja.Id-caja = 66 OR MovCaja.Id-Caja = 88 THEN MovCaja.Referencia
                               ELSE STRING(MovCaja.Folio,'999999')):
       l-SiVale = FALSE.
       IF MovCaja.TipoVenta = 2 THEN DO:
          FIND Remision WHERE Remision.Id-Remision = MovCaja.Referencia 
               NO-LOCK NO-ERROR.
          IF AVAILABLE Remision AND Remision.SustIdRemision <> "" THEN DO:
             FIND b-Remision 
                  WHERE b-Remision.Id-Remision = Remision.SustIdRemision
                  NO-LOCK NO-ERROR.
             IF AVAILABLE b-Remision THEN DO:
                FIND b-MovCaja WHERE b-MovCaja.Refer = b-Remision.Id-Remision
                                 AND b-MovCaja.TipoVenta = b-Remision.TipoVenta
                                 NO-LOCK NO-ERROR.
                IF AVAILABLE b-MovCaja AND b-MovCaja.FecOper <> MovCaja.FecOper
                THEN NEXT.
                /*ELSE IF NOT AVAILABLE b-MovCaja AND b-Remision.FecReg <> MovCaja.FecOper THEN NEXT.*/
             END.
          END.
          ELSE IF AVAILABLE Remision AND Remision.PorIdRemision <> "" THEN DO:
             FIND b-Remision 
                  WHERE b-Remision.Id-Remision = Remision.PorIdRemision
                  NO-LOCK NO-ERROR.
             IF AVAILABLE b-Remision THEN DO:
                FIND b-MovCaja WHERE b-MovCaja.Refer = b-Remision.Id-Remision
                                 AND b-MovCaja.TipoVenta = b-Remision.TipoVenta
                                 NO-LOCK NO-ERROR.
                IF AVAILABLE b-MovCaja AND b-MovCaja.FecOper <> MovCaja.FecOper
                THEN l-SiVale = TRUE.
             END.
          END.
       END.

          IF MovCaja.TipoVenta = 2 OR MovCaja.TipoVenta = 4 OR MovCaja.TipoVenta = 8 OR 
             MovCaja.TipoVenta = 9 OR MovCaja.TipoVenta = 6 /*gRACIELA PIDIO AGREGAR EL TIPO 6 JULIO/09*/ THEN DO:

             IF MovCaja.TipoVenta = 4 THEN DO:
                FIND Devolucion WHERE Devolucion.Id-Dev =
                                INT(MovCaja.Referencia) NO-LOCK NO-ERROR.
                IF Devolucion.TipoVenta <> 2 THEN NEXT.
                FIND NCR WHERE NCR.ID-NCR = Devolucion.Id-Ncr NO-LOCK NO-ERROR.
                FIND Remision WHERE Remision.Id-Remision =
                             Devolucion.Id-Factura AND Remision.TipoVenta = 2
                             NO-LOCK NO-ERROR.
             END.

             ELSE IF MovCaja.TipoVenta = 9 OR MovCaja.TipoVenta = 8 THEN DO:
                FIND NCR WHERE NCR.Id-Ncr = MovCaja.Referencia NO-LOCK NO-ERROR.
                IF NOT AVAILABLE Ncr THEN NEXT.
                FIND FIRST DetNcr OF NCR NO-LOCK NO-ERROR.
                IF DetNCR.TipoVenta <> 2 THEN NEXT.
                FIND Remision WHERE Remision.Id-Remision = DetNcr.Documento
                            AND Remision.TipoVenta = 2 NO-LOCK NO-ERROR.
                FIND FIRST B-Mov WHERE
                           B-Mov.Referencia = Remision.Id-Remision AND
                           B-Mov.TipoVenta = 2 NO-LOCK NO-ERROR.
                IF AVAILABLE B-Mov AND B-Mov.FecOper < B-Mov.FecDep THEN NEXT.
             END.

             ELSE IF MovCaja.TipoVenta = 6 THEN DO:
                 
             END.
             
             ELSE DO:
                FIND Remision WHERE Remision.Id-Remision = MovCaja.Referencia
                                AND Remision.TipoVenta = 2 NO-LOCK NO-ERROR.
                IF MovCaja.FecOper < MovCaja.FecDep THEN NEXT.
                FOR EACH DetMovC WHERE
                         DetMovC.Id-Caja = MovCaja.Id-Caja AND
                         DetMovC.Folio = MovCaja.Folio AND
                         DetMovC.Mov   = 'A' NO-LOCK :
                    ASSIGN l-anticipo  = l-anticipo + DetMovC.MontoPago.
                END.
             END.

             ASSIGN l-encontro = TRUE.
             {programas/tesa0832.i &Frame = "f-siete"}
             IF (MovCaja.Canc AND NOT l-SiVale) OR MovCaja.TipoVenta = 6 THEN NEXT.
             ASSIGN l-reminormales = l-reminormales + IF MovCaja.TipoVenta = 2
                                     THEN MovCaja.TotVenta ELSE
                                         (MovCaja.TotVenta * -1).

          END.
    END.

    {programas/tesa0833.i
       &Framepri = "f-siete"
       &Frame    = "f-sietetot" }


    /***********************************************************************/
    /* DIARIO DE VENTAS DE CONTADO POSTFECHADAS DEL DIA                    */
    /***********************************************************************/

    {programas/tesa0831.i &Mensaje = "DIARIO VTAS CONTADO POSTFEC. DEL DIA"}
    ASSIGN l-encontro = false
           l-tot      = 0
           l-iva      = 0.

    FOR EACH MovCaja WHERE MovCaja.Id-Caja   = CtlCaja.Id-Caja     AND
                           MovCaja.Turno     = CtlCaja.Turno       AND
                           MovCaja.FecOper   = CtlCaja.FecOper     NO-LOCK
                           BY MovCaja.Referencia :

          IF MovCaja.TipoVenta = 2 OR MovCaja.TipoVenta = 9 OR MovCaja.TipoVenta = 8 THEN DO:
             IF MovCaja.TipoVenta = 9 OR MovCaja.TipoVenta = 8 THEN DO:
                FIND NCR WHERE NCR.Id-Ncr = MovCaja.Referencia NO-LOCK NO-ERROR.
                IF NOT AVAILABLE Ncr THEN NEXT.
                FIND FIRST DetNcr OF NCR NO-LOCK NO-ERROR.
                IF DetNCR.TipoVenta <> 2 THEN NEXT.
                FIND Remision WHERE Remision.Id-Remision = DetNcr.Documento
                            AND Remision.TipoVenta = 2 NO-LOCK NO-ERROR.
                IF Remision.FacGlobal <> "" THEN NEXT.
                FIND FIRST B-Mov WHERE
                           B-Mov.Referencia = Remision.Id-Remision AND
                           B-Mov.TipoVenta = 2 NO-LOCK NO-ERROR.
                IF AVAILABLE B-Mov AND B-Mov.FecOper >= B-Mov.FecDep THEN NEXT.
             END.

             ELSE DO:
                FIND Remision WHERE Remision.Id-Remision = MovCaja.Referencia
                                AND Remision.TipoVenta = 2 NO-LOCK NO-ERROR.
                IF Remision.Facglobal <> "" then NEXT.
                IF MovCaja.FecOper >= MovCaja.FecDep THEN NEXT.
             END.
             ASSIGN l-encontro = TRUE.
             {programas/tesa0832.i &Frame = "f-remision"}
             IF MovCaja.Canc THEN NEXT.
             ASSIGN l-remipostdia = l-remipostdia + IF MovCaja.TipoVenta = 2
                                     THEN MovCaja.TotVenta ELSE
                                         (MovCaja.TotVenta * -1).
          END.
    END.

    {programas/tesa0833.i
       &Framepri = "f-remision"
       &Frame = "f-remiposttotdia" }

    /***********************************************************************/
    /* DIARIO DE VENTAS DE CONTADO POSTFECHADAS DE DIAS ANTERIORES         */
    /***********************************************************************/

    {programas/tesa0831.i &Mensaje = "DIARIO VTAS CONTADO POSTFEC. ANT."}
    ASSIGN l-encontro = false
           l-tot      = 0
           l-iva      = 0.

    FOR EACH MovCaja WHERE MovCaja.Id-Caja   = CtlCaja.Id-Caja     AND
                           MovCaja.Turno     = CtlCaja.Turno       AND
                           MovCaja.FecOper  <> CtlCaja.FecOper     AND
                           MovCaja.FecDep    = CtlCaja.FecOper     NO-LOCK
                           USE-INDEX idx-fecdep
                           BY MovCaja.Referencia :

          IF MovCaja.TipoVenta = 2 OR MovCaja.TipoVenta = 9 OR MovCaja.TipoVenta = 8 THEN DO:
             IF MovCaja.TipoVenta = 9 OR MovCaja.TipoVenta = 8 THEN DO:
               FIND NCR WHERE NCR.Id-Ncr = MovCaja.Referencia NO-LOCK NO-ERROR.
               IF NOT AVAILABLE Ncr THEN NEXT.
               FIND FIRST DetNcr OF NCR NO-LOCK NO-ERROR.
               IF DetNCR.TipoVenta <> 2 THEN NEXT.
               FIND Remision WHERE Remision.Id-Remision = DetNcr.Documento
                               AND Remision.TipoVenta = 2 NO-LOCK NO-ERROR.
               IF Remision.Facglobal <> "" THEN NEXT.
               FIND FIRST B-Mov WHERE
                          B-Mov.Referencia = Remision.Id-Remision AND
                          B-Mov.TipoVenta = 2 NO-LOCK NO-ERROR.
               IF AVAILABLE B-Mov AND B-Mov.FecDep <= B-Mov.FecOper THEN NEXT.
               IF AVAILABLE B-Mov AND B-Mov.FecDep <> CtlCaja.FecOper THEN NEXT.
             END.

             ELSE DO:
                FIND Remision WHERE Remision.Id-Remision = MovCaja.Referencia
                                AND Remision.TipoVenta = 2 NO-LOCK NO-ERROR.
                IF Remision.FacGlobal <> "" THEN NEXT.
                IF MovCaja.FecDep <= MovCaja.FecOper THEN NEXT.
                IF MovCaja.FecDep <> CtlCaja.FecOper THEN NEXT.
             END.
             ASSIGN l-encontro = TRUE.
             {programas/tesa0832.i &Frame = "f-remipostant"}
             IF MovCaja.Canc THEN NEXT.
             ASSIGN l-remipostant = l-remipostant + IF MovCaja.TipoVenta = 2
                                                    THEN MovCaja.TotVenta
                                                    ELSE (MovCaja.TotVenta * -1).
          END.
    END.

    {programas/tesa0833.i
       &Framepri = "f-remipostant"
       &Frame = "f-remiposttotant" }
  
    /***********************************************************************/
    /* TOTALES DE VENTAS DE CONTADO                                        */
    /***********************************************************************/

    DISPLAY STREAM s-salida
       SKIP(6)
       l-reminormales + l-remipostdia
                      LABEL "TOTAL DEL DIA"        COLON 14
                      FORMAT '-ZZ,ZZZ,ZZ9.99'
       l-remipostdia  LABEL "POST DEL DIA "        COLON 14 SKIP
       l-remipostant  LABEL "POST ANTERIOR"        COLON 14 SKIP
       SPACE(15)      "--------------"                      SKIP(0)
       (l-reminormales + l-remipostant)
                        LABEL "TOTAL" FORMAT "-zz,zzz,zz9.99" COLON 14 skip(1)
       l-anticipo     LABEL 'ANTICIPOS    '        COLON 14 SKIP
       SPACE(15)      "--------------"                      SKIP(0)
       (l-reminormales + l-remipostant - l-anticipo)
                LABEL "A DEPOSITO" FORMAT "-zz,zzz,zz9.99" COLON 14

    WITH FRAME f-totalesremi OVERLAY SIDE-LABEL COLUMN 5 WIDTH 38.


   /* diario de devoluciones */

   {programas/tesa0831.i &Mensaje = "DIARIO DE DEVOLUCIONES"}
   ASSIGN l-entro = FALSE.

   FOR EACH MovCaja WHERE MovCaja.Id-Caja  = CtlCaja.Id-Caja     AND
                          MovCaja.Turno    = CtlCaja.Turno       AND
                          MovCaja.FecOper  = CtlCaja.FecOper     NO-LOCK:

       IF MovCaja.Canc THEN NEXT.
       IF MovCaja.TipoVenta < 2 THEN DO:
          FIND Remision WHERE Remision.Id-Remision = MovCaja.Referencia AND
               Remision.TipoVenta = MovCaja.TipoVenta NO-LOCK NO-ERROR.
          FOR EACH DetRemis WHERE DetRemis.Id-Remision = MovCaja.Referencia AND
                   DetRemis.Tipo = 6 NO-LOCK :
              CREATE w-dev.
              ASSIGN w-dev.tipo    = MovCaja.TipoVenta
                     w-Dev.Id-Dev  = INT(SUBSTRING(Detremis.descr,14,6))
                     w-Dev.Factura = MovCaja.Referencia
                     w-Dev.Ncr     = ''
                     w-Dev.Monto   = MovCaja.TotVenta
                     w-Dev.usuario = ''.
          END.
       END.

       IF MovCaja.TipoVenta = 4 OR MovCaja.TipoVenta = 8 OR MovCaja.TipoVenta = 9 THEN DO:
          CREATE w-Dev.
          ASSIGN w-Dev.Monto   = MovCaja.TotVenta .
          IF MovCaja.TipoVenta = 4 THEN DO:
            FIND Devolucion WHERE Devolucion.Id-Dev = INT(MovCaja.Referencia)
                                                        NO-LOCK NO-ERROR.
            ASSIGN w-Dev.Factura = Devolucion.Id-Factura
                   w-Dev.Id-Dev  = INT(MovCaja.Referencia)
                   w-Dev.Ncr     =  Devolucion.Id-ncr
                   w-Dev.Tipo    =  Devolucion.TipoVenta.
          END.
          ELSE DO:
            FIND Ncr WHERE Ncr.Id-ncr = MovCaja.Referencia NO-LOCK NO-ERROR.
            FIND FIRST DetNCr OF NCR NO-LOCK NO-ERROR.
            FIND FIRST b-Mov WHERE b-Mov.Referencia = detNcr.Documento
                                NO-LOCK NO-ERROR.
            IF AVAILABLE b-Mov THEN DO:
               FIND FIRST DetMovC WHERE DetMovC.Folio = b-Mov.Folio AND
                          DetMovC.Id-Caja = b-Mov.Id-caja AND  (
                          DetMovC.Mov = 'R' OR DetMovC.Mov = 'F'  )
                          NO-LOCK NO-ERROR.
               IF AVAILABLE DetMovC THEN
                  ASSIGN w-Dev.usuario =  DetMovC.UsuarioAutC.
            END.

            ASSIGN w-Dev.Factura =  Detncr.Documento
                   w-Dev.Id-Dev  =  NCR.Id-cliente
                   w-Dev.Ncr     =  Detncr.Id-ncr
                   w-Dev.Tipo    =  DetNcr.TipoVenta
                   w-Dev.Esp     = true.
          END.
       END.
   END.

   FOR EACH w-Dev BREAK BY w-Dev.Tipo BY w-Dev.Id-Dev :
        IF FIRST(w-Dev.Tipo) THEN
            ASSIGN l-entro = TRUE.

        IF FIRST-OF(w-Dev.Tipo) THEN DO:
            IF w-Dev.Tipo = 2 THEN DO:
                ASSIGN l-tipo = "CTD".
                DISPLAY STREAM s-salida
                    " DEVOLUCIONES DE VENTAS CONTADO"
                    " FOLIO   REF     NCR       MONTO"
                    "------ ------- ------- ---------"
                WITH FRAME f-men OVERLAY NO-LABEL WIDTH 38.
            END.
            ELSE DO:
                ASSIGN l-tipo = 'TIC'.
                DISPLAY STREAM s-salida
                    " DEVOLUCIONES DE TICKET"
                    " FOLIO   REF     NCR       MONTO"
                    "------ ------- ------- ---------"
                WITH FRAME f-men2 OVERLAY NO-LABEL WIDTH 38.
            END.
        END.
        DISPLAY STREAM s-salida
            w-Dev.Id-dev
            w-Dev.Factura
            w-Dev.Ncr
            w-Dev.Monto       WITH FRAME f-ocho.
        DOWN STREAM s-salida WITH FRAME f-ocho.

        IF w-Dev.Esp THEN DO:
           DISPLAY STREAM s-salida w-Dev.Usuario WITH FRAME f-autoriza.
           DOWN STREAM s-salida WITH FRAME f-autoriza.
        END.
        ACCUMULATE w-Dev.Monto (TOTAL).
        ACCUMULATE w-Dev.Monto (TOTAL BY w-Dev.Tipo).
        IF LAST-OF(w-Dev.Tipo) THEN DO:
            UNDERLINE STREAM s-salida w-Dev.Monto WITH FRAME f-ocho.
            DISPLAY STREAM s-salida
                  ACCUMULATE TOTAL BY w-Dev.Tipo w-Dev.Monto @ w-Dev.Monto
                  WITH FRAME f-ocho.
            DOWN STREAM s-salida 1 WITH FRAME f-ocho.
        END.
        IF LAST(w-Dev.Tipo) THEN DO:
            DISPLAY STREAM s-salida
                ACCUM TOTAL w-Dev.Monto LABEL "TOTAL"
                FORMAT "zzzzz,zz9.99"
                WITH FRAME f-totocho OVERLAY SIDE-LABEL WIDTH 38 COLUMN 15.
        END.
  END.

  IF NOT l-entro THEN DO:
      VIEW STREAM s-salida FRAME f-ocho.
      DISPLAY STREAM s-salida
            "   TOTAL:       0.00" AT 2   SKIP
      WITH FRAME f-totocho2 OVERLAY SIDE-LABEL COLUMN 5 WIDTH 38.
  END.

   /* Cheques cambiados por Efectivo */
   {programas/tesa0831.i &Mensaje = "CHEQUES CAMBIADOS POR EFECTIVO"}
   DISPLAY STREAM s-salida
       "#-CHEQ BANCO    No.CUENTA     IMPORTE"
       "====================================="
       WITH FRAME f-cheque NO-LABELS NO-BOX  WIDTH 50.

  FOR EACH MovCaja WHERE
           MovCaja.Id-Caja   = CtlCaja.id-Caja AND
           MovCaja.Turno     = CtlCaja.Turno AND
           MovCaja.Fecoper   = CtlCaja.Fecoper AND
           MovCaja.TipoVenta = 7 NO-LOCK BY MovCaja.Folio :

      FIND FIRST DetMovC WHERE
                 DetMovC.Id-Caja = CtlCaja.Id-Caja AND
                 DetMovC.Folio = MovCaja.Folio AND
                 DetMovC.Sec   = 2 NO-LOCK NO-ERROR.

      IF AVAILABLE DetMovC THEN DO:
          FIND Banco OF DetMovC NO-LOCK NO-ERROR.
          DISPLAY STREAM s-salida
                 DetMovC.Cheque   FORMAT 'X(6)'
                 Banco.NomCto     FORMAT 'X(8)'
                 DetMovC.Ctacheq  FORMAT 'X(10)'
                 DetMovC.MontoPago FORMAT 'ZZZ,ZZ9.99'
                 WITH FRAME f-cambio NO-LABELS DOWN NO-BOX.
          DOWN STREAM s-salida WITH FRAME f-cambio.
          ACCUMULATE DetMovC.MontoPago (TOTAL).
      END.
  END.

  DISPLAY STREAM s-salida
          "----------" AT 28
          (ACCUM TOTAL DetMovC.MontoPago) FORMAT 'ZZZ,ZZ9.99' COLON 26
          LABEL 'TOTAL'
          WITH FRAME f-totcam SIDE-LABELS NO-BOX.


  /***********************************************************************/
  /* DIARIO DE CONCENTRACIONES DEL DIA                    */
  /***********************************************************************/

  {programas/tesa0831.i &Mensaje = "DIARIO DE CONCENTRACIONES"}  
  FOR EACH MovCaja WHERE MovCaja.turno = CtlCaja.turno AND
                         MovCaja.id-caja = CtlCaja.Id-Caja AND                       
                         MovCaja.FecOper = CtlCaja.FecOper AND 
                         MovCaja.TipoVenta = 6 NO-LOCK:
        IF MovCaja.Canc THEN NEXT.
        ACCUMULATE MovCaja.Folio (COUNT).                
        
        DISPLAY STREAM s-salida            
            MovCaja.Folio 
            STRING(MovCaja.HorReg,"HH:MM") @ l-hora
            MovCaja.Referencia   
            MovCaja.Id-Cajero    
            MovCaja.TotVenta     
        WITH FRAME f-dconcentra OVERLAY NO-BOX WIDTH 38.
        DOWN STREAM s-salida WITH FRAME f-dconcentra.          
  END.
  
  DISPLAY STREAM s-salida
      (ACCUM COUNT MovCaja.Folio) LABEL "  CANT. CONC" 
  WITH FRAME f-totconc OVERLAY SIDE-LABEL WIDTH 38 NO-BOX DOWN COLUMN 2.  
  
  DISPLAY STREAM s-salida
     SKIP(5)
     "."
  WITH FRAME f-nada OVERLAY WIDTH 38.

  /*
  PUT STREAM s-Salida CONTROL CHR(27) + CHR(18).
  PUT STREAM s-salida CONTROL CHR(27) + CHR(80).
  PUT STREAM s-salida CONTROL CHR(20).
  */
  PUT STREAM s-salida CONTROL CHR(27) + CHR(105).        /* Corta papel */
  PUT STREAM s-salida CONTROL CHR(27) + CHR(64).         /* Inicializa */

  OUTPUT STREAM s-salida CLOSE.
  UNIX SILENT VALUE("chmod 777 " + l-reporte).
  UNIX SILENT VALUE("ls -l " + l-reporte). /* Diagnóstico */
  
  FIND Usuario WHERE Usuario.Id-User = "sis10" /*USERID('dictdb') */ NO-LOCK NO-ERROR.
  
  
/* Cambiar permisos del archivo para que sea legible por otros usuarios */
  
  IF Usuario.Id-User = "sis10" THEN  
      UNIX SILENT VALUE("cat  " + l-reporte + " > /dev/prb01" ).
  ELSE UNIX SILENT VALUE("cat  " + l-reporte + " > /dev/prb01" ).  
  
  UNIX SILENT VALUE("chmod 777 /dev/prb01").
    
      RUN programas/cieimpre.p (INPUT l-reporte, 20 ,l-impresora).   

  
  HIDE MESSAGE NO-PAUSE.
  LEAVE.

END.
