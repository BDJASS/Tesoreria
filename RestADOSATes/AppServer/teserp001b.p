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
DEFINE VARIABLE l-Archivo      AS CHARACTER NO-UNDO.

DEFINE VARIABLE Acu            AS DECIMAL.
DEFINE VARIABLE Efvo           AS DECIMAL FORMAT ">>>,>>>,>>>,>>9.99".  
DEFINE VARIABLE NCred          AS DECIMAL.
DEFINE VARIABLE l-NCr8         AS DECIMAL.
DEFINE VARIABLE DevTC          AS DECIMAL.
DEFINE VARIABLE Amex           AS DECIMAL.
DEFINE VARIABLE Amig           AS DECIMAL.
DEFINE VARIABLE Cred           AS DECIMAL.
DEFINE VARIABLE Cheq           AS DECIMAL.
DEFINE VARIABLE DebT           AS DECIMAL.
DEFINE VARIABLE CheqPF         AS DECIMAL.
DEFINE VARIABLE CheqPFA        AS DECIMAL.
DEFINE VARIABLE DLoc           AS DECIMAL   NO-UNDO.
DEFINE VARIABLE DFor           AS DECIMAL   NO-UNDO.

DEFINE VARIABLE l-Caja         LIKE MovCaja.Id-Caja NO-UNDO.
DEFINE VARIABLE l-TP           LIKE DetMovC.Id-TP NO-UNDO.    
 
DEFINE VARIABLE l-Cajas        AS CHARACTER EXTENT 16.
DEFINE VARIABLE l-Linea        AS CHARACTER EXTENT 16.
DEFINE VARIABLE l-Linea2       AS CHARACTER EXTENT 16.
DEFINE VARIABLE l-Ticket       AS DECI.
DEFINE VARIABLE l-Factura      AS CHARACTER EXTENT 16.
DEFINE VARIABLE l-Total        AS CHARACTER EXTENT 16.
DEFINE VARIABLE l-Dife         AS CHARACTER EXTENT 16.
DEFINE VARIABLE v-Tic          AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-Fac          AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-Dif          AS DECIMAL   NO-UNDO.
DEFINE VARIABLE l-Term         AS INTEGER. 

DEFINE VARIABLE l-nomSuc       AS CHARACTER NO-UNDO.

DEFINE VARIABLE TDebT          AS DECIMAL.
DEFINE VARIABLE tNCred         AS DECIMAL.
DEFINE VARIABLE l-TNCr8        AS DECIMAL.
DEFINE VARIABLE tDevTC         AS DECIMAL.
DEFINE VARIABLE TCCred         AS DECIMAL.
DEFINE VARIABLE TAmig          AS DECIMAL.
DEFINE VARIABLE tAmex          AS DECIMAL.
DEFINE VARIABLE tCheq          AS DECIMAL.
DEFINE VARIABLE tEfvo          AS DECIMAL.
DEFINE VARIABLE tChPF          AS DECIMAL.
DEFINE VARIABLE tChPFA         AS DECIMAL.
DEFINE VARIABLE tDLoc          AS DECIMAL   NO-UNDO.
DEFINE VARIABLE tDFor          AS DECIMAL   NO-UNDO.

DEF    VAR      l-cf           AS INTEGER   NO-UNDO.
DEF    VAR      l-Refer1       AS CHAR      NO-UNDO FORMAT 'x(6)'.
DEF    VAR      l-Articulo     LIKE Articulo.Id-Articulo NO-UNDO.
DEF    VAR      l-Fecha        AS DATE      FORMAT "99/99/9999" NO-UNDO.
DEF    VAR      l-FrameValue   AS CHAR      NO-UNDO.
DEF    VAR      l-SubTot       LIKE FichaDep.Importe NO-UNDO.
DEF    VAR      l-Depositos    LIKE PagoAcuse.Importe NO-UNDO.
DEF    VAR      l-Deudores     LIKE PagoAcuse.Importe NO-UNDO.
DEF    VAR      l-totME        AS DECIMAL   FORMAT "zzzzz,zz9.99" NO-UNDO.      /*_ RNPC _*/
DEF    VAR      l-totMEMN      AS DECIMAL   FORMAT "zzzzz,zz9.99" NO-UNDO.      /*_ RNPC _*/
DEF    VAR      l-totMEFac     AS DECIMAL   FORMAT "zzzzz,zz9.99" NO-UNDO.      /*_ RNPC _*/
DEF    VAR      l-totMEMNFac   AS DECIMAL   FORMAT "zzzzz,zz9.99" NO-UNDO.      /*_ RNPC _*/
DEF    VAR      l-totFacMNenUS AS DECIMAL   FORMAT "zzzzz,zz9.99" NO-UNDO.      /*_ RNPC _*/
DEF    VAR      l-totMNpagoUS  AS DECIMAL   FORMAT "zzzzz,zz9.99" NO-UNDO.     /*_ RNPC _*/
DEF    VAR      l-totMEMNPago  AS DECIMAL   FORMAT "zzzzz,zz9.99" NO-UNDO.     /*_ RNPC _*/
DEF    VAR      l-moneda       AS CHARACTER FORMAT "x(3)" NO-UNDO INITIAL "".  /*_ RNPC _*/
DEF    VAR      l-acuselst     AS CHARACTER NO-UNDO INITIAL "".                /*_ RNPC _*/
DEF    VAR      l-acuseMNenUS  AS CHARACTER NO-UNDO INITIAL "".                /*_ RNPC _*/
DEF    VAR      l-SubTotME     LIKE FichaDep.Importe NO-UNDO.                   /*_ RNPC _*/
DEF    VAR      l-total1       LIKE PagoAcuse.Importe NO-UNDO.
DEF    VAR      l-total2       LIKE PagoAcuse.Importe NO-UNDO.
DEF    VAR      l-ImpDeud      LIKE FichaDep.Importe NO-UNDO.
DEF    VAR      l-Sub1         LIKE FichaDep.Importe NO-UNDO.
DEF    VAR      l-Sub2         LIKE FichaDep.Importe NO-UNDO.
DEF    VAR      l-Sub3         LIKE FichaDep.Importe NO-UNDO.
DEF    VAR      l-Tot1         LIKE FichaDep.Importe NO-UNDO.
DEF    VAR      l-Tot2         LIKE FichaDep.Importe NO-UNDO.
DEF    VAR      l-Tot3         LIKE FichaDep.Importe NO-UNDO.
DEF    VAR      l-Reporte      AS CHAR      NO-UNDO.
DEF    VAR      l-Tipo         AS CHAR      NO-UNDO FORMAT "x(3)".
DEF    VAR      l-Poliza       AS CHAR      NO-UNDO FORMAT "x(6)".
DEF    VAR      l-NPol         LIKE Poliza.Id-Poliza NO-UNDO.
DEF    VAR      l-NSer         LIKE Poliza.Serie NO-UNDO.
DEF    VAR      l-NS           LIKE Cuenta.Id-SCta NO-UNDO.
DEF    VAR      l-cta          LIKE MovPoliza.Id-Cta NO-UNDO.
DEF    VAR      l-Scta         LIKE MovPoliza.Id-SCta NO-UNDO.
DEF    VAR      l-SScta        LIKE MovPoliza.Id-SSCta NO-UNDO.
DEF    VAR      l-SSScta       LIKE MovPoliza.Id-SSSCta NO-UNDO.
DEF    VAR      l-Sec          LIKE MovPoliza.Sec NO-UNDO.
DEF    VAR      l-cargo        AS DECI      FORMAT "zz,zzz,zz9.99".
DEF    VAR      l-TMax         AS INTEGER   NO-UNDO.

DEFINE VARIABLE l-FecIni       AS DATE      FORMAT "99/99/9999" NO-UNDO.
DEFINE VARIABLE l-FecFin       AS DATE      FORMAT "99/99/9999" NO-UNDO.
DEFINE VARIABLE l-veces        AS INT       NO-UNDO INITIAL 0.
DEFINE VARIABLE l-Camion       AS INTEGER   FORMAT "zz9" NO-UNDO.
DEFINE VARIABLE l-FolEmb       LIKE Embarque.Id-Embarque NO-UNDO.
DEFINE VARIABLE l-FecEmb       AS DATE      FORMAT '99/99/9999' NO-UNDO.
DEFINE VARIABLE l-index        AS INTEGER.
DEFINE VARIABLE l-index2       AS INTEGER.
DEFINE VARIABLE l-HorEmb       AS CHARACTER FORMAT "x(5)" NO-UNDO.
DEFINE VARIABLE l-CantEmb      AS INTEGER   NO-UNDO.
DEFINE VARIABLE l-Estatus      LIKE DetEmbarque.Estatus NO-UNDO.
DEFINE VARIABLE l-SVeces       AS CHARACTER NO-UNDO FORMAT "x(2)".
DEFINE VARIABLE l-Aster        AS CHARACTER NO-UNDO FORMAT "x(1)".
DEFINE VARIABLE l-Prim         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE l-NOExis       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE l-Mtto         AS INTEGER   NO-UNDO.
DEFINE VARIABLE l-SoloTrasp    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE l-ListDev      AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-TotDev       AS DECIMAL   NO-UNDO.
DEFINE VARIABLE l-opcion       AS CHARACTER EXTENT 4 FORMAT "x(16)" INITIAL ["Normal", "Condensado Gral.", "Condensado Chih.", "Internet"] NO-UNDO.
DEFINE VARIABLE l-opcion2      AS CHARACTER EXTENT 2 FORMAT "x(10)" INITIAL ["Remisiones", "Facturas"] NO-UNDO.
DEFINE VARIABLE l-PagInfo      LIKE Pedido.PagInfo NO-UNDO.
DEFINE VARIABLE l-FecPed       AS DATE      NO-UNDO.
DEFINE VARIABLE l-TotPed       AS DECIMAL   NO-UNDO.
DEFINE VARIABLE l-Sobrante     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE l-Pedidos      AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-ReqCte       AS CHARACTER FORMAT "X(6)" LABEL "ReqCte". 

DEF BUFFER b-FichaDep FOR FichaDep.

DEFINE TEMP-TABLE ttCorte
    FIELD idsuc         AS CHARACTER 
    FIELD TotalCorte    LIKE DetMovC.MontoPago
    FIELD TotalEfectivo LIKE DetMovC.MontoPago
    FIELD TotalTarjeta  LIKE DetMovC.MontoPago
    FIELD TotalTransf   LIKE DetMovC.MontoPago
    FIELD TotalCheque   LIKE DetMovC.MontoPago
    FIELD TotalCaja     LIKE DetMovC.MontoPago
    FIELD TotalFacturas LIKE DetMovC.MontoPago.


DEFINE TEMP-TABLE ttCorteSuc
    FIELD idsuc          AS CHAR
    FIELD termi          AS INT // ES PARA EL ORDEN 
    FIELD suc            AS CHAR
    FIELD IdSucursal     AS CHARACTER
    FIELD Sucursal       AS CHARACTER
    FIELD FecCorte       AS DATE
    FIELD tAmex          AS DECIMAL  /* SUMAR TARJETAS */
    FIELD TarjetaDebito  AS DECIMAL
    FIELD TarjetaCredito AS DECIMAL
    FIELD TotVales       AS DECIMAL
    FIELD Cheq           AS DECIMAL   // ajuste
    FIELD Efevo          AS DECIMAL   // ajuste
    FIELD DLoc           AS DECIMAL   // ajuste
    FIELD TotAnticipos   AS DECIMAL 
    FIELD TotDev         AS DECIMAL
    FIELD EfePanam       AS DECIMAL
    FIELD Miles          AS INTEGER
    FIELD Traslado       AS DECIMAL
    FIELD Verificacion   AS DECIMAL
    FIELD CajaChica      AS DECIMAL 
    FIELD TotalSucursal  AS DECIMAL.  // ajuste
DEFINE TEMP-TABLE ttCteCaja
    FIELD IdSucursal     AS CHARACTER
    FIELD IdCaja         AS INTEGER
    FIELD Turno          AS INTEGER    
    FIELD Sucursal       AS CHARACTER
    FIELD NomCajero      AS CHARACTER
    FIELD FecCorte       AS DATE
    FIELD FecOper        AS DATE
    FIELD TarjetaDebito  AS DECIMAL
    FIELD TarjetaCredito AS DECIMAL
    FIELD TotVales       AS DECIMAL
    FIELD TotCheque      AS DECIMAL
    FIELD TotEfe         AS DECIMAL
    FIELD TotFacturas    AS DECIMAL
    FIELD TotDeposito    AS DECIMAL
    FIELD TotAnticipos   AS DECIMAL
    FIELD TotDev         AS DECIMAL
    FIELD Retiros        AS INTEGER
    FIELD TotRetiros     AS DECIMAL
    FIELD TotFaltante    AS DECIMAL
    FIELD TotVtaContado  AS DECIMAL   
    FIELD ImporteTotal   AS DECIMAL.
    
DEFINE TEMP-TABLE ttRetiros
    FIELD IdSucursal    AS CHARACTER
    FIELD IdCaja        AS INTEGER
    FIELD Turno         AS INTEGER 
    FIELD NumRetiro     AS CHARACTER
    FIELD HoraRetiro    AS CHARACTER
    FIELD ImporteRetiro AS DECIMAL.
    
DEFINE TEMP-TABLE ttSaldoSuc
    FIELD idsuc    AS CHARACTER 
    FIELD Cliente  LIKE Cliente.Id-Cliente
    FIELD Sucursal LIKE Cliente.RazonSocial
    FIELD Saldo    LIKE DetMovC.MontoPago.

DEFINE TEMP-TABLE ttDepositos
    FIELD idsuc           AS CHARACTER 
    FIELD Fecha           AS DATE
    FIELD TotalDepositado LIKE DetMovC.MontoPago     
    FIELD DepositoTotal   LIKE DetMovC.MontoPago
    FIELD Diferencia      LIKE DetMovC.MontoPago.

DEFINE DATASET dsTesoreria FOR 
    ttCorte, /* Tabla principal */
    ttCorteSuc, /* Relación  */
    ttSaldoSuc,
    ttDepositos
    DATA-RELATION SucDetalle FOR ttCorte, ttCorteSuc
    RELATION-FIELDS (idsuc, idsuc)
    DATA-RELATION SucDetalle2 FOR ttCorte, ttSaldoSuc
    RELATION-FIELDS (idsuc, idsuc)
    DATA-RELATION SucDetalle3 FOR ttCorte, ttDepositos
    RELATION-FIELDS (idsuc, idsuc). 


DEFINE TEMP-TABLE tt-Cajas2
    FIELD Efevo   AS CHARACTER EXTENT 16 LABEL "efectivo"
    FIELD tAmex   AS CHARACTER EXTENT 16 LABEL "t.Amex"
    FIELD DLoc    AS CHARACTER EXTENT 16 LABEL "D.Local"
    FIELD CheqPFA AS CHARACTER EXTENT 16 LABEL "Cheq pfa".
     
DEFINE TEMP-TABLE tt-MovCaja LIKE movcaja
    FIELD Sucursal AS CHARACTER  
    FIELD SucDesc  AS CHARACTER.

DEFINE VARIABLE l-saldo AS DECIMAL FORMAT ">>>,>>>,>>9.99".
DEFINE VARIABLE l-cte   AS INT.

DEFINE VARIABLE l-suma  AS DECIMAL NO-UNDO.
DEF    VAR      l-pago AS DECIMAL FORMAT ">>>,>>>,>>9.99".   
DEFINE VARIABLE v-nomimp      AS CHARACTER NO-UNDO.

DEF    VAR      l-ventas      AS DECI      FORMAT "$zzzz,zz9.99-" LABEL " CONTADO" NO-UNDO.
DEF    VAR      l-dev         LIKE l-ventas LABEL "DEVOLUCION" NO-UNDO.
DEFINE VARIABLE l-cheque      AS DECIMAL   FORMAT "$zzzz,zz9.99-" NO-UNDO.
DEFINE VARIABLE l-tcredito    AS DECIMAL   FORMAT "$zzzz,zz9.99-" NO-UNDO.
DEFINE VARIABLE l-tdebito     AS DECIMAL   FORMAT "$zzzz,zz9.99-" NO-UNDO.
DEF    VAR      l-ValCtes     AS DECIMAL   NO-UNDO.
DEF    VAR      l-ValEmpEf    AS DECIMAL   NO-UNDO.
DEF    VAR      l-ValEmpNo    AS DECIMAL   NO-UNDO.
DEF    VAR      l-MontoEf     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE l-TotEfectivo AS DECIMAL   NO-UNDO.
DEFINE VARIABLE l-TotVale     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE l-TotVoucher  AS DECIMAL   NO-UNDO.
DEFINE VARIABLE l-TotCheque   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE l-TotDevTC    AS DECIMAL   NO-UNDO.

DEFINE VARIABLE l-efectivo    LIKE CorteCaja.Declaracion FORMAT '$ZZ,ZZZ,ZZ9.99'.
DEFINE VARIABLE l-retiros     LIKE CorteCaja.Declaracion FORMAT '$ZZ,ZZZ,ZZ9.99'.
DEF    VAR      l-facturas    LIKE l-ventas LABEL "FACTURAS" NO-UNDO.

DEFINE VARIABLE l-cantretiros AS INTEGER   NO-UNDO.

DEFINE VARIABLE l-SiVale      AS LOGICAL   NO-UNDO.

DEF    BUFFER bf-cli    FOR Cliente .  
DEF    BUFFER bff-mov   FOR MovCliente. 
   
DEFINE BUFFER bf-cajas  FOR ttCorteSuc.
DEFINE BUFFER b-DetMovC FOR DetMovC.
DEFINE BUFFER bf-mov    FOR MovCaja.
DEFINE BUFFER rm-Mov    FOR tt-MovCaja.   
DEFINE BUFFER b-Mov     FOR MovCaja.  

DEFINE TEMP-TABLE w-Tipo
    FIELD Id-tp LIKE TipoPago.Id-tp
    FIELD Monto AS DECIMAL
    INDEX Idx-IdTP Id-tp ASCENDING.
    
DEFINE BUFFER b-MovCaja  FOR MovCaja.
DEFINE BUFFER b-Remision FOR Remision.
    
/* **********************  Internal Procedures  *********************** */



@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetDashBoard:

    /*************************************************************
      Empresa : ADOSA
      Programa: tesc0122.p
      Funcion : Reporte de corte global por Sucursal
      Autor   : David Aguirre
      Fecha   : 24/10/2013
    ************************************************************* */

    DEFINE INPUT PARAMETER l-Fecha AS DATE NO-UNDO.
    DEFINE OUTPUT PARAMETER DATASET FOR dsTesoreria.  

    DEFINE BUFFER w-Caja FOR Caja.
    DEFINE VARIABLE cDeptosValidos AS CHARACTER NO-UNDO 
        INIT "402,204,506,510,511,512,513,514,515".
    
    FOR EACH Caja NO-LOCK
        WHERE LOOKUP(STRING(Caja.Id-Depto), cDeptosValidos) > 0,
        EACH CtlCaja NO-LOCK
        WHERE CtlCaja.id-caja = Caja.id-caja
        AND CtlCaja.FecOper = l-Fecha BREAK BY Caja.Id-Caja:
        EMPTY TEMP-TABLE w-Tipo.
        IF FIRST-OF (Caja.Id-Caja) THEN 
        DO:            
            ASSIGN   
                l-MontoEf = 0.
        END.
    
        FIND FIRST Depto WHERE Depto.Id-Depto = Caja.Id-Depto NO-LOCK NO-ERROR.
    
        FIND FIRST w-Caja WHERE w-Caja.Id-caja = CtlCaja.Id-caja NO-LOCK NO-ERROR.
        CREATE ttCteCaja.
   
        ASSIGN 
            ttCteCaja.IdCaja     = CtlCaja.Id-caja
            ttCteCaja.Turno      = CtlCaja.Turno
            ttCteCaja.IdSucursal = w-Caja.Id-Depto            
            ttCteCaja.FecCorte   = CtlCaja.FecCierre
            ttCteCaja.FecOper    = CtlCaja.FecOper.
            
        IF AVAILABLE Depto THEN ASSIGN ttCteCaja.Sucursal = Depto.Nombre.
            
        ASSIGN
            l-ValCtes     = 0
            l-ValEmpEf    = 0
            l-ValEmpNo    = 0
            l-TotVale     = 0
            l-TotVoucher  = 0
            l-TotCheque   = 0
            l-TotEfectivo = 0
            l-tcredito    = 0
            l-tdebito     = 0.
        FOR EACH MovCaja WHERE MovCaja.Turno = CtlCaja.Turno AND
            MovCaja.Id-Caja = CtlCaja.Id-caja AND
            MovCaja.Fecoper = CtlCaja.Fecoper NO-LOCK:
            FIND Vendedor WHERE Vendedor.Id-Vendedor = STRING(MovCaja.Id-Cajero) NO-LOCK NO-ERROR.
       
            IF AVAILABLE Vendedor THEN 
            DO:
                FIND LAST Empleado WHERE Empleado.Iniciales = Vendedor.Iniciales NO-LOCK NO-ERROR.
                IF AVAILABLE Empleado THEN ASSIGN ttCteCaja.NomCajero = empleado.Nombre.
            END.
                              
            l-SiVale = FALSE.
            IF MovCaja.TipoVenta = 2 THEN 
            DO:
                FIND Remision WHERE Remision.Id-Remision = MovCaja.Referencia 
                    NO-LOCK NO-ERROR.
                IF AVAILABLE Remision AND Remision.SustIdRemision <> "" THEN 
                DO:
                    FIND b-Remision 
                        WHERE b-Remision.Id-Remision = Remision.SustIdRemision
                        NO-LOCK NO-ERROR.
                    IF AVAILABLE b-Remision THEN 
                    DO:
                        FIND b-MovCaja WHERE b-MovCaja.Refer = b-Remision.Id-Remision
                            AND b-MovCaja.TipoVenta = b-Remision.TipoVenta
                            NO-LOCK NO-ERROR.
                        IF AVAILABLE b-MovCaja AND b-MovCaja.FecOper <> MovCaja.FecOper
                            THEN NEXT.
                    /*ELSE IF NOT AVAILABLE b-MovCaja AND b-Remision.FecReg <> MovCaja.FecOper THEN NEXT.*/
                    END.
                END.
                ELSE IF AVAILABLE Remision AND Remision.PorIdRemision <> "" THEN 
                    DO:
                        FIND b-Remision 
                            WHERE b-Remision.Id-Remision = Remision.PorIdRemision
                            NO-LOCK NO-ERROR.
                        IF AVAILABLE b-Remision THEN 
                        DO:
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
                    IF DetMovC.Mov = 'P' THEN 
                    DO:
                        FIND FIRST w-Tipo WHERE w-Tipo.Id-tp = DetMovC.Id-tp
                            NO-ERROR.
                        IF NOT AVAILABLE w-Tipo THEN 
                        DO:
                            CREATE w-Tipo.
                            ASSIGN 
                                w-Tipo.Id-Tp = DetMovC.Id-tp.
                        END.
                 
                        ASSIGN 
                            w-Tipo.Monto = w-Tipo.Monto + DetMovC.MontoPAGO.
                        
                        IF DetMovC.Id-tp = 52 OR DetMovC.Id-tp = 62 THEN
                            ASSIGN
                                l-TotVoucher = l-TotVoucher + DetMovC.MontoPago.
                                
                        IF DetMovC.Id-TP = 52 THEN ASSIGN l-tdebito = l-tdebito + DetMovC.MontoPago.        
                        IF DetMovC.Id-TP = 62 THEN ASSIGN l-tcredito = l-tcredito + DetMovC.MontoPago.
                        IF DetMovC.Id-tp = 61 THEN
                            ASSIGN
                                l-TotCheque = l-TotCheque + DetMovC.MontoPago.
                         
                    END.
                END.
            IF MovCaja.TipoVenta < 3 THEN 
            DO:
                FIND Remision WHERE Remision.Id-Remision = MovCaja.Referencia NO-LOCK NO-ERROR.
                IF AVAILABLE Remision THEN
                    FOR EACH DetRemis WHERE DetRemis.Id-Remision = MovCaja.Refer
                        AND DetRemis.Tipo = 6
                        NO-LOCK:
                        IF DetRemis.Descr BEGINS 'VALE' THEN 
                        DO:
                            IF Remision.Id-Nomina = 0 THEN
                                l-ValCtes = l-ValCtes + ABS(ROUND(DetRemis.Importe + DetRemis.Iva,2)).
                            ELSE IF DetRemis.Importe = 0 THEN 
                                DO:
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
        ASSIGN 
            ttCteCaja.TotVales = l-TotVale.
//        FOR EACH TipoPago WHERE TipoPago.Id-TP = 60 NO-LOCK BY TipoPago.Id-tp  :
//            FIND FIRST w-Tipo WHERE w-Tipo.Id-Tp = TipoPago.Id-Tp NO-ERROR.
//            FIND FIRST CorteCaja WHERE
//                CorteCaja.Id-Caja = CtlCaja.Id-Caja AND
//                CorteCaja.Turno   = CtlCaja.Turno   AND
//                CorteCaja.FecOper = CtlCaja.FecOper AND
//                CorteCaja.Id-Tp   = TipoPago.id-Tp  NO-LOCK NO-ERROR.
//
//            /* Calcula los pagos de contado por forma de pago */
//            ASSIGN 
//                l-pago = IF AVAILABLE w-Tipo THEN w-Tipo.Monto ELSE 0.
//            IF l-Pago = 0 THEN NEXT.
//
//            ASSIGN 
//                l-TotEfectivo = l-TotEfectivo + l-pago.
//
//        END.
           
            
        ASSIGN 
            l-depositos = 0
            l-pago      = 0.
        //OUTPUT TO /usr2/sis6/cortecaja.txt.    
        FOR EACH TipoPago NO-LOCK BY TipoPago.Id-tp  :
            //l-pago = 0.
            
            FIND FIRST w-Tipo WHERE w-Tipo.Id-Tp = TipoPago.Id-Tp NO-ERROR.

//            FIND FIRST CorteCaja WHERE
//                CorteCaja.Id-Caja = CtlCaja.Id-Caja AND
//                CorteCaja.Turno   = CtlCaja.Turno   AND
//                CorteCaja.FecOper = CtlCaja.FecOper AND
//                CorteCaja.Id-Tp   = TipoPago.id-Tp  NO-LOCK NO-ERROR.

            /* Calcula los pagos de contado por forma de pago */
            ASSIGN 
                l-pago = IF AVAILABLE w-Tipo THEN w-Tipo.Monto ELSE 0.
            
            //DISPLAY w-Tipo.
            
            IF l-Pago = 0 THEN NEXT.
            
            IF TipoPago.Descr MATCHES '*EFECTIVO*' THEN
                l-MontoEf = l-MontoEf + l-Pago.
        
            ACCUMULATE l-pago (TOTAL).

            IF TipoPago.Id-TP = 60 THEN NEXT.
        
            IF TipoPago.Id-Tp = 57 OR TipoPago.Id-Tp = 58 THEN ASSIGN l-depositos = l-depositos + l-pago.            

        END.
        
        //OUTPUT CLOSE.
        ASSIGN 
            ttCteCaja.TotCheque      = l-TotCheque
            ttCteCaja.TarjetaCredito = l-tcredito
            ttCteCaja.TarjetaDebito  = l-tdebito
            ttCteCaja.TotDeposito    = l-depositos
            ttCteCaja.TotEfe         = l-MontoEf.
        /* RETIROS DE EFECTIVO Y FALTANTES */
        /* EFECTIVO */
        
        ASSIGN 
            l-cantretiros = 0
            l-retiros     = 0.
        FOR EACH MovCaja WHERE MovCaja.turno = CtlCaja.turno
            AND MovCaja.id-caja = CtlCaja.Id-Caja
            AND MovCaja.FecOper = CtlCaja.FecOper 
            AND MovCaja.TipoVenta = 6
            AND MovCaja.FolioAut = 0 NO-LOCK:
            IF MovCaja.Canc THEN NEXT.
            ASSIGN 
                l-efectivo = 0.
            FOR EACH b-Mov WHERE b-Mov.turno = MovCaja.Turno AND
                b-Mov.id-caja = MovCaja.Id-caja AND
                b-Mov.FecOper = MovCaja.FecOper AND
                b-Mov.Folio   < MovCaja.Folio NO-LOCK.
                IF b-Mov.Canc THEN NEXT.                           
                IF b-Mov.TipoVenta = 1 OR b-Mov.TipoVenta = 2 THEN 
                    FOR EACH detMovC WHERE detMovC.Id-Caja = b-Mov.Id-Caja
                        AND detMovC.Folio = b-Mov.Folio 
                        AND detMovC.Id-TP = 60 NO-LOCK:
                        ASSIGN 
                            l-efectivo = l-efectivo + detMovC.MontoPAGO. 
                    END.
                IF b-Mov.TipoVenta = 6 AND b-Mov.FolioAut = 0 THEN
                    ASSIGN l-efectivo = l-efectivo - b-Mov.TotVenta.                
            END.                                
            ASSIGN 
                l-retiros     = l-retiros + MovCaja.TotVenta
                l-cantretiros = l-cantretiros + 1.
            CREATE ttRetiros.
            ASSIGN 
                ttRetiros.IdSucursal    = Depto.Id-Depto
                ttRetiros.IdCaja        = MovCaja.id-caja
                ttRetiros.Turno         = MovCaja.turno
                ttRetiros.NumRetiro     = MovCaja.Referencia
                ttRetiros.HoraRetiro    = STRING(MovCaja.HorReg,"HH:MM:SS")
                ttRetiros.ImporteRetiro = MovCaja.TotVenta. 
             
        END.
   
        ASSIGN 
            ttCteCaja.TotFaltante = (l-MontoEf - l-retiros)
            ttCteCaja.Retiros     = l-cantretiros
            ttCteCaja.TotRetiros  = l-retiros.
          
          
        ASSIGN 
            l-ticket   = 0 
            l-facturas = 0 
            l-ventas   = 0 
            l-dev      = 0.

        FOR EACH MovCaja WHERE MovCaja.Id-Caja = CtlCaja.Id-Caja AND
            MovCaja.Turno   = CtlCaja.Turno   AND
            MovCaja.FecOper = CtlCaja.FecOper NO-LOCK:
            l-SiVale = FALSE.
            IF MovCaja.TipoVenta = 2 THEN 
            DO:
                FIND Remision WHERE Remision.Id-Remision = MovCaja.Referencia 
                    NO-LOCK NO-ERROR.
                IF AVAILABLE Remision AND Remision.SustIdRemision <> "" THEN 
                DO:
                    FIND b-Remision 
                        WHERE b-Remision.Id-Remision = Remision.SustIdRemision
                        NO-LOCK NO-ERROR.
                    IF AVAILABLE b-Remision THEN 
                    DO:
                        FIND b-MovCaja WHERE b-MovCaja.Refer = b-Remision.Id-Remision
                            AND b-MovCaja.TipoVenta = b-Remision.TipoVenta
                            NO-LOCK NO-ERROR.
                        IF AVAILABLE b-MovCaja AND b-MovCaja.FecOper <> MovCaja.FecOper
                            THEN NEXT.
                    /*ELSE IF NOT AVAILABLE b-MovCaja AND b-Remision.FecReg <> MovCaja.FecOper THEN NEXT.*/
                    END.
                END.
                ELSE IF AVAILABLE Remision AND Remision.PorIdRemision <> "" THEN 
                    DO:
                        FIND b-Remision 
                            WHERE b-Remision.Id-Remision = Remision.PorIdRemision
                            NO-LOCK NO-ERROR.
                        IF AVAILABLE b-Remision THEN 
                        DO:
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
                ASSIGN l-ventas = l-ventas + MovCaja.TotVenta.
            ELSE
                IF MovCaja.TipoVenta = 1 THEN
                    ASSIGN l-ticket = l-ticket + MovCaja.TotVenta.
                ELSE
                    IF MovCaja.TipoVenta = 3 THEN
                        ASSIGN l-facturas = l-facturas + MovCaja.TotVenta.
                    ELSE
                        IF (MovCaja.TipoVenta = 9 OR MovCaja.TipoVenta = 8) THEN 
                        DO:
                            ASSIGN
                                l-dev = l-dev + MovCaja.TotVenta.
                       
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

        END.
   
   
        ASSIGN 
            ttCteCaja.TotVtaContado = l-ventas
            ttCteCaja.TotFacturas   = l-facturas
            ttCteCaja.ImporteTotal  = (l-ticket + l-ventas + l-facturas)
            ttCteCaja.TotDev        = l-dev.
   
   
        RELEASE ttRetiros.
        RELEASE ttCteCaja. 
    END.
    
    //Acumulado de cada sucursal
    FOR EACH Depto NO-LOCK:
        FIND FIRST Caja WHERE Caja.Id-depto = Depto.id-depto NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Caja THEN NEXT. 
        
        FIND FIRST Panamericano WHERE Panamericano.Id-Depto = Depto.Id-Depto AND Panamericano.FecCorte = l-Fecha NO-LOCK NO-ERROR.
        
        CREATE  ttCorteSuc.
        ASSIGN 
            ttCorteSuc.IdSucursal = Depto.id-depto
            ttCorteSuc.Sucursal   = Depto.Nombre
            ttCorteSuc.FecCorte   = l-Fecha.  
            
            
        IF AVAILABLE Panamericano THEN 
            ASSIGN ttCorteSuc.EfePanam     = Panamericano.Monto
                ttCorteSuc.Miles        = Panamericano.Miles
                ttCorteSuc.Traslado     = Panamericano.Traslado
                ttCorteSuc.Verificacion = Panamericano.Verificacion.
            
                
        FOR EACH Caja NO-LOCK WHERE Caja.Id-Depto = Depto.Id-Depto,
            EACH CtlCaja NO-LOCK
            WHERE CtlCaja.id-caja = Caja.id-caja
            AND CtlCaja.FecOper = l-Fecha BREAK BY Caja.Id-Caja:
            
            FIND FIRST ModifCorteCaja WHERE ModifCorteCaja.Id-caja = CtlCaja.Id-caja AND ModifCorteCaja.Turno = CtlCaja.Turno AND ModifCorteCaja.FecOper = CtlCaja.FecOper NO-LOCK NO-ERROR.
            
            EMPTY TEMP-TABLE w-Tipo.
                
            IF FIRST-OF (Caja.Id-Caja) THEN 
            DO:            
                ASSIGN 
                    l-MontoEf = 0.
            END.
                
            ASSIGN
                l-ValCtes     = 0
                l-ValEmpEf    = 0
                l-ValEmpNo    = 0
                l-TotVale     = 0
                l-TotVoucher  = 0
                l-TotCheque   = 0
                l-TotEfectivo = 0
                l-tcredito    = 0
                l-tdebito     = 0.
            FOR EACH MovCaja WHERE MovCaja.Turno = CtlCaja.Turno AND
                MovCaja.Id-Caja = CtlCaja.Id-caja AND
                MovCaja.Fecoper = CtlCaja.Fecoper NO-LOCK:
            
                              
                l-SiVale = FALSE.
                IF MovCaja.TipoVenta = 2 THEN 
                DO:
                    FIND Remision WHERE Remision.Id-Remision = MovCaja.Referencia 
                        NO-LOCK NO-ERROR.
                    IF AVAILABLE Remision AND Remision.SustIdRemision <> "" THEN 
                    DO:
                        FIND b-Remision 
                            WHERE b-Remision.Id-Remision = Remision.SustIdRemision
                            NO-LOCK NO-ERROR.
                        IF AVAILABLE b-Remision THEN 
                        DO:
                            FIND b-MovCaja WHERE b-MovCaja.Refer = b-Remision.Id-Remision
                                AND b-MovCaja.TipoVenta = b-Remision.TipoVenta
                                NO-LOCK NO-ERROR.
                            IF AVAILABLE b-MovCaja AND b-MovCaja.FecOper <> MovCaja.FecOper
                                THEN NEXT.
                        /*ELSE IF NOT AVAILABLE b-MovCaja AND b-Remision.FecReg <> MovCaja.FecOper THEN NEXT.*/
                        END.
                    END.
                    ELSE IF AVAILABLE Remision AND Remision.PorIdRemision <> "" THEN 
                        DO:
                            FIND b-Remision 
                                WHERE b-Remision.Id-Remision = Remision.PorIdRemision
                                NO-LOCK NO-ERROR.
                            IF AVAILABLE b-Remision THEN 
                            DO:
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
                        IF DetMovC.Mov = 'P' THEN 
                        DO:
                            FIND FIRST w-Tipo WHERE w-Tipo.Id-tp = DetMovC.Id-tp
                                NO-ERROR.
                            IF NOT AVAILABLE w-Tipo THEN 
                            DO:
                                CREATE w-Tipo.
                                ASSIGN 
                                    w-Tipo.Id-Tp = DetMovC.Id-tp.
                            END.
                 
                            ASSIGN 
                                w-Tipo.Monto = w-Tipo.Monto + DetMovC.MontoPAGO.
                            IF DetMovC.Id-tp = 52 OR DetMovC.Id-tp = 62 THEN
                                ASSIGN
                                    l-TotVoucher = l-TotVoucher + DetMovC.MontoPago.
                                    
                            IF DetMovC.Id-TP = 52 THEN ASSIGN l-tdebito = l-tdebito + DetMovC.MontoPago.        
                            IF DetMovC.Id-TP = 62 THEN ASSIGN l-tcredito = l-tcredito + DetMovC.MontoPago.
                            
                            IF DetMovC.Id-tp = 61 THEN
                                ASSIGN
                                    l-TotCheque = l-TotCheque + DetMovC.MontoPago.
                         
                            
                         
                        END.
                    END.
                IF MovCaja.TipoVenta < 3 THEN 
                DO:
                    FIND Remision WHERE Remision.Id-Remision = MovCaja.Referencia NO-LOCK NO-ERROR.
                    IF AVAILABLE Remision THEN
                        FOR EACH DetRemis WHERE DetRemis.Id-Remision = MovCaja.Refer
                            AND DetRemis.Tipo = 6
                            NO-LOCK:
                            IF DetRemis.Descr BEGINS 'VALE' THEN 
                            DO:
                                IF Remision.Id-Nomina = 0 THEN
                                    l-ValCtes = l-ValCtes + ABS(ROUND(DetRemis.Importe + DetRemis.Iva,2)).
                                ELSE IF DetRemis.Importe = 0 THEN 
                                    DO:
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
                    
            
            IF AVAILABLE ModifCorteCaja THEN 
            DO:
                IF l-TotVale <> ModifCorteCaja.ValesM THEN l-TotVale = ModifCorteCaja.ValesM.
                IF l-TotCheque <> ModifCorteCaja.ChequesM THEN l-TotCheque = ModifCorteCaja.ChequesM.
                IF l-tdebito <> ModifCorteCaja.TarjetaDebitoM THEN l-tdebito = ModifCorteCaja.TarjetaDebitoM.
                IF l-tcredito <> ModifCorteCaja.TarjetaCreditoM THEN l-tcredito = ModifCorteCaja.TarjetaCreditoM.
            END.    
            
            ACCUMULATE l-TotVale (TOTAL).
            ACCUMULATE l-TotCheque (TOTAL).            
            ACCUMULATE l-tdebito (TOTAL). 
            ACCUMULATE l-tcredito (TOTAL).
            ASSIGN 
                l-cheque    = 0
                
                l-depositos = 0.
            FOR EACH TipoPago NO-LOCK BY TipoPago.Id-tp  :
                FIND FIRST w-Tipo WHERE w-Tipo.Id-Tp = TipoPago.Id-Tp NO-ERROR.
                FIND FIRST CorteCaja WHERE
                    CorteCaja.Id-Caja = CtlCaja.Id-Caja AND
                    CorteCaja.Turno   = CtlCaja.Turno   AND
                    CorteCaja.FecOper = CtlCaja.FecOper AND
                    CorteCaja.Id-Tp   = TipoPago.id-Tp  NO-LOCK NO-ERROR.

                /* Calcula los pagos de contado por forma de pago */
                ASSIGN 
                    l-pago = IF AVAILABLE w-Tipo THEN w-Tipo.Monto ELSE 0.
                IF l-Pago = 0 THEN NEXT.
                IF TipoPago.Descr MATCHES '*EFECTIVO*' THEN
                    l-MontoEf = l-MontoEf + l-Pago.
        
                

                IF TipoPago.Id-TP = 60 THEN NEXT.
        
                //IF TipoPago.Id-TP = 52 THEN ASSIGN l-tdebito = l-tdebito + l-pago.
                IF TipoPago.Id-Tp = 57 OR TipoPago.Id-Tp = 58 THEN ASSIGN l-depositos = l-depositos + l-pago.
                //IF TipoPago.Id-Tp = 61 THEN ASSIGN l-cheque = l-cheque + l-pago.
                //IF TipoPago.Id-TP = 62 THEN ASSIGN l-tcredito = l-tcredito + l-pago.

            END.
        
            IF AVAILABLE ModifCorteCaja THEN 
            DO:
                IF l-MontoEf <> ModifCorteCaja.EfectivoM THEN l-MontoEf = ModifCorteCaja.EfectivoM.
                IF l-depositos <> ModifCorteCaja.DepositosM THEN l-depositos = ModifCorteCaja.DepositosM.                
            END.       
        
        
            ACCUMULATE l-MontoEf (TOTAL).
            ACCUMULATE l-depositos (TOTAL).  //TotalDepositos
          
            ASSIGN 
                l-ticket   = 0 
                l-facturas = 0 
                l-ventas   = 0 
                l-dev      = 0.

            FOR EACH MovCaja WHERE MovCaja.Id-Caja = CtlCaja.Id-Caja AND
                MovCaja.Turno   = CtlCaja.Turno   AND
                MovCaja.FecOper = CtlCaja.FecOper NO-LOCK:
                l-SiVale = FALSE.
                IF MovCaja.TipoVenta = 2 THEN 
                DO:
                    FIND Remision WHERE Remision.Id-Remision = MovCaja.Referencia 
                        NO-LOCK NO-ERROR.
                    IF AVAILABLE Remision AND Remision.SustIdRemision <> "" THEN 
                    DO:
                        FIND b-Remision 
                            WHERE b-Remision.Id-Remision = Remision.SustIdRemision
                            NO-LOCK NO-ERROR.
                        IF AVAILABLE b-Remision THEN 
                        DO:
                            FIND b-MovCaja WHERE b-MovCaja.Refer = b-Remision.Id-Remision
                                AND b-MovCaja.TipoVenta = b-Remision.TipoVenta
                                NO-LOCK NO-ERROR.
                            IF AVAILABLE b-MovCaja AND b-MovCaja.FecOper <> MovCaja.FecOper
                                THEN NEXT.
                        /*ELSE IF NOT AVAILABLE b-MovCaja AND b-Remision.FecReg <> MovCaja.FecOper THEN NEXT.*/
                        END.
                    END.
                    ELSE IF AVAILABLE Remision AND Remision.PorIdRemision <> "" THEN 
                        DO:
                            FIND b-Remision 
                                WHERE b-Remision.Id-Remision = Remision.PorIdRemision
                                NO-LOCK NO-ERROR.
                            IF AVAILABLE b-Remision THEN 
                            DO:
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
                    ASSIGN l-ventas = l-ventas + MovCaja.TotVenta.
                ELSE
                    IF MovCaja.TipoVenta = 1 THEN
                        ASSIGN l-ticket = l-ticket + MovCaja.TotVenta.
                    ELSE
                        IF MovCaja.TipoVenta = 3 THEN
                            ASSIGN l-facturas = l-facturas + MovCaja.TotVenta.
                        ELSE
                            IF (MovCaja.TipoVenta = 9 OR MovCaja.TipoVenta = 8) THEN 
                            DO:
                                ASSIGN
                                    l-dev = l-dev + MovCaja.TotVenta.
                       
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

            END.
   
            ACCUMULATE l-dev (TOTAL).
            ACCUMULATE (l-ticket + l-ventas + l-facturas) (TOTAL).
   
        
        END.     
        ASSIGN  
            ttCorteSuc.idsuc          = "T"
            ttCorteSuc.TotVales       = (ACCUM TOTAL l-TotVale)
            ttCorteSuc.Efevo          = (ACCUM TOTAL l-MontoEf)
            ttCorteSuc.Cheq           = (ACCUM TOTAL l-TotCheque)
            ttCorteSuc.TarjetaCredito = (ACCUM TOTAL l-tcredito)
            ttCorteSuc.TarjetaDebito  = (ACCUM TOTAL l-tdebito)
            ttCorteSuc.DLoc           = (ACCUM TOTAL l-depositos)
            ttCorteSuc.TotDev         = (ACCUM TOTAL l-dev)
            ttCorteSuc.tAmex          = ttCorteSuc.TarjetaCredit + ttCorteSuc.TarjetaDebito
            ttCorteSuc.TotalSucursal  = (ACCUM TOTAL (l-ticket + l-ventas + l-facturas))
            ttCorteSuc.TotalSucursal  = ttCorteSuc.Efevo + ttCorteSuc.Cheq + ttCorteSuc.DLoc + ttCorteSuc.tAmex  .
            
        /* Asignación de termi y suc según IdSucursal */
        CASE INTEGER(ttCorteSuc.IdSucursal):
            WHEN 402 THEN 
                ASSIGN 
                    ttCorteSuc.termi = 1 
                    ttCorteSuc.suc   = "ADMVAS".
            WHEN 204 THEN 
                ASSIGN 
                    ttCorteSuc.termi = 2 
                    ttCorteSuc.suc   = "MATRIZ".
            WHEN 506 THEN 
                ASSIGN 
                    ttCorteSuc.termi = 3 
                    ttCorteSuc.suc   = "SALTILLO".
            WHEN 510 THEN 
                ASSIGN 
                    ttCorteSuc.termi = 4 
                    ttCorteSuc.suc   = "CHIHUAHUA".
            WHEN 511 THEN 
                ASSIGN 
                    ttCorteSuc.termi = 5 
                    ttCorteSuc.suc   = "P.LIVAS".
            WHEN 512 THEN 
                ASSIGN 
                    ttCorteSuc.termi = 6 
                    ttCorteSuc.suc   = "RUIZ C.".
            WHEN 513 THEN 
                ASSIGN 
                    ttCorteSuc.termi = 7 
                    ttCorteSuc.suc   = "CUMBRES".
            WHEN 514 THEN 
                ASSIGN 
                    ttCorteSuc.termi = 8 
                    ttCorteSuc.suc   = "D.DIAZ".
            WHEN 515 THEN 
                ASSIGN 
                    ttCorteSuc.termi = 9 
                    ttCorteSuc.suc   = "C.ANAHUAC".
        END CASE.
            
     
        RELEASE ttCorteSuc. 
       /* Primero inicializamos la tabla ttCorte */
    EMPTY TEMP-TABLE ttCorte.
    CREATE ttCorte.
    ASSIGN 
        ttCorte.TotalCorte    = 0
        ttCorte.TotalEfectivo = 0
        ttCorte.TotalTarjeta  = 0
        ttCorte.TotalTransf   = 0
        ttCorte.TotalCheque   = 0.

    /* Luego acumulamos los valores de todas las sucursales */
    FOR EACH ttCorteSuc WHERE ttCorteSuc.termi <> 0 NO-LOCK:
        ASSIGN 
            ttCorte.TotalCorte    = ttCorte.TotalCorte + ttCorteSuc.TotalSucursal
            ttCorte.TotalEfectivo = ttCorte.TotalEfectivo + ttCorteSuc.Efevo
            ttCorte.TotalTarjeta  = ttCorte.TotalTarjeta + ttCorteSuc.tAmex
            ttCorte.TotalTransf   = ttCorte.TotalTransf + ttCorteSuc.DLoc
            ttCorte.TotalCheque   = ttCorte.TotalCheque + ttCorteSuc.Cheq
            ttCorte.idsuc         = "T".
    END.
          
END.

    
    
    
    
    
    
    
    
    
    
    DEFINE VARIABLE l-lista AS CHARACTER.
    l-lista = "1,2,4,5,6,7,8,9,10,11".

    FOR EACH Cliente NO-LOCK : 
        IF INDEX(l-lista, STRING(Cliente.Id-Cliente)) > 0 THEN 
        DO:
            /* Inicializar la variable de saldo */
            ASSIGN 
                l-saldo = 0.

            FOR EACH Movcliente WHERE Movcliente.id-cliente = Cliente.id-cliente AND
                Movcliente.FecReg <= TODAY                 AND
                MovCliente.Id-MC  <= 3                     AND
                MovCliente.Afectado                       
                NO-LOCK  BREAK  BY Cliente.RazonSocial 
                BY Cliente.id-cliente
                BY MovCliente.Id-Cliente:
            
                IF MovCliente.Id-MC <= 3 THEN 
                DO:
                    FOR EACH bff-mov WHERE bff-mov.RefSaldo = MovCliente.RefSaldo
                        AND bff-mov.Id-MC    > 3 
                        AND bff-mov.Afectado  
                        AND bff-mov.FecReg  <= TODAY  NO-LOCK:
                        FIND Acuse WHERE Acuse.Id-Acuse = bff-mov.Documento NO-LOCK NO-ERROR.
                        IF AVAILABLE Acuse THEN
                            IF  Acuse.Estatus <> 4 THEN NEXT.
                        ACCUMULATE bff-mov.importe (TOTAL).
                    END.
                    ASSIGN
                        l-saldo = Movcliente.Importe + (ACCUM TOTAL bff-mov.Importe).
                    IF Movcliente.Id-Moneda > 1 THEN
                        ASSIGN l-saldo = l-saldo * MovCliente.TipoCambio.
      
                END. 
               
                IF l-saldo > 0 THEN 
                DO:
                    FIND FIRST ttSaldoSuc WHERE ttSaldoSuc.Cliente = MovCliente.Id-Cliente NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE ttSaldoSuc THEN 
                    DO:
                        CREATE ttSaldoSuc.
                        ASSIGN 
                            ttSaldoSuc.Cliente = MovCliente.Id-Cliente
                            ttSaldoSuc.idsuc   = "T"
                            ttSaldoSuc.Saldo   = l-saldo.
                        /* Aquí hacemos el mapeo de sucursales */
                        CASE Cliente.Id-Cliente:
                            WHEN 1 THEN 
                                ttSaldoSuc.Sucursal = "MATRIZ".
                            WHEN 4 THEN 
                                ttSaldoSuc.Sucursal = "SALTILLO".
                            WHEN 5 THEN 
                                ttSaldoSuc.Sucursal = "CHIHUAHUA".
                            WHEN 6 THEN 
                                ttSaldoSuc.Sucursal = "PABLO LIVAS".
                            WHEN 7 THEN 
                                ttSaldoSuc.Sucursal = "RUIZ CORTINEZ".
                            WHEN 8 THEN 
                                ttSaldoSuc.Sucursal = "CUMBRES".
                            WHEN 9 THEN 
                                ttSaldoSuc.Sucursal = "DIEGO DIAZ".
                            WHEN 10 THEN 
                                ttSaldoSuc.Sucursal = "CERRADAS DE ANAHUAC".
                            OTHERWISE 
                            ttSaldoSuc.Sucursal = Cliente.RazonSocial.
                        END CASE.   
                                   
                                  // ttSaldoSuc.Sucursal = Cliente.RazonSocial .  
                    END.
                    ELSE 
                    DO:
                         
                        ASSIGN 
                            ttSaldoSuc.Saldo = ttSaldoSuc.Saldo + l-saldo.
                    END.
                
                END.
            END.
        END.
    END.
    DEFINE VARIABLE iFecha         AS DATE      NO-UNDO.
    DEFINE VARIABLE l-Depositos    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE l-Total2       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE l-SubTot       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE l-Deudores     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE l-SubTotME     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE l-total1       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE l-totME        AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE l-totMEMN      AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE l-totMEMNFac   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE l-totMEMNPago  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE l-totMNpagoUS  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE l-totFacMNenUS AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE l-acuselst     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE l-acuseMNenUS  AS CHARACTER NO-UNDO.

    /* Recorremos desde l-Fecha hacia atrás hasta 30 días antes */
    DO iFecha = l-Fecha TO (l-Fecha - 30) BY -1:

        /* Inicializamos variables por día */
        ASSIGN
            l-Depositos    = 0
            l-total1       = 0
            l-totME        = 0
            l-totMEMN      = 0
            l-totMEMNFac   = 0
            l-totMEMNPago  = 0
            l-totMNpagoUS  = 0
            l-totFacMNenUS = 0
            l-acuselst     = ""
            l-acuseMNenUS  = ""
            l-Total2       = 0
            l-SubTot       = 0
            l-Deudores     = 0
            l-SubTotME     = 0.

        FOR EACH Acuse 
            WHERE Acuse.FecDep = iFecha 
            AND Acuse.Estatus = 4 NO-LOCK:

            IF Acuse.Tipo = "C" THEN NEXT.

            FOR EACH DocAcuse OF Acuse NO-LOCK:
                FIND FIRST PagoAcuse OF Acuse 
                    WHERE PagoAcuse.Id-Moneda > 1 NO-LOCK NO-ERROR.

                IF AVAILABLE PagoAcuse AND DocAcuse.Id-Moneda <= 1 THEN 
                DO:
                    IF LOOKUP(DocAcuse.Id-Acuse, l-acuseMNenUS) = 0 THEN
                        l-acuseMNenUS = l-acuseMNenUS + ',' + STRING(DocAcuse.Id-Acuse).
                    l-totFacMNenUS = l-totFacMNenUS + DocAcuse.ImpPago.
                END.
                ELSE IF DocAcuse.Id-Moneda > 1 THEN 
                    DO:
                        IF LOOKUP(DocAcuse.Id-Acuse, l-acuselst) = 0 THEN
                            l-acuselst = l-acuselst + ',' + STRING(DocAcuse.Id-Acuse).
                        l-totMEMNFac = l-totMEMNFac + ROUND(DocAcuse.ImpPago * DocAcuse.TipoCambio, 2).
                        l-totME = l-totME + DocAcuse.ImpPago.
                    END.
            END.

            FOR EACH PagoAcuse OF Acuse NO-LOCK:
                FIND TipoPago OF PagoAcuse NO-LOCK NO-ERROR.
                IF AVAILABLE TipoPago AND PagoAcuse.Id-Tp <> 50 THEN 
                DO:
                    IF LOOKUP(PagoAcuse.Id-Acuse, l-acuselst) > 0 AND PagoAcuse.Id-Moneda = 1 THEN
                        l-totMEMNPago = l-totMEMNPago + (PagoAcuse.Importe * PagoAcuse.TC).

                    IF LOOKUP(PagoAcuse.Id-Acuse, l-acuseMNenUS) > 0 AND PagoAcuse.Id-Moneda > 1 THEN
                        l-totMNpagoUS = l-totMNpagoUS + PagoAcuse.Importe.

                    ELSE IF PagoAcuse.Id-Moneda > 1 THEN 
                        DO:
                            ACCUMULATE ROUND((PagoAcuse.Importe * PagoAcuse.TC) * PagoAcuse.TipoCambio, 2) (TOTAL).
                            ACCUMULATE PagoAcuse.Importe (TOTAL).
                        END.
                        ELSE
                            ACCUMULATE PagoAcuse.Importe * PagoAcuse.TC (TOTAL).
                END.
            END.

            ASSIGN 
                l-total1  = (ACCUM TOTAL PagoAcuse.Importe * PagoAcuse.TC)
                l-totME   = l-totME + (ACCUM TOTAL PagoAcuse.Importe)
                l-totMEMN = l-totMEMN + (ACCUM TOTAL ROUND((PagoAcuse.Importe * PagoAcuse.TC) * PagoAcuse.TipoCambio, 2)).

            ACCUMULATE l-total1 (TOTAL).
        END.

        l-Depositos = ACCUM TOTAL l-total1.

        /* Procesamos FichaDep de ese día */
        FOR EACH b-FichaDep 
            WHERE b-FichaDep.FecReg = iFecha NO-LOCK:

            IF b-FichaDep.Clave = 'C' THEN 
            DO:
                IF b-FichaDep.Tipo = 6 THEN
                    ACCUMULATE b-FichaDep.Importe (TOTAL).
                ELSE 
                    l-SubTot = l-SubTot + b-FichaDep.Importe.
            END.
            ELSE 
                l-Deudores = l-Deudores + b-FichaDep.Importe.

            l-Total2 = l-Total2 + b-FichaDep.Importe.
        END.

        /* Guardamos un registro en la tabla temporal por fecha */
        CREATE ttDepositos.
        ASSIGN 
            ttDepositos.idsuc           = "T"
            ttDepositos.Fecha           = iFecha
            ttDepositos.TotalDepositado = l-Depositos
            ttDepositos.DepositoTotal   = l-Total2
            ttDepositos.Diferencia      = l-Depositos - l-Total2.
    END.   

    FOR EACH Remision WHERE Remision.Pagada = FALSE 
        AND Remision.TipoVenta = 2 AND Remision.FecReg >= l-Fecha
        AND Remision.FecReg <= l-Fecha
        AND (IF l-Index = 4 THEN Remision.Id-Vendedor = "0100" ELSE (IF l-Index = 1 THEN Remision.Id-Vendedor <> "0100" ELSE TRUE))
        NO-LOCK BY Remision.Id-Remision:

        IF Remision.Feccanc <> ? THEN NEXT.
        IF remision.tipoventa = 1 AND remision.id-Remis BEGINS "@" THEN NEXT.
        IF remision.tipoventa = 2 AND CAN-DO("0,1,2,3,4,5,6,7,8,9",SUBSTRING(remision.id-remis,7,1)) THEN NEXT.
        IF NOT Remision.Id-Remision MATCHES "*J" AND l-Index = 3 THEN NEXT. 
        PAUSE 0.
        DISPLAY Remision.Id-Remision WITH FRAME f-sel SIDE-LABELS OVERLAY ROW 10 CENTERED.
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

        l-suma = l-suma + Remision.Tot.
  
    END. // FOR EACH
    FIND FIRST ttDepositos WHERE ttDepositos.idsuc = "T" NO-LOCK NO-ERROR.
    IF AVAILABLE ttDepositos THEN
        ASSIGN 
            ttCorte.TotalFacturas = l-suma.        
   
END PROCEDURE.   



