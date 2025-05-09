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
DEFINE VARIABLE l-Archivo AS CHARACTER NO-UNDO.

DEFINE VARIABLE Acu       AS DECIMAL.
DEFINE VARIABLE Efvo      AS DECIMAL.
DEFINE VARIABLE NCred     AS DECIMAL.
DEFINE VARIABLE l-NCr8    AS DECIMAL.
DEFINE VARIABLE DevTC     AS DECIMAL.
DEFINE VARIABLE Amex      AS DECIMAL.
DEFINE VARIABLE Amig      AS DECIMAL.
DEFINE VARIABLE Cred      AS DECIMAL.
DEFINE VARIABLE Cheq      AS DECIMAL.
DEFINE VARIABLE DebT      AS DECIMAL.
DEFINE VARIABLE CheqPF    AS DECIMAL.
DEFINE VARIABLE CheqPFA   AS DECIMAL.
DEFINE VARIABLE DLoc      AS DECIMAL   NO-UNDO.
DEFINE VARIABLE DFor      AS DECIMAL   NO-UNDO.

DEFINE VARIABLE l-Caja    LIKE MovCaja.Id-Caja NO-UNDO.
DEFINE VARIABLE l-TP      LIKE DetMovC.Id-TP NO-UNDO.    
 
DEFINE VARIABLE l-Cajas   AS CHARACTER EXTENT 16.
DEFINE VARIABLE l-Linea   AS CHARACTER EXTENT 16.
DEFINE VARIABLE l-Linea2  AS CHARACTER EXTENT 16.
DEFINE VARIABLE l-Ticket  AS CHARACTER EXTENT 16.
DEFINE VARIABLE l-Factura AS CHARACTER EXTENT 16.
DEFINE VARIABLE l-Total   AS CHARACTER EXTENT 16.
DEFINE VARIABLE l-Dife    AS CHARACTER EXTENT 16.
DEFINE VARIABLE v-Tic     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-Fac     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-Dif     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE l-Term    AS INTEGER. 

DEFINE VARIABLE l-nomSuc  AS CHARACTER NO-UNDO.

DEFINE VARIABLE TDebT     AS DECIMAL.
DEFINE VARIABLE tNCred    AS DECIMAL.
DEFINE VARIABLE l-TNCr8   AS DECIMAL.
DEFINE VARIABLE tDevTC    AS DECIMAL.
DEFINE VARIABLE TCCred    AS DECIMAL.
DEFINE VARIABLE TAmig     AS DECIMAL.
DEFINE VARIABLE tAmex     AS DECIMAL.
DEFINE VARIABLE tCheq     AS DECIMAL.
DEFINE VARIABLE tEfvo     AS DECIMAL.
DEFINE VARIABLE tChPF     AS DECIMAL.
DEFINE VARIABLE tChPFA    AS DECIMAL.
DEFINE VARIABLE tDLoc     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE tDFor     AS DECIMAL   NO-UNDO.

DEF VAR l-cf AS INTEGER NO-UNDO.
DEF VAR l-Refer1    AS   CHAR                 NO-UNDO FORMAT 'x(6)'.
DEF VAR l-Articulo  LIKE Articulo.Id-Articulo NO-UNDO.
DEF VAR l-Fecha AS DATE FORMAT "99/99/9999" NO-UNDO.
DEF VAR l-FrameValue AS CHAR         NO-UNDO.
DEF VAR l-SubTot LIKE FichaDep.Importe NO-UNDO.
DEF VAR l-Depositos LIKE PagoAcuse.Importe NO-UNDO.
DEF VAR l-Deudores LIKE PagoAcuse.Importe NO-UNDO.
DEF VAR l-totME      AS DECIMAL FORMAT "zzzzz,zz9.99" NO-UNDO.      /*_ RNPC _*/
DEF VAR l-totMEMN    AS DECIMAL FORMAT "zzzzz,zz9.99" NO-UNDO.      /*_ RNPC _*/
DEF VAR l-totMEFac   AS DECIMAL FORMAT "zzzzz,zz9.99" NO-UNDO.      /*_ RNPC _*/
DEF VAR l-totMEMNFac AS DECIMAL FORMAT "zzzzz,zz9.99" NO-UNDO.      /*_ RNPC _*/
DEF VAR l-totFacMNenUS AS DECIMAL FORMAT "zzzzz,zz9.99" NO-UNDO.      /*_ RNPC _*/
DEF VAR l-totMNpagoUS AS DECIMAL FORMAT "zzzzz,zz9.99" NO-UNDO.     /*_ RNPC _*/
DEF VAR l-totMEMNPago AS DECIMAL FORMAT "zzzzz,zz9.99" NO-UNDO.     /*_ RNPC _*/
DEF VAR l-moneda    AS CHARACTER FORMAT "x(3)" NO-UNDO INITIAL "".  /*_ RNPC _*/
DEF VAR l-acuselst  AS CHARACTER NO-UNDO INITIAL "".                /*_ RNPC _*/
DEF VAR l-acuseMNenUS AS CHARACTER NO-UNDO INITIAL "".                /*_ RNPC _*/
DEF VAR l-SubTotME LIKE FichaDep.Importe NO-UNDO.                   /*_ RNPC _*/
DEF VAR l-total1 LIKE PagoAcuse.Importe NO-UNDO.
DEF VAR l-total2 LIKE PagoAcuse.Importe NO-UNDO.
DEF VAR l-ImpDeud LIKE FichaDep.Importe NO-UNDO.
DEF VAR l-Sub1 LIKE FichaDep.Importe NO-UNDO.
DEF VAR l-Sub2 LIKE FichaDep.Importe NO-UNDO.
DEF VAR l-Sub3 LIKE FichaDep.Importe NO-UNDO.
DEF VAR l-Tot1 LIKE FichaDep.Importe NO-UNDO.
DEF VAR l-Tot2 LIKE FichaDep.Importe NO-UNDO.
DEF VAR l-Tot3 LIKE FichaDep.Importe NO-UNDO.
DEF VAR l-Reporte AS CHAR NO-UNDO.
DEF VAR l-Tipo AS CHAR NO-UNDO FORMAT "x(3)".
DEF VAR l-Poliza AS CHAR NO-UNDO FORMAT "x(6)".
DEF VAR l-NPol LIKE Poliza.Id-Poliza NO-UNDO.
DEF VAR l-NSer LIKE Poliza.Serie NO-UNDO.
DEF VAR l-NS LIKE Cuenta.Id-SCta NO-UNDO.
DEF VAR l-cta    LIKE MovPoliza.Id-Cta NO-UNDO.
DEF VAR l-Scta   LIKE MovPoliza.Id-SCta NO-UNDO.
DEF VAR l-SScta  LIKE MovPoliza.Id-SSCta NO-UNDO.
DEF VAR l-SSScta LIKE MovPoliza.Id-SSSCta NO-UNDO.
DEF VAR l-Sec    LIKE MovPoliza.Sec NO-UNDO.
DEF VAR l-cargo    AS DECI FORMAT "zz,zzz,zz9.99".
DEF VAR l-TMax AS INTEGER NO-UNDO.

DEFINE VARIABLE l-FecIni     AS DATE FORMAT "99/99/9999" NO-UNDO.
DEFINE VARIABLE l-FecFin     AS DATE FORMAT "99/99/9999" NO-UNDO.
DEFINE VARIABLE l-veces      AS INT  NO-UNDO INITIAL 0.
DEFINE VARIABLE l-Camion     AS INTEGER FORMAT "zz9" NO-UNDO.
DEFINE VARIABLE l-FolEmb     LIKE Embarque.Id-Embarque NO-UNDO.
DEFINE VARIABLE l-FecEmb     AS DATE FORMAT '99/99/9999' NO-UNDO.
DEFINE VARIABLE l-index      AS INTEGER.
DEFINE VARIABLE l-index2     AS INTEGER.
DEFINE VARIABLE l-HorEmb     AS CHARACTER FORMAT "x(5)" NO-UNDO.
DEFINE VARIABLE l-CantEmb    AS INTEGER NO-UNDO.
DEFINE VARIABLE l-Estatus    LIKE DetEmbarque.Estatus NO-UNDO.
DEFINE VARIABLE l-SVeces     AS CHARACTER NO-UNDO FORMAT "x(2)".
DEFINE VARIABLE l-Aster      AS CHARACTER NO-UNDO FORMAT "x(1)".
DEFINE VARIABLE l-Prim       AS LOGICAL NO-UNDO.
DEFINE VARIABLE l-NOExis     AS LOGICAL NO-UNDO.
DEFINE VARIABLE l-Mtto       AS INTEGER NO-UNDO.
DEFINE VARIABLE l-SoloTrasp  AS LOGICAL NO-UNDO.
DEFINE VARIABLE l-ListDev    AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-TotDev     AS DECIMAL NO-UNDO.
DEFINE VARIABLE l-opcion     AS CHARACTER EXTENT 4 FORMAT "x(16)" INITIAL ["Normal", "Condensado Gral.", "Condensado Chih.", "Internet"] NO-UNDO.
DEFINE VARIABLE l-opcion2    AS CHARACTER EXTENT 2 FORMAT "x(10)" INITIAL ["Remisiones", "Facturas"] NO-UNDO.
DEFINE VARIABLE l-PagInfo LIKE Pedido.PagInfo NO-UNDO.
DEFINE VARIABLE l-FecPed AS DATE NO-UNDO.
DEFINE VARIABLE l-TotPed AS DECIMAL NO-UNDO.
DEFINE VARIABLE l-Sobrante AS DECIMAL NO-UNDO.
DEFINE VARIABLE l-Pedidos AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-ReqCte AS CHARACTER FORMAT "X(6)" LABEL "ReqCte". 

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
    FIELD idsuc         AS CHARACTER 
    FIELD termi         AS INTEGER
    FIELD suc           AS CHAR 
    FIELD Efevo         LIKE DetMovC.MontoPago LABEL "Efectivo"
    FIELD tAmex         LIKE DetMovC.MontoPago LABEL "T.Amex"
    FIELD DLoc          LIKE DetMovC.MontoPago LABEL "D.Local"
    FIELD Cheq          LIKE DetMovC.MontoPago LABEL "Cheque"
    FIELD CajaChica     LIKE DetMovC.MontoPago
    FIELD TotalSucursal LIKE DetMovC.MontoPago.

DEFINE TEMP-TABLE ttSaldoSuc
    FIELD idsuc         AS CHARACTER 
    FIELD Cliente       LIKE Cliente.Id-Cliente
    FIELD Sucursal      LIKE Cliente.RazonSocial
    FIELD Saldo         LIKE DetMovC.MontoPago.

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

DEFINE VARIABLE l-suma   AS DECIMAL NO-UNDO.

DEF    BUFFER bf-cli    FOR Cliente .  
DEF    BUFFER bff-mov   FOR MovCliente. 
   
DEFINE BUFFER bf-cajas  FOR ttCorteSuc.
DEFINE BUFFER b-DetMovC FOR DetMovC.
DEFINE BUFFER bf-mov    FOR MovCaja.
DEFINE BUFFER rm-Mov    FOR tt-MovCaja.   
  

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

    DEFINE VAR l-Suc AS CHARACTER NO-UNDO FORMAT 'x(1)'.



  
    EMPTY TEMP-TABLE ttCorteSuc.
    EMPTY TEMP-TABLE ttCorte.
    EMPTY TEMP-TABLE tt-Cajas2.
    EMPTY TEMP-TABLE tt-MovCaja.

    l-Cajas = "".
    l-Linea = "".
    l-Linea2 = "". 
    l-Total = "".
    l-Term = 0. 

    acu = 0.
    Efvo = 0.
    NCred = 0.
    l-NCr8 = 0.
    DevTC = 0.
    Amex = 0.
    Amig = 0.
    Cred = 0.
    Cheq = 0.
    DebT = 0.
    CheqPF = 0.
    CheqPFA = 0.
    DLoc = 0.
    DFor = 0.
  
    TDebT = 0.
    TCCred = 0.
    TNCred = 0.
    l-TNCr8 = 0.
    TAmig = 0.
    tAmex = 0.
    tCheq = 0.
    tEfvo = 0.
    tChPF = 0.
    tChPFA = 0.
    tDLoc = 0.
    tDFor = 0.
  

    ASSIGN 
        l-suc = "T".
    FOR EACH movcaja WHERE MovCaja.fecoper = l-Fecha  
        AND MovCaja.Canc = FALSE
        AND MovCaja.tipoventa <> 6 
        AND MovCaja.tipoventa <> 3 NO-LOCK:  
                         
        IF CAN-DO("22,44,56,58,65,70,80",STRING(MovCaja.Id-Caja)) THEN NEXT.              
     
        CREATE tt-MovCaja.
        BUFFER-COPY Movcaja TO tt-MovCaja.
  
        IF CAN-DO("1,2,3,4,5,6,7,8,16,17,18,33",STRING(MovCaja.Id-Caja)) THEN 
            ASSIGN tt-MovCaja.Sucursal = "B" //"M"
                tt-MovCaja.SucDesc  = "MATRIZ". 
        IF CAN-DO("9,12,13,14,19,20,21,23,77",STRING(MovCaja.Id-Caja)) THEN 
            ASSIGN tt-MovCaja.Sucursal = "C" //"S"
                tt-MovCaja.SucDesc  = "SALTILLO".
        IF CAN-DO("10,11,15,55,66,88",STRING(MovCaja.Id-Caja)) THEN 
            ASSIGN tt-MovCaja.Sucursal = "A"
                tt-MovCaja.SucDesc  = "ADMVAS".
        IF CAN-DO("50,51,52,53,54,57,59",STRING(MovCaja.Id-Caja)) THEN 
            ASSIGN tt-MovCaja.Sucursal = "D" //"C"
                tt-MovCaja.SucDesc  = "CHIHUAHUA".   
        IF CAN-DO("60,61,62,63,64,67,68,69",STRING(MovCaja.Id-Caja)) THEN 
            ASSIGN tt-MovCaja.Sucursal = "E" //"P"
                tt-MovCaja.SucDesc  = "P.LIVAS".
        IF CAN-DO("71,72,73,74,75,76,78,79",STRING(MovCaja.Id-Caja)) THEN 
            ASSIGN tt-MovCaja.Sucursal = "F" //"R"
                tt-MovCaja.SucDesc  = "RUIZ C.".
        IF CAN-DO("81,82,83,84,85,86",STRING(MovCaja.Id-Caja)) THEN 
            ASSIGN tt-MovCaja.Sucursal = "G" //"Q"
                tt-MovCaja.SucDesc  = "CUMBRES".
        IF CAN-DO("91,92,93,94,95,96",STRING(MovCaja.Id-Caja)) THEN 
            ASSIGN tt-MovCaja.Sucursal = "H" //"D"
                tt-MovCaja.SucDesc  = "D.DIAZ".
        IF CAN-DO("101,102,103,104,105,106",STRING(MovCaja.Id-Caja)) THEN 
            ASSIGN tt-MovCaja.Sucursal = "I" // "N"
                tt-MovCaja.SucDesc  = "C.ANAHUAC".
    END. 
                       


    FOR EACH tt-MovCaja WHERE tt-MovCaja.fecoper = l-Fecha 
        AND tt-MovCaja.Canc = FALSE
        AND tt-MovCaja.tipoventa <> 6 
        AND tt-MovCaja.tipoventa <> 3 NO-LOCK BREAK BY tt-MovCaja.Sucursal:    
                           


        FOR EACH b-DetMovC WHERE b-DetMovC.Id-Caja = tt-MovCaja.Id-Caja
            AND b-DetMovC.Id-TP = 61
            AND b-DetMovC.FecCheque = l-fecha:
                                 
            FIND FIRST bf-mov WHERE bf-mov.Id-Caja = b-DetMovC.Id-Caja
                AND bf-mov.folio = b-DetMovC.folio 
                AND bf-mov.fecoper < l-fecha NO-LOCK NO-ERROR.
            IF AVAILABLE bf-mov THEN 
            DO:
                CheqPFA = CheqPFA + b-DetMovC.MontoPago.
            END.                                       
        END.
    
        IF tt-MovCaja.TipoVenta = 8 OR tt-MovCaja.TipoVenta = 9 THEN
            NCred = NCred + tt-MovCaja.Tot.
        
        IF tt-MovCaja.TipoVenta = 8 THEN
            l-NCr8 = l-NCr8 + tt-MovCaja.Tot.
        
        IF tt-MovCaja.TipoVenta = 10 THEN
            DevTC = DevTC + tt-MovCaja.Tot.
        
        FOR EACH DetMovC WHERE DetMovC.Id-Caja = tt-MovCaja.Id-Caja
            AND DetMovC.folio = tt-MovCaja.folio
            AND DetMovC.mov = "P" NO-LOCK:

            IF DetMovC.Id-TP = 60 THEN 
                Efvo = Efvo + DetMovC.MontoPago.
   
            IF DetMovC.Id-TP = 62 THEN
                Amex = Amex + DetMovC.MontoPago.
           
            IF DetMovC.Id-TP = 52 THEN  /* TARJETA DEBITO */
                Amex = Amex + DetMovC.MontoPago.
        
            /* CHEQUES */     
            IF DetMovC.Id-TP = 61 THEN
                CheqPFA = CheqPFA + DetMovC.MontoPago.
               
            IF DetMovC.Id-TP = 61 AND DetMovC.FecCheque > tt-MovCaja.fecoper THEN
                CheqPFA = CheqPFA + DetMovC.MontoPago.   
                  
        
            /* TRANSFERENCIAS */      
            IF DetMovC.Id-TP = 57 OR DetMovC.Id-TP = 58 THEN
                DLoc = DLoc + DetMovC.MontoPago.
                                    
            Acu = Acu + DetMovC.MontoPago.      
        END.        

        IF LAST-OF(tt-MovCaja.Sucursal) THEN 
        DO:   
            l-Term = l-Term + 1.
              
            FOR EACH Remision WHERE Remision.FecReg = l-Fecha
                AND Remision.CancRec = FALSE NO-LOCK,
                EACH rm-Mov WHERE rm-Mov.Referencia = Remision.id-Remision
                AND rm-Mov.Sucursal = tt-MovCaja.Sucursal
                AND rm-Mov.Canc = FALSE NO-LOCK:
                IF rm-Mov.TipoVenta = 1 THEN 
                    ASSIGN v-Tic            = v-Tic + Remision.Tot 
                        l-Ticket[l-Term] = STRING(DECIMAL(l-Ticket[l-Term]) + Remision.Tot, ">>>>,>>9.99").
                IF rm-Mov.TipoVenta = 2 THEN
                    ASSIGN  v-Fac             = v-Fac + Remision.Tot
                        l-Factura[l-Term] = STRING(DECIMAL(l-Factura[l-Term]) + Remision.Tot, ">>>>,>>9.99").
            END.     
                       
            tNCred = tNCred + NCred.
            l-TNCr8 = l-TNCr8 + l-NCr8.
            tDevTC = tDevTC + DevTC.
            tEfvo = tEfvo + Efvo.        /* EFECTIVO */
            tAmex = tAmex + Amex.       /* TARJETAS */
            tChPFA = tChPFA + CheqPFA.  /* CHEQUES */
            tDLoc = tDLoc + DLoc.       /* TRASNSFERENCIAS */
                     
            CREATE ttCorteSuc.
            ASSIGN 
                ttCorteSuc.idsuc = l-suc 
                ttCorteSuc.Efevo = Efvo    /* EFECTIVO */
                ttCorteSuc.tAmex = Amex   /* TARJETAS */
                ttCorteSuc.DLoc  = DLoc    /* TRANSFERENCIAS */
                ttCorteSuc.Cheq  = CheqPFA  /* CHEQUES */
                ttCorteSuc.termi = l-Term
                ttCorteSuc.suc   = STRING(tt-MovCaja.SucDesc).
                
            ASSIGN 
                l-Cajas[l-Term]          = STRING(tt-MovCaja.SucDesc)
                l-Linea[l-Term]          = "-----------"
                l-Linea2[l-Term]         = "-----------"
                ttCorteSuc.TotalSucursal = (Efvo + Amex + CheqPFA + DLoc)
                l-Total[l-Term]          = STRING(Efvo + Amex + CheqPFA + DLoc , ">>>>,>>9.99").
                        
            ASSIGN 
                l-Dife[l-Term] = STRING(DECIMAL(l-Total[l-Term]) + DECIMAL(NCred) - (DECIMAL(l-Ticket[l-Term]) + DECIMAL(l-Factura[l-Term])), "->>>>,>>9.99")
                v-Dif          = v-Dif + DECIMAL(l-Dife[l-Term]).       
                        
            Acu = 0.   
            NCred = 0.
            l-NCr8 = 0.
            DevTC = 0.
            Efvo = 0.
            Amex = 0. 
            Cred = 0.
            Amig = 0.
            Cheq = 0.
            DebT = 0.
            DLoc = 0.
            DFor = 0.
            CheqPF = 0.
            CheqPFA = 0.
            v-Fac = 0.
            v-Tic = 0.       
        END.      
    END.
    
    FOR EACH ttCorteSuc BREAK BY ttCorteSuc.idsuc:
        IF FIRST-OF(ttCorteSuc.idsuc) THEN 
        DO:
            CREATE tt-Cajas2.
            FOR EACH bf-cajas:
                ASSIGN 
                    tt-Cajas2.Efevo[bf-cajas.termi]   = STRING(bf-cajas.Efevo, ">>>>,>>9.99")
                    tt-Cajas2.DLoc[bf-cajas.termi]    = STRING(bf-cajas.DLoc, ">>>>,>>9.99")
                    tt-Cajas2.tAmex[bf-cajas.termi]   = STRING(bf-cajas.tAmex, ">>>>,>>9.99")
                    tt-Cajas2.CheqPFA[bf-cajas.termi] = STRING(bf-cajas.Cheq, ">>>>,>>9.99").
            END. 
        END.
        IF LAST-OF(ttCorteSuc.idsuc)THEN 
        DO:
            ttCorteSuc.termi = ttCorteSuc.termi + 1.
            ASSIGN 
                tt-Cajas2.Efevo[ttCorteSuc.termi] = STRING(tEfvo, ">>>>,>>9.99")
                tt-Cajas2.tAmex[ttCorteSuc.termi] = STRING(tAmex, ">>>>,>>9.99")
                tt-Cajas2.Cheq[ttCorteSuc.termi]  = STRING(tChPFA, ">>>>,>>9.99")
                tt-Cajas2.DLoc[ttCorteSuc.termi]  = STRING(tDLoc, ">>>>,>>9.99").
            l-Linea[ttCorteSuc.termi] = "-----------".     
            l-Linea2[ttCorteSuc.termi] = "-----------".
            l-Cajas[ttCorteSuc.termi] = " Total ".
            l-Total[ttCorteSuc.termi] = STRING(tEfvo + tAmex + tChPFA + tDLoc, ">>>>,>>9.99").
            l-Ticket[ttCorteSuc.termi] = STRING(v-Tic, ">>>>,>>9.99").
            l-Factura[ttCorteSuc.termi] = STRING(v-Fac, ">>>>,>>9.99").      
            l-Dife[ttCorteSuc.termi] = STRING(v-Dif, "->>>>,>>9.99").
        END.    
    END.     

    DEFINE VARIABLE totalEfevo    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE totalAmex     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE totalDLoc     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE totalCheq     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE totalCaja     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE totalFacturas AS DECIMAL NO-UNDO.
    DEFINE VARIABLE totalCorte    AS DECIMAL NO-UNDO.

    FOR EACH ttCorteSuc NO-LOCK:

        /* Sumar los totales de tt-Cajas */
        totalEfevo   = totalEfevo + ttCorteSuc.Efevo.
        totalAmex    = totalAmex + ttCorteSuc.tAmex.
        totalDLoc    = totalDLoc + ttCorteSuc.DLoc.
        totalCheq    = totalCheq + ttCorteSuc.Cheq.

    END.

    CREATE ttCorte.
    ASSIGN 
        ttCorte.idsuc         = "T"  /* O el valor que quieras para identificar el total */
        ttCorte.TotalEfectivo = totalEfevo
        ttCorte.TotalTarjeta  = totalAmex
        ttCorte.TotalTransf   = totalDLoc
        ttCorte.TotalCheque   = totalCheq
        ttCorte.TotalCaja     = totalCaja
     //   ttCorte.TotalFacturas = totalFacturas
        ttCorte.TotalCorte    = totalEfevo + totalAmex + totalDLoc + totalCheq + totalCaja + totalFacturas.

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
                     IF NOT AVAILABLE ttSaldoSuc THEN DO:
                            CREATE ttSaldoSuc.
                            ASSIGN ttSaldoSuc.Cliente = MovCliente.Id-Cliente
                                   ttSaldoSuc.idsuc   = "T"
                                   ttSaldoSuc.Saldo   = l-saldo.
                                   /* Aquí hacemos el mapeo de sucursales */
                    CASE Cliente.Id-Cliente:
                        WHEN 1 THEN ttSaldoSuc.Sucursal = "MATRIZ".
                        WHEN 4 THEN ttSaldoSuc.Sucursal = "SALTILLO".
                        WHEN 5 THEN ttSaldoSuc.Sucursal = "CHIHUAHUA".
                        WHEN 6 THEN ttSaldoSuc.Sucursal = "PABLO LIVAS".
                        WHEN 7 THEN ttSaldoSuc.Sucursal = "RUIZ CORTINEZ".
                        WHEN 8 THEN ttSaldoSuc.Sucursal = "CUMBRES".
                        WHEN 9 THEN ttSaldoSuc.Sucursal = "DIEGO DIAZ".
                        WHEN 10 THEN ttSaldoSuc.Sucursal = "CERRADAS DE ANAHUAC".
                        OTHERWISE ttSaldoSuc.Sucursal = Cliente.RazonSocial.
                    END CASE.   
                                   
                                  // ttSaldoSuc.Sucursal = Cliente.RazonSocial .  
                     END.
                     ELSE DO:
                         
                           ASSIGN ttSaldoSuc.Saldo   = ttSaldoSuc.Saldo + l-saldo.
                     END.
                
                END.
            END.
        END.
    END.
      DEFINE VARIABLE iFecha          AS DATE     NO-UNDO.
DEFINE VARIABLE l-Depositos     AS DECIMAL  NO-UNDO.
DEFINE VARIABLE l-Total2        AS DECIMAL  NO-UNDO.
DEFINE VARIABLE l-SubTot        AS DECIMAL  NO-UNDO.
DEFINE VARIABLE l-Deudores      AS DECIMAL  NO-UNDO.
DEFINE VARIABLE l-SubTotME      AS DECIMAL  NO-UNDO.
DEFINE VARIABLE l-total1        AS DECIMAL  NO-UNDO.
DEFINE VARIABLE l-totME         AS DECIMAL  NO-UNDO.
DEFINE VARIABLE l-totMEMN       AS DECIMAL  NO-UNDO.
DEFINE VARIABLE l-totMEMNFac    AS DECIMAL  NO-UNDO.
DEFINE VARIABLE l-totMEMNPago   AS DECIMAL  NO-UNDO.
DEFINE VARIABLE l-totMNpagoUS   AS DECIMAL  NO-UNDO.
DEFINE VARIABLE l-totFacMNenUS  AS DECIMAL  NO-UNDO.
DEFINE VARIABLE l-acuselst      AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-acuseMNenUS   AS CHARACTER NO-UNDO.

/* Recorremos desde l-Fecha hacia atrás hasta 30 días antes */
DO iFecha = l-Fecha TO (l-Fecha - 30) BY -1:

    /* Inicializamos variables por día */
    ASSIGN
        l-Depositos     = 0
        l-total1        = 0
        l-totME         = 0
        l-totMEMN       = 0
        l-totMEMNFac    = 0
        l-totMEMNPago   = 0
        l-totMNpagoUS   = 0
        l-totFacMNenUS  = 0
        l-acuselst      = ""
        l-acuseMNenUS   = ""
        l-Total2        = 0
        l-SubTot        = 0
        l-Deudores      = 0
        l-SubTotME      = 0.

    FOR EACH Acuse 
        WHERE Acuse.FecDep = iFecha 
          AND Acuse.Estatus = 4 NO-LOCK:

        IF Acuse.Tipo = "C" THEN NEXT.

        FOR EACH DocAcuse OF Acuse NO-LOCK:
            FIND FIRST PagoAcuse OF Acuse 
                WHERE PagoAcuse.Id-Moneda > 1 NO-LOCK NO-ERROR.

            IF AVAILABLE PagoAcuse AND DocAcuse.Id-Moneda <= 1 THEN DO:
                IF LOOKUP(DocAcuse.Id-Acuse, l-acuseMNenUS) = 0 THEN
                    l-acuseMNenUS = l-acuseMNenUS + ',' + STRING(DocAcuse.Id-Acuse).
                l-totFacMNenUS = l-totFacMNenUS + DocAcuse.ImpPago.
            END.
            ELSE IF DocAcuse.Id-Moneda > 1 THEN DO:
                IF LOOKUP(DocAcuse.Id-Acuse, l-acuselst) = 0 THEN
                    l-acuselst = l-acuselst + ',' + STRING(DocAcuse.Id-Acuse).
                l-totMEMNFac = l-totMEMNFac + ROUND(DocAcuse.ImpPago * DocAcuse.TipoCambio, 2).
                l-totME = l-totME + DocAcuse.ImpPago.
            END.
        END.

        FOR EACH PagoAcuse OF Acuse NO-LOCK:
            FIND TipoPago OF PagoAcuse NO-LOCK NO-ERROR.
            IF AVAILABLE TipoPago AND PagoAcuse.Id-Tp <> 50 THEN DO:
                IF LOOKUP(PagoAcuse.Id-Acuse, l-acuselst) > 0 AND PagoAcuse.Id-Moneda = 1 THEN
                    l-totMEMNPago = l-totMEMNPago + (PagoAcuse.Importe * PagoAcuse.TC).

                IF LOOKUP(PagoAcuse.Id-Acuse, l-acuseMNenUS) > 0 AND PagoAcuse.Id-Moneda > 1 THEN
                    l-totMNpagoUS = l-totMNpagoUS + PagoAcuse.Importe.

                ELSE IF PagoAcuse.Id-Moneda > 1 THEN DO:
                    ACCUMULATE ROUND((PagoAcuse.Importe * PagoAcuse.TC) * PagoAcuse.TipoCambio, 2) (TOTAL).
                    ACCUMULATE PagoAcuse.Importe (TOTAL).
                END.
                ELSE
                    ACCUMULATE PagoAcuse.Importe * PagoAcuse.TC (TOTAL).
            END.
        END.

        ASSIGN 
            l-total1 = (ACCUM TOTAL PagoAcuse.Importe * PagoAcuse.TC)
            l-totME  = l-totME + (ACCUM TOTAL PagoAcuse.Importe)
            l-totMEMN = l-totMEMN + (ACCUM TOTAL ROUND((PagoAcuse.Importe * PagoAcuse.TC) * PagoAcuse.TipoCambio, 2)).

        ACCUMULATE l-total1 (TOTAL).
    END.

    l-Depositos = ACCUM TOTAL l-total1.

    /* Procesamos FichaDep de ese día */
    FOR EACH b-FichaDep 
        WHERE b-FichaDep.FecReg = iFecha NO-LOCK:

        IF b-FichaDep.Clave = 'C' THEN DO:
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
        ttDepositos.idsuc = "T"
        ttDepositos.Fecha = iFecha
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

    ASSIGN l-linea = FILL(" ", 82) + FILL("_", 55).
    l-Camion = 0.
    l-FolEmb = "".
    l-FecEmb = ?.
    l-HorEmb = "".
    l-NOExis = TRUE.
    FOR EACH EstPedido WHERE EstPedido.Id-Factura = Remision.Id-Remision
            NO-LOCK BREAK BY EstPedido.FecEmb DESCENDING BY EstPedido.HorEmb DESCENDING:
            IF FIRST-OF(EstPedido.FecEmb) THEN DO:
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
    
            IF LAST-OF(DetEmbarque.Id-Embarque) AND l-NOExis = TRUE THEN DO:
               FIND Embarque WHERE Embarque.Id-Embarque = DetEmbarque.Id-Embarque
                    NO-LOCK NO-ERROR.
               IF AVAILABLE Embarque THEN DO:
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
    ASSIGN l-PagInfo = "".
    IF AVAILABLE Pedido THEN DO: 
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



