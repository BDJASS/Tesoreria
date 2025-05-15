@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*------------------------------------------------------------------------
    File        : teserp008.p
    Purpose     : HU07   /
    COMO Responsable de Tesoreria QUIERO poder consultar
     y visualizar la información de acuser PARA poder realizar 
     la operación de cancelación cuando asi sea requerido.
                  
                  teserp008.p
    Author(s)   : sis10
    Created     : Fecha actual   
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW. /* Manejo de errores global */

/* **********************  Internal Procedures  *********************** */



/* ***************************  Main Procedure *************************** */


DEFINE VARIABLE l-titulo   AS CHARACTER FORMAT "x(40)" NO-UNDO.
DEFINE VARIABLE l-dias     AS INTEGER   NO-UNDO.
DEFINE VARIABLE l-Acuse    AS RECID     NO-UNDO.
DEFINE VARIABLE l-teclas   AS CHARACTER
    INITIAL "GO,ENTER,RETURN,TAB" NO-UNDO.
DEFINE VARIABLE l-totgasto AS DECIMAL   
    FORMAT "-zzz,zzz,zz9.99" NO-UNDO.
DEFINE VARIABLE l-im1      LIKE DocAcuse.ImpPago NO-UNDO.   
DEFINE VARIABLE l-esp      LIKE DocAcuse.ImpPago NO-UNDO.
DEFINE VARIABLE l-pp1      LIKE DocAcuse.ImpPago NO-UNDO.
DEFINE VARIABLE l-dev      LIKE DocAcuse.ImpPago NO-UNDO.   
DEFINE VARIABLE l-rec      AS RECID     NO-UNDO.
DEFINE BUFFER bf-DepBanco FOR DepBanco.            // RNPC 2019-11-08

DEFINE VARIABLE l-sec1   AS RECID   NO-UNDO.
DEFINE VARIABLE l-vez    AS INTEGER NO-UNDO.
DEFINE VARIABLE l-prueba AS INTEGER NO-UNDO.
DEFINE VARIABLE l-tar    LIKE Banco.Nombre.
DEFINE VARIABLE l-descr  LIKE TipoPago.Descr NO-UNDO.

DEFINE BUFFER bf_PagoAcuse FOR PagoAcuse.

DEFINE VARIABLE l-fecvence AS DATE.     
DEFINE VARIABLE l-ubic     AS CHARACTER .
DEFINE VARIABLE l-recmov   AS RECID.
DEFINE VARIABLE l-contado  AS LOGI      NO-UNDO INITIAL FALSE.
DEFINE VARIABLE l-ren      AS INTEGER   NO-UNDO .

DEFINE WORKFILE w-fac
    FIELD Id-mc  LIKE DocAcuse.Id-mc
    FIELD Doc    LIKE DocAcuse.Documento    
    FIELD Monto  LIKE DocAcuse.ImpPago
    FIELD Saldo    LIKE MovCliente.Importe.  


DEFINE TEMP-TABLE ttAcuse NO-UNDO           
    FIELD IdAcuse       AS CHARACTER FORMAT "x(15)" /* Número de Acuse */
    FIELD IdCliente     AS INTEGER                  /* Número de cliente */
    FIELD FecReg        AS DATE                     /* Fecha de registro */
    FIELD IdCobrador    LIKE Acuse.Id-Cobrador
    FIELD Cobrador      AS CHARACTER
    FIELD RazonSocial   AS CHARACTER FORMAT "x(40)" /* Razón social del cliente */
    FIELD AcuseCobrador LIKE Acuse.AcuseCobrador
    FIELD Iniciales     LIKE Acuse.Iniciales
    FIELD InicialesForm AS CHARACTER 
    FIELD Comen         LIKE Acuse.Comen
    FIELD Tipo          LIKE Acuse.Tipo 
    FIELD Estatus       LIKE Acuse.Estatus
    FIELD Complemento   LIKE Acuse.Id-CPago.    

DEFINE TEMP-TABLE ttDetAcuse NO-UNDO
    FIELD IdAcuse    AS CHARACTER FORMAT "x(15)" /* Número de Acuse */
    FIELD Num        LIKE DocAcuse.Sec           
    FIELD FecReg     AS DATE                     /* Fecha de registro */
    FIELD Clave      LIKE DocAcuse.Documento
    FIELD Descr      LIKE TabMC.Descr
    FIELD Importe    LIKE DocAcuse.ImpPago 
    FIELD ProntoPago LIKE DocAcuse.ImpDescPP
    FIELD DescEsp    LIKE DocAcuse.ImpDescPP
    FIELD Devolucion LIKE DocAcuse.ImpDevol
    FIELD Dias       AS INTEGER.

DEFINE TEMP-TABLE ttDetPago NO-UNDO
    FIELD IdAcuse AS CHARACTER
    FIELD IdTp    LIKE PagoAcuse.Id-Tp
    FIELD Descr   AS CHARACTER
    FIELD Pago    LIKE PagoAcuse.ImpRecibido
    FIELD Banco   LIKE PagoAcuse.Id-Banco
    FIELD Nombre  AS CHARACTER
    FIELD NumCheq LIKE PagoAcuse.Cheque
    FIELD FecCheq LIKE PagoAcuse.FecCheque.
DEFINE DATASET dsFactura FOR ttAcuse, ttDetAcuse,ttDetPago
    DATA-RELATION RelFacturaDetalle FOR ttAcuse, ttDetAcuse 
    RELATION-FIELDS (IdAcuse, IdAcuse)
    DATA-RELATION RelFacturaDetalle2 FOR ttDetAcuse , ttDetPago 
    RELATION-FIELDS (IdAcuse, IdAcuse).


DEFINE BUFFER bf_Acuse FOR Acuse.


PROCEDURE "cxca0241.p":
    /*------------------------------------------------------------------------------
     Purpose: DESAFECTA ACUSES    Acuse.Tipo = "N" OR Acuse.Tipo = "P"
     Notes:   cxca0240.p
    ------------------------------------------------------------------------------*/
    /*
      Empresa  : Consultoria en Informatica Ejecutiva, S.A. de C.V.
      Modulo   : Cuentas por Cobrar
      Programa : cxca0243.i
      Funcion  : Accion del boton de Desafectar Acuse
      Autor    : IOC
      Fecha    : 22-10-1996
    */
    DEF INPUT PARAMETER  l-AcuseR AS RECID NO-UNDO.
    DEFINE VAR l-Acu1 LIKE Acuse.Id-Acuse.

    DEF BUFFER MovCliente_bf FOR MovCliente.
    DEF VAR l-deposito AS DATE.
    DEF VAR l-ren      AS INT     NO-UNDO.
    DEF VAR l-rec      AS RECID.
    DEF VAR l-estatus  AS INTEGER FORMAT '9'.
    DEF VAR l-recpf    AS RECID.

    FIND Acuse WHERE RECID(Acuse) = l-AcuseR NO-LOCK NO-ERROR.
    ASSIGN 
        l-Acu1     = Acuse.Id-Acuse
        l-deposito = TODAY
        l-estatus  = 0
        l-rec      = 0.

    MESSAGE "Procesando DESAFECTACION TIPO N-P" +  STRING(l-AcuseR) + STRING(l-Acu1) VIEW-AS ALERT-BOX.    
    

    DO TRANSACTION :  
        /* Checa si existen cheques para cambiarlos a postfechados */

       // FOR EACH PagoAcuse OF Acuse NO-LOCK :
            FOR EACH PagoAcuse where PagoAcuse.Id-Acuse = l-Acu1 NO-LOCK:
            FIND TipoPago OF PagoAcuse NO-LOCK NO-ERROR.
            IF NOT TipoPago.Descr MATCHES '*CHEQUE*' THEN NEXT.
            IF PagoAcuse.FecCheque <> ? THEN 
            DO:
                FIND FIRST ChequePF WHERE
                    ChequePF.ID-Acuse = Acuse.Id-Acuse AND
                    ChequePF.Id-Banco = PagoAcuse.Id-Banco AND
                    ChequePF.Cheque   = PagoAcuse.Cheque AND
                    ChequePF.FecCheque = PagoAcuse.FecCheque NO-LOCK
                    NO-ERROR.
                IF AVAILABLE ChequePF THEN 
                DO:
                    ASSIGN 
                        l-recpf = RECID(ChequePF).
                    FIND ChequePF WHERE RECID(ChequePF) = l-recpf EXCLUSIVE-LOCK
                        NO-ERROR.
                    ASSIGN 
                        ChequePF.FecDep = l-deposito.
                END.
                ELSE 
                DO:
                    CREATE ChequePF.
                    ASSIGN 
                        ChequePF.Id-Acuse   = Acuse.Id-Acuse
                        ChequePF.Id-Caja    = Acuse.Id-Caja
                        ChequePF.Id-Cliente = Acuse.Id-Cliente
                        ChequePF.FecCheque  = PagoAcuse.FecCheque
                        ChequePF.FecDep     = l-deposito
                        ChequePF.Id-Banco   = PagoAcuse.Id-Banco
                        ChequePF.Cheque     = PagoAcuse.Cheque
                        ChequePF.Importe    = PagoAcuse.Importe * PagoAcuse.TC.
                END.
            END.
        END.
        FIND Acuse WHERE Acuse.Id-Acuse = l-Acu1 NO-LOCK NO-ERROR.    
        /* Se Checa si ya se hizo el Corte de Caja */
        FIND CtlCaja WHERE
            CtlCaja.Id-Caja = Acuse.Id-Caja AND
            CtlCaja.Turno   = Acuse.Turno   AND
            CtlCaja.FecOper = Acuse.FecOper NO-LOCK NO-ERROR.

        IF AVAILABLE (CtlCaja) AND Ctlcaja.Feccierre <> ? THEN 
        DO:
            FOR EACH PagoAcuse WHERE PagoAcuse.Id-Acuse = l-Acu1 NO-LOCK BREAK BY PagoAcuse.Id-Tp:
                ACCUMULATE PagoAcuse.Importe (TOTAL by PagoAcuse.Id-Tp).

                IF LAST-OF(PagoAcuse.Id-Tp) THEN 
                DO:   
                    FIND FIRST CorteCaja WHERE
                        CorteCaja.Id-Caja = Acuse.Id-Caja AND
                        CorteCaja.Turno   = Acuse.Turno   AND
                        CorteCaja.FecOper = Acuse.FecOper AND
                        CorteCaja.Id-Tp   = PagoAcuse.Id-Tp NO-LOCK.
                    IF AVAILABLE (CorteCaja) THEN 
                    DO:
                        ASSIGN 
                            l-rec = RECID(CorteCaja).
                        FIND CorteCaja WHERE RECID(CorteCaja) = l-rec EXCLUSIVE-LOCK.
                    END.
                    ELSE 
                    DO:
                        CREATE CorteCaja.
                        ASSIGN 
                            CorteCaja.Id-Caja = Acuse.Id-Caja
                            CorteCaja.Turno   = Acuse.Turno
                            CorteCaja.FecOper = Acuse.FecOper
                            CorteCaja.Id-Tp   = PagoAcuse.Id-Tp.
                    END.
                    ASSIGN 
                        CorteCaja.TotPagoP = CorteCaja.TotPagoP +
                                       (ACCUM TOTAL by PagoAcuse.Id-TP
                                                       PagoAcuse.Importe )
                        CorteCaja.TotPagoN = CorteCaja.TotPagoN -
                                       (ACCUM TOTAL by PagoAcuse.Id-TP
                                                       PagoAcuse.Importe ).
                END.  /* last-of   */
            END.  /* pagoacuse  */
        END.    /* AVAILABLE  Corte Caja */

        IF Acuse.Tipo = 'P' OR Acuse.Tipo = 'N' THEN 
        DO:

            FOR EACH w-Fac :
                DELETE w-Fac.
            END.
            FOR EACH DocAcuse OF Acuse NO-LOCK :

                IF DocAcuse.ImpPago > 0 THEN 
                DO:
                    ASSIGN 
                        l-ren = IF DocAcuse.Id-Mc = 1 THEN 4         /*AbonoCr*/
                             ELSE IF DocAcuse.Id-Mc = 2 THEN 7 /*AbonoCo*/
                                 ELSE 8.                       /*AbonoCh*/
                    {cxca0006.i
              &Cliente = Acuse.Id-Cliente
              &Importe = " ( DocAcuse.ImpPago * -1 ) "
              &renglon = l-ren
              &fecha   = TODAY }
                END.
                IF DocAcuse.ImpDevol > 0 THEN 
                DO:
                    ASSIGN 
                        l-ren = IF DocAcuse.Id-Mc = 1 THEN 2         /*VentasCr*/
                            ELSE IF DocAcuse.Id-Mc = 2 THEN 5 /*Cargos*/
                                ELSE 6.                      /*Chedev*/
                    {cxca0006.i
              &Cliente = Acuse.Id-Cliente
              &Importe = DocAcuse.ImpDevol
              &renglon = l-ren
              &fecha   = TODAY }

           IF DocAcuse.Tipo-Dev = 67 THEN DO:
                FIND Devolucion WHERE Devolucion.Id-Dev = DocAcuse.Id-Dev
                    NO-LOCK NO-ERROR.
                    {cxca0006.i
                     &Cliente  = Acuse.Id-Cliente
                     &Importe  = 0
                     &SUbtotal = Devolucion.Subtotal
                     &renglon  = l-ren
                     &fecha    = TODAY }
            END.
        END.
        IF DocAcuse.ImpDescEsp > 0 THEN 
        DO:
            ASSIGN 
                l-ren = IF DocAcuse.Id-Mc = 1 THEN 2
                            ELSE IF DocAcuse.Id-Mc = 2 THEN 5
                                ELSE 6.
            {cxca0006.i
              &Cliente = Acuse.Id-Cliente
              &Importe = DocAcuse.ImpDescEsp
              &renglon = l-ren
              &fecha   = TODAY }
        END.
        IF DocAcuse.ImpDescPP > 0 THEN 
        DO:
            ASSIGN 
                l-ren = IF DocAcuse.Id-Mc = 1 THEN 2
                            ELSE IF DocAcuse.Id-Mc = 2 THEN 5
                               ELSE 6.
            {cxca0006.i
              &Cliente = Acuse.Id-Cliente
              &Importe = DocAcuse.ImpDescPP
              &renglon = l-ren
              &fecha   = TODAY }
        END.
        IF DocAcuse.ImpDescAdc > 0 THEN 
        DO:
            ASSIGN 
                l-ren = IF DocAcuse.Id-Mc = 1 THEN 2
                            ELSE IF DocAcuse.Id-Mc = 2 THEN 5
                               ELSE 6.
            {cxca0006.i
              &Cliente = Acuse.Id-Cliente
              &Importe = DocAcuse.ImpDescAdc
              &renglon = l-ren
              &fecha   = TODAY }
        END.
        FOR EACH MovCliente WHERE
            MovCliente.RefSaldo = DocAcuse.Documento AND
            MovCliente.Documento = Acuse.Id-Acuse EXCLUSIVE-LOCK :

            FIND FIRST w-Fac WHERE w-Fac.Doc = DocAcuse.Documento AND
                w-Fac.Id-Mc = DocAcuse.Id-mc NO-ERROR.
            IF NOT AVAILABLE w-Fac THEN 
            DO:
                CREATE w-Fac.
                ASSIGN 
                    w-Fac.Id-mc = DocAcuse.Id-mc
                    w-Fac.Doc   = DocAcuse.Documento.
            END.

            ASSIGN 
                w-Fac.Saldo         = w-Fac.Saldo + (MovCliente.Importe * -1)
                MovCliente.Afectado = FALSE.
        END.
    END.
    /* Desafecta saldo de la factura */
    FOR EACH w-Fac:
    {cxca0004.i &Factura = w-Fac.Doc}
        IF NOT AVAILABLE MovCliente THEN
            NEXT.
        ELSE 
        DO:
            IF MovCliente.Id-Mc <> w-Fac.Id-mc THEN
                NEXT.
            ELSE
                ASSIGN MovCliente.Saldo = MovCliente.Saldo + w-Fac.Saldo.
        END.
        DELETE w-Fac.
    END.
END.      /* End de Acuse Normal y PostFechado */  
 
FIND Acuse WHERE Acuse.Id-Acuse = l-Acu1 EXCLUSIVE-LOCK NO-ERROR.
ASSIGN 
    Acuse.Estatus = 1
    Acuse.FecDep  = l-deposito.
IF Acuse.Id-Origen = "SA" OR Acuse.Id-Acuse MATCHES "*S" OR
    Acuse.Id-Acuse MATCHES "*SA" THEN
    ASSIGN Acuse.Act-Origen = TRUE
        Acuse.FecAOrigen = TODAY.
ELSE 
DO:
    FIND Cliente OF Acuse NO-LOCK NO-ERROR.
    IF AVAILABLE Cliente THEN 
    DO:
    {cxca0009.i}
            ASSIGN Acuse.Act-Origen = TRUE
                   Acuse.FecAOrigen = TODAY.
         END.
    END. /* si es de saltillo */
    END.
END.  
MESSAGE "Procesando FIN-DESAFECTACION TIPO N-P" + STRING(Acuse.Estatus) VIEW-AS ALERT-BOX.    

RELEASE Acuse.
END.  /* FIN DE TRANSACCION */

END PROCEDURE.


PROCEDURE Cancel1:   
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER  l-AcuseS AS RECID NO-UNDO.
    DEFINE INPUT  PARAMETER pIdUser  AS CHARACTER NO-UNDO. /* Usuario a validar */
    DEFINE INPUT  PARAMETER pIdUserSol AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER pMotivo    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER vbandera   AS LOGICAL NO-UNDO.
    
    DEFINE VAR l-Acu2 LIKE Acuse.Id-Acuse.

    ASSIGN 
        l-Acuse  = 0
        vbandera = FALSE.
    /* BLOQUE PARA CANCELAR ACUSES */
    MESSAGE "Procesando CANCEL1 TIPO N-P" + STRING(l-AcuseS) VIEW-AS ALERT-BOX.    
 

            
         // cxca0231.p  cxca0231.i
    Proceso:
    DO TRANSACTION ON ENDKEY UNDO, LEAVE ON ERROR UNDO, LEAVE :
        FIND Acuse WHERE RECID(Acuse) = l-AcuseS EXCLUSIVE-LOCK NO-ERROR.
        IF Acuse.Id-Origen = "SA" OR Acuse.Id-Acuse MATCHES "*S" OR
            Acuse.Id-Acuse MATCHES "*SA" THEN
            ASSIGN Acuse.Act-Origen = TRUE
                Acuse.FecAOrigen = TODAY.
        ELSE 
        DO:
            FIND Cliente OF Acuse NO-LOCK NO-ERROR.
            IF AVAILABLE Cliente THEN 
            DO:
                    {cxca0009.i}   
                        ASSIGN 
                            Acuse.Act-Origen = TRUE
                            Acuse.FecAOrigen = TODAY.
                    END.
            END. /* si es de saltillo */
            END.
        END.
        IF Acuse.Estatus = 2 OR Acuse.Estatus = 4 THEN 
        DO:
            FOR EACH DocAcuse OF Acuse EXCLUSIVE-LOCK :
                IF DocAcuse.ImpPago > 0 THEN 
                DO:
                    ASSIGN 
                        l-ren = IF DocAcuse.Id-Mc = 1 THEN 4         /*AbonoCr*/
                               ELSE IF DocAcuse.Id-Mc = 2 THEN 7 /*AbonoCo*/
                                   ELSE 8.                       /*AbonoCh*/
                    {cxca0006.i
                 &Cliente = Acuse.Id-Cliente
                 &Importe = " ( DocAcuse.ImpPago * -1 ) "
                 &renglon = l-ren
                 &fecha   = TODAY }
                END.
                IF DocAcuse.ImpDevol > 0 THEN 
                DO:
                    ASSIGN 
                        l-ren = IF DocAcuse.Id-Mc = 1 THEN 2         /*VentasCr*/
                            ELSE IF DocAcuse.Id-Mc = 2 THEN 5    /*Cargos*/
                               ELSE 6.                           /*Chedev*/
                    {cxca0006.i
                 &Cliente = Acuse.Id-Cliente
                 &Importe = DocAcuse.ImpDevol
                 &renglon = l-ren
                 &fecha   = TODAY }

                    IF DocAcuse.Tipo-Dev = 67 THEN DO:
                FIND Devolucion WHERE Devolucion.Id-Dev = DocAcuse.Id-Dev
                    NO-LOCK NO-ERROR.
                            {cxca0006.i
                     &Cliente  = Acuse.Id-Cliente
                     &Importe  = 0
                     &SUbtotal = Devolucion.Subtotal
                     &renglon  = l-ren
                     &fecha    = TODAY }
            END.
        END.


        IF DocAcuse.ImpDescAdc > 0 THEN 
        DO:
            ASSIGN 
                l-ren = IF DocAcuse.Id-Mc = 1 THEN 2         /*VentasCr*/
                            ELSE IF DocAcuse.Id-Mc = 2 THEN 5    /*Cargos*/
                               ELSE 6.                           /*Chedev*/
            {cxca0006.i
                 &Cliente = Acuse.Id-Cliente
                 &Importe = DocAcuse.ImpDescAdc
                 &renglon = l-ren
                 &fecha   = TODAY }
        END.
        IF DocAcuse.ImpDescEsp > 0 THEN 
        DO:
            ASSIGN 
                l-ren = IF DocAcuse.Id-Mc = 1 THEN 2
                            ELSE IF DocAcuse.Id-Mc = 2 THEN 5
                               ELSE 6.
            {cxca0006.i
                 &Cliente = Acuse.Id-Cliente
                 &Importe = DocAcuse.ImpDescEsp
                 &renglon = l-ren
                 &fecha   = TODAY }
        END.
        IF DocAcuse.ImpDescPP > 0 THEN 
        DO:
            ASSIGN 
                l-ren = IF DocAcuse.Id-Mc = 1 THEN 2
                            ELSE IF DocAcuse.Id-Mc = 2 THEN 5
                               ELSE 6.
            {cxca0006.i
                 &Cliente = Acuse.Id-Cliente
                 &Importe = DocAcuse.ImpDescPP
                 &renglon = l-ren
                 &fecha   = TODAY }
        END.
    END.
END.   
    

/******************************************************************/
/* Afectar el corte de caja si ya se habia realizado              */
/******************************************************************/
FIND CtlCaja WHERE   
    CtlCaja.Id-Caja = Acuse.Id-Caja AND
    CtlCaja.Turno   = Acuse.Turno   AND
    CtlCaja.FecOper = Acuse.FecOper NO-LOCK NO-ERROR.

IF AVAILABLE (CtlCaja) AND CtlCaja.FecCierre <> ? THEN 
DO:
    FOR EACH PagoAcuse OF Acuse NO-LOCK BREAK BY PagoAcuse.Id-Tp:
        FIND TipoPago OF PagoAcuse NO-LOCK NO-ERROR.
        IF Acuse.Estatus = 1 THEN
            ACCUMULATE PagoAcuse.Importe (TOTAL BY PagoAcuse.Id-Tp).
        ELSE
            ACCUMULATE PagoAcuse.Importe * 1 (TOTAL BY PagoAcuse.Id-Tp).

        ASSIGN 
            l-totgasto = l-totgasto + PagoAcuse.ImpGasto.

        IF LAST-OF(PagoAcuse.Id-Tp) THEN 
        DO:
            FIND FIRST CorteCaja WHERE
                CorteCaja.Id-Caja = Acuse.Id-Caja AND
                CorteCaja.Turno   = Acuse.Turno   AND
                CorteCaja.FecOper = Acuse.FecOper AND
                CorteCaja.Id-Tp   = PagoAcuse.Id-Tp NO-LOCK.
            IF AVAILABLE (CorteCaja) THEN 
            DO:
                ASSIGN 
                    l-rec = RECID(CorteCaja).
                FIND CorteCaja WHERE RECID(CorteCaja) = l-rec EXCLUSIVE-LOCK.
            END.
            ELSE 
            DO:
                CREATE CorteCaja.
                ASSIGN 
                    CorteCaja.Id-Caja = Acuse.Id-Caja
                    CorteCaja.Turno   = Acuse.Turno
                    CorteCaja.FecOper = Acuse.FecOper
                    CorteCaja.Id-Tp   = PagoAcuse.Id-Tp.
            END.
            ASSIGN 
                CorteCaja.TotPagoP = CorteCaja.TotPagoP -
                          (IF Acuse.Tipo = 'P' THEN
                          (ACCUM TOTAL BY PagoAcuse.Id-TP PagoAcuse.Importe )
                          ELSE 0 )
                CorteCaja.TotPagoN = CorteCaja.TotPagoN -
                          (IF Acuse.Tipo = 'N' THEN
                          (ACCUM TOTAL BY PagoAcuse.Id-TP PagoAcuse.Importe * 1)
                          ELSE 0 )
                CorteCaja.TotGasto = CorteCaja.TotGasto - l-totgasto .
            l-totgasto = 0.
        END.  /* last-of   */
    END.  /* pagoacuse  */
END.
        


FOR EACH w-Fac :
    DELETE w-Fac.
END.

/******************************************************************/
/* Borra los movimientos de clientes                              */
/******************************************************************/
FOR EACH DocAcuse OF Acuse NO-LOCK BY DocAcuse.Id-MC
    ON ERROR UNDO Proceso, LEAVE Proceso
    ON ENDKEY UNDO Proceso, LEAVE Proceso :

    FOR EACH MovCliente WHERE
        MovCliente.RefSaldo = DocAcuse.Documento AND
        MovCliente.Documento = Acuse.Id-Acuse EXCLUSIVE-LOCK
        ON ERROR UNDO Proceso, LEAVE Proceso
        ON ENDKEY UNDO Proceso, LEAVE Proceso :

        FIND FIRST w-Fac WHERE w-Fac.Doc = DocAcuse.Documento AND
            w-Fac.Id-Mc = DocAcuse.Id-mc NO-ERROR.
        IF NOT AVAILABLE w-Fac THEN 
        DO:
            CREATE w-Fac.
            ASSIGN 
                w-Fac.Id-mc = DocAcuse.Id-mc
                w-Fac.Doc   = DocAcuse.Documento.
        END.
        ASSIGN 
            w-Fac.Monto = w-Fac.Monto + (MovCliente.Importe * -1).
        DELETE MovCliente.
    END.

    /*SI hay devolucion la desaplica */
    IF DocAcuse.id-Dev > 0 THEN 
    DO:
        FIND Devolucion OF DocAcuse EXCLUSIVE-LOCK NO-ERROR.
        IF DocAcuse.tipo-Dev = 67 THEN 
        DO:
            FIND Factura WHERE Factura.Id-factura = Devolucion.Documento
                EXCLUSIVE-LOCK NO-ERROR.
            ASSIGN 
                Factura.FecCanc     = ?
                Factura.UsuarioCanc = ''.
        END.
        ASSIGN 
            Devolucion.FecApl     = ?
            Devolucion.UsuarioApl = ''
            Devolucion.Documento  = ''
            Devolucion.Id-mc      = 0.
    END.
END.
/******************************************************************/
/* Desafecta saldo de la factura SOLO SI EL ACUSE TIENE ESTATUS 2 */
/******************************************************************/
IF Acuse.Estatus = 2 OR Acuse.Estatus = 4 THEN 
DO:
    FOR EACH w-Fac ON ERROR UNDO Proceso, LEAVE Proceso
        ON ENDKEY UNDO Proceso, LEAVE Proceso :
                {cxca0004.i &Factura = w-Fac.Doc}
        IF NOT AVAILABLE MovCliente THEN
            UNDO Proceso, LEAVE Proceso.
        ELSE 
        DO:
            IF MovCliente.Id-Mc <> w-Fac.Id-mc THEN
                UNDO Proceso, LEAVE Proceso.
            ELSE
                ASSIGN MovCliente.Saldo = MovCliente.Saldo + w-Fac.Monto.
        END.
        DELETE w-Fac.
    END.

    FIND NCR WHERE Ncr.Id-Ncr = Acuse.Id-Ncr EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE (Ncr) THEN
        ASSIGN NCR.UsuarioCanc = CAPS(pIdUser)
            NCR.FecCanc     = TODAY.
END.

/****************************************************/
/*Borra cheques postfechados                        */
/****************************************************/
FOR EACH ChequePF WHERE ChequePF.Id-Acuse = Acuse.Id-Acuse EXCLUSIVE-LOCK
    ON ERROR UNDO Proceso, LEAVE Proceso
    ON ENDKEY UNDO Proceso, LEAVE Proceso :
    DELETE ChequePF.
END.
/****************************************************/
/*Checa si no se pago con Devolucion                */
/****************************************************/
FOR EACH PagoAcuse OF Acuse ON ERROR UNDO Proceso, LEAVE Proceso
    ON ENDKEY UNDO Proceso, LEAVE Proceso :
    IF PagoAcuse.Id-Dev > 0 THEN 
    DO:
        FIND Devolucion WHERE Devolucion.Id-dev = PagoAcuse.Id-dev
            EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE Devolucion THEN
            ASSIGN Devolucion.FecApl     = ?
                Devolucion.Documento  = ''
                Devolucion.Id-mc      = 0
                Devolucion.UsuarioApl = ''.
    END.
END.

/****************************************************/
/*Cancela Acuse                                     */
/****************************************************/

ASSIGN 
    Acuse.Estatus     = 3   /* ACUSE CANCELADO */
    Acuse.UsuarioCanc = CAPS(pIdUser)     
    Acuse.FecCanc     = TODAY
    Acuse.UsuarioSol  = CAPS(pIdUserSol)
    Acuse.Motivo      = pMotivo
    vbandera          = TRUE.

/****************************************************/
/* 2019-09-17 - RNPC - Libera registro DepBanco     */
/****************************************************/
FIND FIRST DepBanco WHERE DepBanco.conciliado AND DepBanco.id-acuse = Acuse.Id-Acuse 
    NO-LOCK NO-ERROR.
IF AVAILABLE DepBanco THEN 
DO:
    FIND FIRST bf-DepBanco WHERE RECID(bf-DepBanco) = RECID(DepBanco) 
        EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE bf-DepBanco THEN 
    DO:                                            
        ASSIGN 
            bf-DepBanco.id-acuse   = ''
            bf-DepBanco.Conciliado = FALSE 
            bf-DepBanco.id-user    = ''
            bf-DepBanco.FecAplica  = ?.
        RELEASE bf-DepBanco.
    END.
END.
END. // DO TRANS
    
           
END PROCEDURE.

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetAcuse:
    DEFINE INPUT  PARAMETER pIdAcuse   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER Respuesta  AS CHARACTER. 
    DEFINE OUTPUT PARAMETER IdError    AS LOGICAL.
    DEFINE OUTPUT PARAMETER DATASET FOR dsFactura.
    
    FIND FIRST SysGeneral NO-LOCK NO-ERROR.
    FIND Acuse WHERE Acuse.Id-Acuse = pIdAcuse NO-LOCK NO-ERROR.
    IF AVAILABLE (Acuse) THEN 
    DO:
        /*
        FIND FIRST CPago WHERE CPago.Id-Acuse = Acuse.Id-Acuse
            AND CPago.FecCanc = ?
            NO-LOCK NO-ERROR.
        IF AVAILABLE CPago THEN 
        DO: 
            ASSIGN
                Respuesta = 'No se permite cancelar, fue generado Complemento de Pago.'
                IdError   = TRUE.
            RETURN.
        END.
        */
        IF Acuse.Id-nco <> '' THEN 
        DO:
            FIND Remision WHERE Remision.Id-Remision =
                Acuse.Id-NCO NO-LOCK NO-ERROR.
            IF AVAILABLE Remision THEN
                IF Remision.FecCanc = ? THEN 
                DO: 
                    ASSIGN 
                        Respuesta = 'No se permite cancelar, fue generada Remision.'
                        IdError   = TRUE.
                    RETURN.
                END.
            IF NOT AVAILABLE Remision THEN 
            DO:
                ASSIGN 
                    Respuesta = 'No se permite cancelar, fue generada Remision.'
                    IdError   = TRUE.
                RETURN.
            END.
        END.   

        IF Acuse.Estatus = 3 AND Acuse.FecCanc <> ? THEN 
        DO: 
            DEFINE VARIABLE cCancela       AS CHARACTER NO-UNDO.
            DEFINE VARIABLE cAutoriza      AS CHARACTER NO-UNDO.
            DEFINE VARIABLE cMensajeMotivo AS CHARACTER NO-UNDO.
    
            /* Obtener nombre de quien cancela */
            IF Acuse.UsuarioCanc <> "" THEN 
            DO:
                FIND FIRST Empleado WHERE empleado.Iniciales = Acuse.UsuarioCanc NO-LOCK NO-ERROR.
                cCancela = IF AVAILABLE Empleado THEN Empleado.Nombre ELSE Acuse.UsuarioCanc.
            END.
            ELSE 
            DO:
                cCancela = "Desconocido".
            END.
    
            /* Obtener nombre de quien autoriza */
            IF Acuse.UsuarioSol <> "" THEN 
            DO:
                FIND FIRST Empleado WHERE empleado.Iniciales = Acuse.UsuarioSol NO-LOCK NO-ERROR.
                cAutoriza = IF AVAILABLE Empleado THEN Empleado.Nombre ELSE Acuse.UsuarioSol.
            END.
            ELSE 
            DO:
                cAutoriza = "No especificado".
            END.
    
            /* Manejar el motivo solo si existe */
            IF Acuse.Motivo <> "" AND Acuse.Motivo <> ? THEN
                cMensajeMotivo = " Motivo: " + STRING(Acuse.Motivo) + ".".
            ELSE
                cMensajeMotivo = "".              
    
            /* Construir mensaje final */
            ASSIGN
                Respuesta = "El acuse fue cancelado el dia " + STRING(Acuse.FecCanc) +
                   " por " + cCancela + "." +
                   (IF Acuse.UsuarioSol <> "" 
                    THEN " Solicitado por: " + cAutoriza + "."      
                    ELSE "") + 
                   cMensajeMotivo
                IdError   = TRUE.
            RETURN.  
        END.

        FOR EACH DetNco WHERE
            DetNco.Referencia = Acuse.Id-Acuse NO-LOCK:
            FIND NCO OF DetNco NO-LOCK NO-ERROR.
            IF NCO.FecCanc = ? THEN 
            DO: 
                ASSIGN
                    Respuesta = "El Acuse TIENE asociado NCO por DESC CANCELADOS."
                    IdError   = TRUE.
                RETURN.
            END.
        END.
        
        IF Acuse.Estatus = 4 THEN 
        DO:
            IF Acuse.FecDep <= SysGeneral.FecCieDep THEN 
            DO:
                         
                ASSIGN
                    Respuesta = "Fecha de deposito CERRADA en contabilidad, imposible modificar..."
                    IdError   = TRUE.
                RETURN.
            END.
        END.

    /*
    IF Acuse.Estatus = 4 THEN 
    DO: 
        ASSIGN
            Respuesta = "El Acuse esta Depositado."
            IdError   = TRUE.
        RETURN.
    END. */  
    END. /* end del available       */
    ELSE 
    DO: 
        ASSIGN
            Respuesta = "El Acuse no existe."
            IdError   = TRUE.
        RETURN.
    END. /* end del not available   */
    
    IF AVAILABLE (Acuse) THEN 
    DO:
        FIND Cliente OF Acuse NO-LOCK NO-ERROR.
        FIND FIRST Cobrador WHERE Cobrador.Id-Cobrador = Acuse.Id-Cobrador NO-LOCK NO-ERROR.
        FIND FIRST Empleado WHERE Empleado.Iniciales   = Acuse.Iniciales   NO-LOCK NO-ERROR. 
        CREATE ttAcuse. 
        ASSIGN 
            ttAcuse.IdAcuse       = Acuse.Id-Acuse
            ttAcuse.IdCliente     = Acuse.Id-Cliente
            ttAcuse.FecReg        = Acuse.FecReg
            ttAcuse.IdCobrador    = Acuse.Id-Cobrador
            ttAcuse.RazonSocial   = Cliente.RazonSocial
            ttAcuse.AcuseCobrador = Acuse.AcuseCobrador
            ttAcuse.Iniciales     = Acuse.Iniciales
            ttAcuse.Comen         = Acuse.Comen[1]
            ttAcuse.Tipo          = Acuse.Tipo
            ttAcuse.Estatus       = Acuse.Estatus
            ttAcuse.Complemento   = Acuse.Id-CPago.
            
        IF AVAILABLE Cobrador THEN
            ttAcuse.Cobrador = Cobrador.Nombre.
        ELSE
            ttAcuse.Cobrador = "".
            
        IF AVAILABLE Empleado THEN
            ttAcuse.InicialesForm = empleado.Nombre.
        ELSE
            ttAcuse.InicialesForm = "".
                
        ASSIGN 
            l-Acuse = RECID(Acuse).
        IF Acuse.Tipo = "N" OR Acuse.Tipo = "P" THEN 
        DO:
            FIND Acuse WHERE RECID(Acuse) = l-Acuse NO-LOCK NO-ERROR.
            FOR EACH DocAcuse OF Acuse NO-LOCK:
                
                FIND TabMC WHERE TabMC.Id-MC = DocAcuse.Id-MC
                    NO-LOCK NO-ERROR.
                ASSIGN 
                    l-dias = TODAY - DocAcuse.FecDoc
                    l-esp  = DocAcuse.ImpDescEsp + DocAcuse.ImpDescAdc.
               
                CREATE ttDetAcuse.
                ASSIGN
                    ttDetAcuse.IdAcuse    = DocAcuse.Id-Acuse
                    ttDetAcuse.Num        = DocAcuse.Sec
                    ttDetAcuse.FecReg     = DocAcuse.FecDoc
                    ttDetAcuse.Clave      = DocAcuse.Documento
                    ttDetAcuse.Descr      = TabMC.Descr 
                    WHEN AVAILABLE TabMC
                    ttDetAcuse.Importe    = DocAcuse.ImpPago
                    ttDetAcuse.ProntoPago = DocAcuse.ImpDescPP
                    ttDetAcuse.DescEsp    = l-esp
                    ttDetAcuse.Devolucion = DocAcuse.ImpDevol
                    ttDetAcuse.Dias       = l-dias.   
                      
            END. 
            FIND Acuse WHERE RECID(Acuse) = l-Acuse NO-LOCK NO-ERROR.
            FOR EACH PagoAcuse OF Acuse NO-LOCK :
                
                IF PagoAcuse.Id-dev > 0 THEN
                    ASSIGN l-descr = 'DEV. ' + STRING(PagoAcuse.Id-dev).
                ELSE 
                DO:
                    FIND TipoPago OF PagoAcuse NO-LOCK NO-ERROR .
                    ASSIGN 
                        l-tar   = ''
                        l-descr = TipoPago.Descr.
                    IF TipoPago.Descr MATCHES '*CHEQUE*' THEN 
                    DO:
                        FIND Banco WHERE Banco.Id-Banco = PagoAcuse.Id-Banco NO-LOCK NO-ERROR.
                        ASSIGN 
                            l-tar = IF AVAILABLE Banco THEN Banco.Nombre ELSE ''.
                    END.
                    ELSE 
                    DO:
                        FIND TarjetaC WHERE TarjetaC.Id-tarjeta = PagoAcuse.Id-banco NO-LOCK
                            NO-ERROR.
                        ASSIGN 
                            l-tar = IF AVAILABLE TarjetaC THEN TarjetaC.Nombre ELSE ''.
                    END.
                END.
                
                CREATE ttDetPago.
                ASSIGN 
                    ttDetPago.IdAcuse = PagoAcuse.Id-Acuse
                    ttDetPago.IdTp    = PagoAcuse.Id-Tp 
                    ttDetPago.Descr   = l-descr
                    ttDetPago.Pago    = PagoAcuse.ImpRecibido 
                    ttDetPago.FecCheq = PagoAcuse.FecCheque
                    ttDetPago.NumCheq = PagoAcuse.Cheque 
                    ttDetPago.Banco   = PagoAcuse.Id-Banco 
                    ttDetPago.Nombre  = l-tar. 
              
            END.     
           // RUN cxca0231.p (INPUT l-Acuse).
        END.

        IF Acuse.Tipo = "A" OR Acuse.Tipo = 'C' THEN 
        DO:
            FIND Acuse WHERE RECID(Acuse) = l-Acuse NO-LOCK NO-ERROR.
            FOR EACH PagoAcuse OF Acuse NO-LOCK :
                
                IF PagoAcuse.Id-dev > 0 THEN
                    ASSIGN l-descr = 'DEV. ' + STRING(PagoAcuse.Id-dev).
                ELSE 
                DO:
                    FIND TipoPago OF PagoAcuse NO-LOCK NO-ERROR .
                    ASSIGN 
                        l-tar   = ''
                        l-descr = TipoPago.Descr.
                    IF TipoPago.Descr MATCHES '*CHEQUE*' THEN 
                    DO:
                        FIND Banco WHERE Banco.Id-Banco = PagoAcuse.Id-Banco NO-LOCK NO-ERROR.
                        ASSIGN 
                            l-tar = IF AVAILABLE Banco THEN Banco.Nombre ELSE ''.
                    END.
                    ELSE 
                    DO:
                        FIND TarjetaC WHERE TarjetaC.Id-tarjeta = PagoAcuse.Id-banco NO-LOCK
                            NO-ERROR.
                        ASSIGN 
                            l-tar = IF AVAILABLE TarjetaC THEN TarjetaC.Nombre ELSE ''.
                    END.
                END.
                
                CREATE ttDetPago.
                ASSIGN 
                    ttDetPago.IdAcuse = PagoAcuse.Id-Acuse
                    ttDetPago.IdTp    = PagoAcuse.Id-Tp 
                    ttDetPago.Descr   = l-descr
                    ttDetPago.Pago    = PagoAcuse.ImpRecibido 
                    ttDetPago.FecCheq = PagoAcuse.FecCheque
                    ttDetPago.NumCheq = PagoAcuse.Cheque 
                    ttDetPago.Banco   = PagoAcuse.Id-Banco 
                    ttDetPago.Nombre  = l-tar. 
              
            END.   
            
          //  RUN cxca0232.p (INPUT l-Acuse).
        END.

    END.
      
       
     
END PROCEDURE.



@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE CancelAcuse:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER pIdAcuse   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER pMotivo    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER pIdUser    AS CHARACTER NO-UNDO. /* Usuario a validar */
    DEFINE INPUT  PARAMETER pConfirmar AS LOGICAL NO-UNDO INITIAL FALSE. /* Confirmación del usuario */
    DEFINE INPUT  PARAMETER pIdUserSol AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER Respuesta  AS CHARACTER. 
    DEFINE OUTPUT PARAMETER IdError    AS LOGICAL.
    
    DEFINE VARIABLE l-bandera AS LOGICAL NO-UNDO INITIAL FALSE.
    
    IF pMotivo = ? THEN pMotivo = "".
    IF pIdUserSol = ? THEN pIdUserSol = "".
    IF pMotivo = "" THEN 
    DO:
        ASSIGN 
            Respuesta = "Se debe enviar Motivo de Cancelacion."
            IdError   = TRUE.
        RETURN.
    END.
    IF pConfirmar = ? THEN pConfirmar = FALSE.
    FIND Usuario WHERE Usuario.Id-User = pIdUser NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Usuario THEN 
    DO:
        ASSIGN 
            Respuesta = "El usuario especificado no existe."
            IdError   = TRUE.
        RETURN.
    END.
    FIND Usuario WHERE Usuario.Id-User = pIdUserSol NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Usuario THEN 
    DO:
        ASSIGN 
            Respuesta = "El usuario que Solicita la Cancelacion no existe."
            IdError   = TRUE.
        RETURN.
    END.
    
    FIND FIRST SysGeneral NO-LOCK NO-ERROR.
    FIND Acuse WHERE Acuse.Id-Acuse = pIdAcuse NO-LOCK NO-ERROR.
    IF AVAILABLE (Acuse) THEN 
    DO:
   
        IF Acuse.Id-nco <> '' THEN 
        DO:
            FIND Remision WHERE Remision.Id-Remision =
                Acuse.Id-NCO NO-LOCK NO-ERROR.
            IF AVAILABLE Remision THEN
                IF Remision.FecCanc = ? THEN 
                DO: 
                    ASSIGN 
                        Respuesta = 'No se permite cancelar, fue generada Remision.'
                        IdError   = TRUE.
                    RETURN.
                END.
            IF NOT AVAILABLE Remision THEN 
            DO:
                ASSIGN 
                    Respuesta = 'No se permite cancelar, fue generada Remision.'
                    IdError   = TRUE.
                RETURN.
            END.
        END.
        IF Acuse.Estatus = 3 THEN 
        DO: 
            ASSIGN
                Respuesta = "El Acuse esta Cancelado."
                IdError   = TRUE.
            RETURN.
        END.

        FOR EACH DetNco WHERE
            DetNco.Referencia = Acuse.Id-Acuse NO-LOCK:
            FIND NCO OF DetNco NO-LOCK NO-ERROR.
            IF NCO.FecCanc = ? THEN 
            DO: 
                ASSIGN
                    Respuesta = "El Acuse TIENE asociado NCO por DESC CANCELADOS."
                    IdError   = TRUE.
                RETURN.
            END.
        END.
        IF Acuse.Estatus = 4 THEN 
        DO:
            IF Acuse.FecDep <= SysGeneral.FecCieDep THEN 
            DO:
                         
                ASSIGN
                    Respuesta = "Fecha de deposito CERRADA en contabilidad, imposible modificar..."
                    IdError   = TRUE.
                RETURN.
            END.
        END.
        /*
        FIND FIRST CPago WHERE CPago.Id-Acuse = Acuse.Id-Acuse
            AND CPago.FecCanc = ?
            NO-LOCK NO-ERROR.
        IF AVAILABLE CPago THEN 
        DO: 
            RUN /usr2/adosa/procs/tesd0012.p(INPUT CPago.Id-CPago).
            RELEASE CPago.
        END. */ 
        
       
    END. /* end del available       */
    ELSE 
    DO: 
        ASSIGN
            Respuesta = "El Acuse no existe."
            IdError   = TRUE.
        RETURN.
    END. /* end del not available   */
    
    /* BLOQUE PARA DEFAFECTAR ACUSES */
    FIND Acuse WHERE Acuse.Id-Acuse = pIdAcuse NO-LOCK NO-ERROR.
    IF AVAILABLE Acuse THEN 
    DO:
        
        IF Acuse.Estatus = 4 THEN 
        DO: 
            IF Acuse.Tipo = "N" OR Acuse.Tipo = "P" THEN 
            DO:
              //  RUN cxca0241.p (INPUT RECID(Acuse)).
                
            END.

            IF Acuse.Tipo = "A" OR Acuse.Tipo = 'C' THEN 
            DO:
                RUN cxca0242.p (INPUT RECID(Acuse)).
            END.
        END. 
       // RELEASE Acuse.
    END.
    /*
    FIND Acuse WHERE Acuse.Id-Acuse = pIdAcuse NO-LOCK NO-ERROR.
    IF AVAILABLE Acuse THEN 
    DO:
        
        IF NOT pConfirmar THEN 
        DO:
            ASSIGN 
                Respuesta = "Confirme la cancelacion del Acuse No. " + STRING (Acuse.Id-Acuse).
            IdError = FALSE.
            RETURN.
        END.       
        */
        FIND bf_Acuse WHERE bf_Acuse.Id-Acuse = pIdAcuse EXCLUSIVE-LOCK NO-ERROR.
        ASSIGN 
            bf_Acuse.Estatus     = 3   /* ACUSE CANCELADO */
            bf_Acuse.UsuarioCanc = CAPS(pIdUser)     
            bf_Acuse.FecCanc     = TODAY
            bf_Acuse.UsuarioSol  = CAPS(pIdUserSol)
            bf_Acuse.Motivo      = pMotivo

            Respuesta         = "Acuse cancelado de manera exitosa. "
            IdError           = FALSE.
        RETURN.  
        /*
        RUN Cancel1 (INPUT RECID(Acuse),INPUT pIdUser, INPUT pIdUserSol, INPUT pMotivo,OUTPUT l-bandera).
        
        IF l-bandera = TRUE THEN 
        DO:
        
            ASSIGN
                Respuesta = "Acuse cancelado de manera exitosa. "
                IdError   = FALSE.
            RETURN.
        
        END. */ 
    
  //  END.              
   
/*
IF Acuse.Tipo = "A" OR Acuse.Tipo = 'C' THEN 
DO:  // cxca0232.i
    IF NOT pConfirmar THEN 
    DO:
        ASSIGN 
            Respuesta = "Confirme la cancelacion del Acuse No. " + STRING (Acuse.Id-Acuse).
        IdError = FALSE.
        RETURN.
    END.          
    DO TRANSACTION ON ENDKEY UNDO, LEAVE ON ERROR UNDO, LEAVE :
        FIND Acuse WHERE RECID(Acuse) = l-acuse EXCLUSIVE-LOCK NO-ERROR.
        IF Acuse.Estatus <> 3 THEN 
        DO:
            IF Acuse.Tipo = 'A' THEN 
            DO:
                FIND Anticipo OF Acuse EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE (Anticipo) THEN 
                DO:
                    IF Anticipo.Canc = TRUE THEN 
                    DO: 
                        ASSIGN
                            Respuesta = "El Anticipo ya fue Cancelado."
                            IdError   = TRUE.
                        RETURN.
                    END.
                    IF Anticipo.ImpAplicado <> 0 THEN 
                    DO:
                        ASSIGN
                            Respuesta = "El Anticipo fue Aplicado en Otras Facturas."
                            IdError   = TRUE.
                        RETURN.
                    END.
                    IF Anticipo.ImpDevuelto <> 0 THEN 
                    DO:
                        ASSIGN 
                            Respuesta = "El Anticipo tiene Devoluciones de Efectivo."
                            IdError   = TRUE.
                        RETURN.
                    END.
                    ASSIGN 
                        Anticipo.Canc     = TRUE
                        Acuse.Estatus     = 3
                        Acuse.UsuarioCanc = CAPS(pIdUser)
                        Acuse.FecCanc     = TODAY
                        Acuse.UsuarioSol  = CAPS(pIdUserSol)
                        Acuse.Motivo      = pMotivo.
                    IF Acuse.Id-Origen = "SA" OR Acuse.Id-Acuse MATCHES "*S" OR
                        Acuse.Id-Acuse MATCHES "*SA" THEN
                        ASSIGN Acuse.Act-Origen = TRUE
                            Acuse.FecAOrigen = TODAY.
                    ELSE 
                    DO:
                        FIND Cliente OF Acuse NO-LOCK NO-ERROR.
                        IF AVAILABLE Cliente THEN 
                        DO:
                            {cxca0009.i}
                            ASSIGN Acuse.Act-Origen = TRUE
                                Acuse.FecAOrigen = TODAY.
                        END.
                        END. /* si es de saltillo */
                        END.
                    END.
                    /****************************************************/
                    /*Borra cheques postfechados                        */
                    /****************************************************/
                    FOR EACH ChequePF WHERE ChequePF.Id-Acuse = Acuse.Id-Acuse 
                        EXCLUSIVE-LOCK:
                        DELETE ChequePF.
                    END.
                END.
            END.
            ELSE 
            DO:
                ASSIGN 
                    Acuse.Estatus     = 3
                    Acuse.UsuarioCanc = CAPS(pIdUser)
                    Acuse.FecCanc     = TODAY
                    Acuse.UsuarioSol  = CAPS(pIdUserSol)
                    Acuse.Motivo      = pMotivo.
                /****************************************************/
                /*Borra cheques postfechados                        */
                /****************************************************/
                FOR EACH ChequePF WHERE ChequePF.Id-Acuse = Acuse.Id-Acuse 
                    EXCLUSIVE-LOCK:
                    DELETE ChequePF.
                END.
            END.
        END.
    END.  /* DO transaction  */

    RELEASE Anticipo.
    IF Acuse.Estatus = 3 THEN  
    DO:
        FIND FIRST Empleado WHERE empleado.Iniciales = Acuse.UsuarioCanc NO-LOCK NO-ERROR.
        ASSIGN
            Respuesta = "Acuse cancelado correctamente " + STRING(Acuse.FecCanc) +
                   " por " + (IF AVAILABLE Empleado THEN Empleado.Nombre ELSE Acuse.UsuarioCanc)
            IdError   = FALSE.
        RETURN.  
    END.        
END. // IF Acuse.Tipo = "A" OR Acuse.Tipo = 'C'  */



END PROCEDURE.


