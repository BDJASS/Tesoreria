@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*------------------------------------------------------------------------
    File        : teserp005.p
    Purpose     : HU03   /ChequesDevueltos  GET
                  
                  COMO Responsable de Tesoreria, QUIERO poder registrar cheques PARA 
                  registrar los cargos a los que han sido devueltos, asi como poder 
                  realizar búsquedas mediante el # de referencia de registros anteriores.
    Author(s)   : sis10
    Created     : Fecha actual   
  ----------------------------------------------------------------------*/

  /* ticket: 465 Azure: Solicitud que en pantalla descripcion muestre CHEDEV
                        y no CHE DEV. {JASS}
                        
  */
/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW. /* Manejo de errores global */

/* **********************  Internal Procedures  *********************** */



/* ***************************  Main Procedure *************************** */

DEF VAR l-refer      AS CHAR.
DEF VAR l-entro      AS CHAR    NO-UNDO.
DEF VAR recid2       AS RECID   NO-UNDO.
DEF VAR l-Paso       AS LOG     NO-UNDO.
DEF VAR x            AS CHAR    NO-UNDO.
DEF VAR l-comision   AS DECI    LABEL "Aviso Cargo" FORMAT "zzz,zz9.99" .
DEF VAR l-imp        AS DECI    FORMAT "z,zzz,zz9.99" NO-UNDO.
DEF VAR l-iva        AS DECI    FORMAT "z,zzz,zz9.99" NO-UNDO.
DEF VAR l-iva1       AS DECI    FORMAT "z,zzz,zz9.99" NO-UNDO.
DEF VAR Resp         AS LOGICAL INITIAL TRUE FORMAT "Si/No" NO-UNDO.
DEF VAR l-teclas1    AS CHAR    FORMAT "x(20)" INITIAL "RETURN,ENTER,GO" NO-UNDO.
DEF VAR l-menu       AS CHAR    FORMAT "x(12)" EXTENT 2
    INITIAL ["VerSaldo","Salir"] NO-UNDO.
DEF VAR l-aviso      LIKE AvisoCargo.Id-Aviso.
DEF VAR l-teclas     AS CHAR    FORMAT "x(20)"
    INITIAL "F1,RETURN,ENTER,GO,TAB,CURSOR-RIGHT,CURSOR-LEFT,CURSOR-DOWN,CURSOR-UP"
    NO-UNDO.
DEF VAR l-banco1     AS CHAR    FORMAT "x(15)" NO-UNDO.
DEF VAR l-banco2     AS CHAR    FORMAT "x(15)" NO-UNDO.


DEF VAR l-Saldo      AS DECIMAL FORMAT "Z,zzz,zz9.99-" NO-UNDO.
DEF VAR l-folio      LIKE folio.folio NO-UNDO.
DEF VAR l-recid      AS RECID   NO-UNDO.
DEF VAR l-rec        AS RECID   NO-UNDO.
DEF VAR l-recid-ac   AS RECID   NO-UNDO.
DEF VAR l-total      AS DECIMAL FORMAT "zzz,zzz,zz9.99" NO-UNDO.
DEF VAR v-calidadant LIKE cliente.id-calidad NO-UNDO.
DEF VAR v-calidadnew LIKE cliente.id-calidad NO-UNDO.
DEF VAR l-fecvence   AS DATE.
DEF VAR l-ubic       AS CHAR .
DEF VAR l-recmov     AS RECID.

DEF VAR l-SaldoSeek AS DECI FORMAT "-ZZZ,ZZ9.99" NO-UNDO.
    DEF VAR indice      AS INT  NO-UNDO.
    DEF VAR l-saldo2    AS DECI LABEL "Saldo" FORMAT "-z,zzz,zz9.99" NO-UNDO.

DEFINE TEMP-TABLE ttCheque NO-UNDO
    FIELD Id          AS INT
    FIELD IdChedev    LIKE CheDev.Id-CheDev 
    FIELD IdCliente   LIKE CheDev.Id-Cliente
    FIELD FecCargo    LIKE CheDev.FecCargo
    FIELD RazonSocial LIKE Chedev.RazonSocial 
    FIELD FecCanc     LIKE Chedev.FecCanc
    FIELD Tel1        LIKE CheDev.Tel1
    FIELD BancoCo     LIKE CheDev.BancoCO
    FIELD Banco       AS CHAR
    FIELD CtaCheqCo   LIKE CheDev.CtaCheqCO
    FIELD Cheque      LIKE CheDev.Cheque
    FIELD ImpCheque   LIKE CheDev.ImpCheque
    FIELD Comision    AS DECIMAL
    FIELD ImpComision LIKE CheDev.ImpComision
    FIELD Aviso       LIKE AvisoCargo.Id-Aviso
    FIELD Total       LIKE l-total
    FIELD BancoDe     LIKE CheDev.BancoDE
    FIELD Banco2      AS CHAR
    FIELD CtaCheqDE   LIKE CheDev.CtaCheqDE
    FIELD Comen1      LIKE CheDev.Comen
    FIELD Comen2      LIKE CheDev.Comen
    FIELD Comen3      LIKE CheDev.Comen.


DEF TEMP-TABLE ttSaldo NO-UNDO
    FIELD Id        AS INT 
    FIELD RefSaldo  LIKE MovCliente.RefSaldo 
    FIELD FecReg    LIKE MovCliente.FecReg
    FIELD Documento LIKE MovCliente.Documento
    FIELD Descr     LIKE TabMC.Descr
    FIELD Cargo     AS DECI LABEL "Cargo" FORMAT "zzz,zz9.99"
    FIELD Credito   AS DECI LABEL "Credito" FORMAT "zzz,zz9.99"
    FIELD Saldo     LIKE l-saldo2
    .
       
DEF BUFFER b-Cliente FOR Cliente.


DEFINE DATASET dsCheque FOR 
    ttCheque, /* Tabla principal */
    ttSaldo
    DATA-RELATION SucDetalle FOR ttCheque, ttSaldo
    RELATION-FIELDS (Id,Id).
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetChequeDevuelto:

    DEFINE INPUT PARAMETER l-Id-CheDev LIKE Chedev.Id-Chedev.
    DEFINE OUTPUT PARAMETER DATASET FOR dsCheque.
    
    IF LENGTH(TRIM(l-Id-CheDev)) <= 6 AND NOT l-Id-CheDev BEGINS "C" THEN DO:
    l-Id-CheDev = "C" + FILL("0", 6 - LENGTH(TRIM(l-Id-CheDev))) + TRIM(l-Id-CheDev).
     END.

    FIND CheDev WHERE CheDev.Id-CheDev = l-Id-CheDev
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE CheDev THEN RETURN.
    IF AVAILABLE CheDev  THEN 
    DO:
        ASSIGN 
            l-recid = RECID(CheDev).
    END. 



    FIND CheDev WHERE RECID(CheDev) = l-recid NO-LOCK NO-ERROR.
    IF AVAILABLE CheDev THEN 
    DO:
        FIND Cliente WHERE Cliente.Id-Cliente = CheDev.Id-Cliente
            NO-LOCK NO-ERROR.

        v-calidadant = cliente.id-calidad.

        FIND Banco WHERE Banco.Id-Banco = CheDev.BancoCO NO-LOCK NO-ERROR.
        ASSIGN 
            l-Banco1 = Banco.Nombre
            l-recid  = RECID(CheDev).
        FIND Banco WHERE Banco.Id-Banco = CheDev.BancoDE NO-LOCK NO-ERROR.

        FIND FIRST AvisoCargo WHERE AvisoCargo.Id-Cliente =
            Cliente.Id-Cliente AND AvisoCargo.Id-CheDev  = CheDev.Id-CheDev
            NO-LOCK NO-ERROR.
        ASSIGN 
            l-total = CheDev.ImpCheque + CheDev.ImpComision.
        IF AVAILABLE AvisoCargo THEN
            ASSIGN l-entro    = "V"
                l-comision = AvisoCargo.Importe + AvisoCargo.Iva
                l-aviso    = AvisoCargo.Id-Aviso.
        ELSE
            ASSIGN l-entro    = 'I'
                l-comision = 0
                l-aviso    = 0.

        ASSIGN 
            l-Banco2 = Banco.Nombre.
        CREATE ttCheque.
        ASSIGN 
            ttCheque.Id          = 1
            ttCheque.IdCheDev    = CheDev.Id-CheDev
            ttCheque.IdCliente   = CheDev.Id-Cliente
            ttCheque.FecCargo    = CheDev.FecCargo
            ttCheque.RazonSocial = IF Chedev.Id-Cliente <> 3 THEN Cliente.RazonSocial
                   ELSE Chedev.RazonSocial 
            ttCheque.FecCanc     = Chedev.FecCanc
            ttCheque.Tel1        = IF Chedev.ID-Cliente <> 3 THEN Cliente.Tel1
                   ELSE Chedev.Tel
            ttCheque.BancoCo     = CheDev.BancoCO
            ttCheque.Banco       = l-Banco1
            ttCheque.CtaCheqCo   = CheDev.CtaCheqCO
            ttCheque.Cheque      = CheDev.Cheque
            ttCheque.ImpCheque   = CheDev.ImpCheque
            ttCheque.Comision    = l-comision
            ttCheque.ImpComision = Chedev.ImpComision
            ttCheque.Aviso       = l-aviso
            ttCheque.Total       = l-total
            ttCheque.BancoDe     = CheDev.BancoDE
            ttCheque.Banco2      = l-Banco2
            ttCheque.CtaCheqDE   = CheDev.CtaCheqDE    
            ttCheque.Comen1      = CheDev.Comen[1]
            ttCheque.Comen2      = CheDev.Comen[2]
            ttCheque.Comen2      = CheDev.Comen[3] .

        RELEASE Cliente.
    END.
    
    
    DO TRANSACTION :
        FOR EACH ttSaldo:
            DELETE ttSaldo.
        END.
        FIND CheDev WHERE RECID(CheDev) = l-recid NO-LOCK NO-ERROR.
        FIND FIRST AvisoCargo OF CheDev NO-LOCK NO-ERROR.
        IF Chedev.ImpCheque > 0 THEN 
        DO:
            CREATE ttSaldo.
            ASSIGN 
                ttSaldo.Id        = 1
                ttSaldo.Cargo     = Chedev.ImpCheque
                ttSaldo.RefSaldo  = CheDev.Id-CheDev
                ttSaldo.FecReg    = CheDev.FecCargo
                ttSaldo.Documento = CheDev.Id-CheDev
                ttSaldo.descr     = 'CHEDEV'.   // 22/04/2025 JASS
        END.
        IF CheDev.ImpComision > 0 THEN 
        DO:
            CREATE ttSaldo.
            ASSIGN 
                ttSaldo.Id        = 1
                ttSaldo.Cargo     = CheDev.ImpComision
                ttSaldo.RefSaldo  = CheDev.Id-CheDev
                ttSaldo.FecReg    = CheDev.FecCargo
                ttSaldo.Documento = CheDev.Id-CheDev
                ttSaldo.descr     = 'COM BAN'.
        END.
        IF AVAILABLE AvisoCargo THEN DO:
        IF AvisoCargo.Importe + AvisoCargo.IVA > 0 THEN 
        DO:
            CREATE ttSaldo.
            ASSIGN 
                ttSaldo.Id        = 1
                ttSaldo.Cargo     = AvisoCargo.Importe + AvisoCargo.IVA
                ttSaldo.RefSaldo  = STRING(AvisoCargo.Id-Aviso,"9999999")
                ttSaldo.FecReg    = AvisoCargo.FecReg
                ttSaldo.Documento = STRING(AvisoCargo.Id-Aviso,"9999999")
                ttSaldo.descr     = 'AVISOC'.
        END.
        END.
        IF Chedev.ImpInteres > 0 THEN 
        DO:
            CREATE ttSaldo.
            ASSIGN
                ttSaldo.Id        = 1  
                ttSaldo.Cargo     = Chedev.ImpInteres
                ttSaldo.RefSaldo  = Chedev.Id-chedev

                ttSaldo.Documento = Chedev.Id-Chedev
                ttSaldo.descr     = 'INTERES'.
        END.

        FOR EACH Movcliente WHERE MovCliente.RefSaldo = CheDev.Id-CheDev
            NO-LOCK BY MovCliente.FecReg BY MovCliente.Id-mc:
            IF MovCliente.Importe < 0 THEN 
            DO:
                FIND TabMc OF MovCliente NO-LOCK NO-ERROR.
                CREATE ttSaldo.
                ASSIGN 
                    ttSaldo.Id        = 1
                    ttSaldo.Credito   = MovCliente.Importe * -1
                    ttSaldo.RefSaldo  = MovCliente.RefSaldo
                    ttSaldo.FecReg    = MovCliente.FecReg
                    ttSaldo.Documento = MovCliente.Documento
                    ttSaldo.descr     = Tabmc.Descr.
            END.
        END.

        FOR EACH HistMovCte WHERE HistMovCte.RefSaldo = CheDev.Id-CheDev
            NO-LOCK BY HistMovCte.FecReg BY HistMovCte.Id-mc:
            IF HistMovCte.Importe < 0 THEN 
            DO:
                FIND TabMc OF HistMovCte NO-LOCK NO-ERROR.
                CREATE ttSaldo.
                ASSIGN 
                    ttSaldo.Id        = 1
                    ttSaldo.Credito   = HistMovCte.Importe * -1
                    ttSaldo.RefSaldo  = HistMovCte.RefSaldo
                    ttSaldo.FecReg    = HistMovCte.FecReg
                    ttSaldo.Documento = HistMovCte.Documento
                    ttSaldo.descr     = Tabmc.Descr.
            END.
        END.

        FOR EACH MovAviso WHERE MovAviso.Id-Chedev = Chedev.Id-Chedev
            NO-LOCK BY MovAviso.FecMov:
            IF MovAviso.ImpPago > 0 THEN 
            DO:
                CREATE ttSaldo.
                ASSIGN 
                    ttSaldo.Id        = 1
                    ttSaldo.Credito   = MovAviso.ImpPago
                    ttSaldo.RefSaldo  = CheDev.Id-CheDev
                    ttSaldo.FecReg    = MovAviso.FecMov
                    ttSaldo.Documento = MovAviso.Id-Remision
                    ttSaldo.descr     = IF MovAviso.CveMov = 'A' THEN
                                            'PAGO AVISO'
                                         ELSE IF MovAviso.CveMov = 'C' THEN
                                            'PAGO COMISION'
                                         ELSE
                                            'PAGO INTERES'.
            END.
            IF MovAviso.ImpDesc > 0 THEN 
            DO:
                CREATE ttSaldo.
                ASSIGN 
                    ttSaldo.Id        = 1
                    ttSaldo.Credito   = MovAviso.ImpDesc
                    ttSaldo.RefSaldo  = CheDev.Id-CheDev
                    ttSaldo.FecReg    = MovAviso.FecMov
                    ttSaldo.Documento = MovAviso.Id-Acuse
                    ttSaldo.descr     = IF MovAviso.CveMov = 'A' THEN
                                            'DESC AVISO'
                                         ELSE IF MovAviso.CveMov = 'C' THEN
                                            'DESC COMISION'
                                         ELSE
                                            'DESC INTERES'.
            END.
            IF MovAviso.ImpCanc > 0 THEN 
            DO:
                CREATE ttSaldo.
                ASSIGN 
                    ttSaldo.Id        = 1
                    ttSaldo.Credito   = MovAviso.ImpCanc
                    ttSaldo.RefSaldo  = CheDev.Id-CheDev
                    ttSaldo.FecReg    = MovAviso.FecMov
                    ttSaldo.Documento = MovAviso.Id-Acuse
                    ttSaldo.descr     = IF MovAviso.CveMov = 'A' THEN
                                            'CANC AVISO'
                                         ELSE IF MovAviso.CveMov = 'C' THEN
                                            'CANC COMISION'
                                         ELSE
                                            'CANC INTERES'.      
            END.
        END.
    END.         
    ASSIGN 
        l-saldoseek = 0.
    FOR EACH ttSaldo :
        ASSIGN 
            l-saldoseek   = l-saldoseek + ttSaldo.cargo - ttSaldo.credito
            ttSaldo.saldo = l-saldoseek.
    END.
    /*
    /*busca en base de datos historicas */
    IF l-saldoseek <> 0 THEN 
    DO:
        {resa0004.i
        &run    = "RUN tesa0523.p(INPUT Chedev.Id-Chedev)." 
        &AnoMax = "YEAR(CheDev.FecCargo) - 1"}
    END. */     
END PROCEDURE.