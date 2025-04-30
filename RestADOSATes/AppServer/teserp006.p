@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*------------------------------------------------------------------------
    File        : teserp006.p
    Purpose     : HU03   /ChequesDevueltos  POST
                  
                  COMO Responsable de Tesoreria, QUIERO poder registrar cheques PARA 
                  registrar los cargos a los que han sido devueltos, asi como poder 
                  realizar búsquedas mediante el # de referencia de registros anteriores.
    Author(s)   : sis10
    Created     : Fecha actual   
  ----------------------------------------------------------------------*/

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

DEF VAR l-SaldoSeek  AS DECI    FORMAT "-ZZZ,ZZ9.99" NO-UNDO.
DEF VAR indice       AS INT     NO-UNDO.
DEF VAR l-saldo2     AS DECI    LABEL "Saldo" FORMAT "-z,zzz,zz9.99" NO-UNDO.

DEFINE TEMP-TABLE ttCheque NO-UNDO
    FIELD Id          AS INT
    FIELD IdUser      AS CHAR
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
    FIELD Comen1      AS CHAR
    FIELD Comen2      LIKE CheDev.Comen
    FIELD Comen3      LIKE CheDev.Comen.


       
DEF BUFFER b-Cliente FOR Cliente.



@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE PostChequeDev:
    DEFINE INPUT PARAMETER TABLE FOR ttCheque.
    DEFINE OUTPUT PARAMETER IdError    AS LOGICAL.
    DEFINE OUTPUT PARAMETER Respuesta  AS CHAR. 
    

    DO TRANSACTION:
        FIND Folio WHERE Folio.Id-Doc = "TCheDev" EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE Folio THEN 
        DO: ASSIGN 
            Respuesta = "No existe folio temporal de cheques devueltos."
            IdError = TRUE.

        END.
        ASSIGN 
            l-folio     = Folio.Folio
            folio.folio = folio.folio + 1.
        RELEASE folio.
    END.

    FOR EACH ttCheque :      

    FIND b-Cliente WHERE b-Cliente.Id-Cliente = ttCheque.IdCliente  NO-LOCK NO-ERROR.
    IF NOT AVAILABLE b-Cliente THEN 
    DO: ASSIGN
        Respuesta =  "No existe el cliente."
        IdError = TRUE.
    END.
           


    CREATE CheDev.
    ASSIGN 
        CheDev.Id-CheDev   = STRING(l-folio)
        CheDev.FecCargo    = TODAY
        CheDev.Id-Cliente  = ttCheque.IdCliente
        l-recid-ac         = 0
        Chedev.RazonSocial = b-Cliente.RazonSocial
        Chedev.Tel1        = b-Cliente.Tel1
        CheDev.BancoCO     = ttCheque.BancoCo
        CheDev.CtaCheqCO   = ttCheque.CtaCheqCo // Favor de teclear el numero de cuenta
        CheDev.Cheque      = ttCheque.Cheque   // "Favor de teclear el numero de cheque"
        CheDev.ImpCheque   = ttCheque.ImpCheque // "Favor de teclear el importe del cheque"
        CheDev.BancoDE     = ttCheque.BancoDe
        CheDev.CtaCheqDE   = ttCheque.CtaCheqDE
        CheDev.Comen       = ttCheque.Comen1.




    FIND folio WHERE folio.id-doc = "CheDev" EXCLUSIVE-LOCK NO-ERROR.
    FIND FIRST Calidad WHERE Calidad.CheDev = TRUE NO-LOCK NO-ERROR.
    FIND Cliente OF CheDev EXCLUSIVE-LOCK NO-ERROR.

    ASSIGN 
        v-calidadant     = cliente.id-calidad
        v-calidadnew     = 0
        CheDev.Id-CheDev = Folio.Prefijo +
                                        STRING(Folio.Folio,'999999')
        Folio.Folio      = Folio.Folio + 1
        l-recid          = RECID(CheDev)
        l-rec            = RECID(Cliente).

    IF AVAILABLE (Calidad) THEN 
    DO:

        ASSIGN 
            Cliente.Id-Calidad = Calidad.Id-Calidad
            v-calidadnew       = cliente.id-calidad.

        IF v-calidadnew <> v-calidadant THEN 
        DO TRANSACTION:

            CREATE cambiocte.
            ASSIGN 
                cambiocte.id-cliente = Cliente.Id-Cliente
                cambiocte.id-user    = ttCheque.IdUser
                cambiocte.descr      = "id-Calidad"
                cambiocte.valornuevo = STRING(v-calidadnew)
                cambiocte.valorold   = STRING(v-calidadant)
                cambiocte.fecreg     = TODAY
                cambiocte.hora       = TIME
                cambiocte.campo      = 505.
        END.

        RELEASE CambioCte.   
               
    END.

    {cxca0006.i
              &Cliente = CheDev.Id-Cliente
              &Importe = " (CheDev.ImpCheque) " /* + CheDev.ImpComision) " */
              &renglon = 6
              &fecha   = CheDev.FecCargo }

        IF Cliente.CtCheq1 = CheDev.CtaCheqCo THEN 
        ASSIGN Cliente.Blk1 = TRUE.  
        ELSE IF Cliente.CtaCheq2 = CheDev.CtaCheqCo THEN
        ASSIGN Cliente.Blk2 = TRUE.
        ELSE IF Cliente.CtaCheq3 = CheDev.CtaCheqCo THEN 
        ASSIGN Cliente.Blk3 = TRUE.
    {cxca0001.i
              &TipoMov     = 3
              &TipoPadre   = 3
              &FecReg      = Chedev.Feccargo
              &FecVence    = Chedev.Feccargo
              &Documento   = CheDev.Id-CheDev
              &RefSaldo    = CheDev.Id-CheDev
              &Importe     = " (CheDev.ImpCheque)" /* + CheDev.ImpComision) " */
              &Afectar     = TRUE
              &Cliente     = CheDev.Id-Cliente
              &Ubic        = 'TE'  }

    RELEASE Folio.
    ASSIGN 
        Resp = TRUE.
    FIND Folio WHERE Folio.Id-Doc = "AVISO" EXCLUSIVE-LOCK NO-ERROR.
    ASSIGN 
        l-folio     = Folio.Folio
        Folio.Folio = Folio.Folio + 1 .
    DISP l-folio @ l-aviso CheDev.Id-CheDev WITH FRAME f-alta1.
    FIND FIRST SysGeneral NO-LOCK NO-ERROR.
    CREATE AvisoCargo.
    ASSIGN 
        l-imp                 = (Chedev.ImpCheque +
                              Chedev.ImpComision) * (SysGeneral.ComcheDev / 100)
        l-iva                 = ((CheDev.ImpCheque +
                           Chedev.ImpComision) * (SysGeneral.ComCheDev / 100)) *
                                                     (SysGeneral.Porc-IVA / 100)
        l-iva1                = SysGeneral.Porc-IVA
        AvisoCargo.IVA        = l-iva
        AvisoCargo.PorcIVA    = l-iva1
        AvisoCargo.Importe    = l-imp
        AvisoCargo.Id-Aviso   = l-folio
        AvisoCargo.Id-CheDev  = CheDev.Id-CheDev
        AvisoCargo.Id-Cliente = CheDev.Id-Cliente
        AvisoCargo.FecReg     = TODAY
        AvisoCargo.Usuario    = ttCheque.IdUser
        l-recid-ac            = RECID(AvisoCargo)
        l-recid               = RECID(CheDev)
        l-refer               = Chedev.Id-chedev.
END. // TTCHEQUE
RELEASE Cliente.
RELEASE EstCte.  
RELEASE Folio.
RELEASE MovCliente.
RELEASE CheDev.
RELEASE AvisoCargo.
IF l-recid-ac > 0 THEN 
DO: ASSIGN
    Respuesta = 'Cheque Devuelto registrado con Referencia: ' +
        STRING(l-refer)
    IdError = FALSE.
    RETURN.
END.
 
     
     
     
     
END PROCEDURE.