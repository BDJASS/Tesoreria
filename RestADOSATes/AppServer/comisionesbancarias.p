@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
USING OpenEdge.Web.* FROM PROPATH.
/*------------------------------------------------------------------------
    File        : comisionesbancarias.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : sis6
    Created     : Mon Apr 07 15:02:18 CST 2025
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

DEFINE TEMP-TABLE ttMovBanco
    FIELD IdMovimiento AS INTEGER
    FIELD IdBanco   AS INTEGER
    FIELD NomBanco  AS CHARACTER
    FIELD IdTipoMB  AS INTEGER
    FIELD Descr     AS CHARACTER
    FIELD NomCuenta AS CHARACTER
    FIELD TipoCC    AS LOGICAL
    FIELD IdCta     AS INTEGER
    FIELD IdSCta    AS INTEGER
    FIELD IdSSCta   AS INTEGER
    FIELD IdSSSCta  AS INTEGER
    FIELD Importe   AS DECIMAL
    FIELD Coment    AS CHARACTER.
    
 DEFINE TEMP-TABLE ttTotMovBanco
  FIELD IdMovimiento AS INTEGER
  FIELD TotCargo AS DECIMAL
  FIELD TotCredito AS DECIMAL
  FIELD CortCargo AS DECIMAL
  FIELD CortCredito AS DECIMAL.
  
  DEFINE DATASET dsMovimientos FOR ttTotMovBanco, ttMovBanco DATA-RELATION Movimientos FOR ttTotMovBanco, ttMovBanco RELATION-FIELDS (IdMovimiento, IdMovimiento).
    
DEF VAR l-FecIni AS DATE NO-UNDO.
DEF VAR l-FecFin AS DATE NO-UNDO.
DEF VAR l-NPol   LIKE Poliza.Id-Poliza NO-UNDO.
DEF VAR l-NSer   LIKE Poliza.Serie NO-UNDO.
DEF VAR l-NS     LIKE Cuenta.Id-SCta NO-UNDO.
DEF VAR l-cta    LIKE MovPoliza.Id-Cta NO-UNDO.
DEF VAR l-Scta   LIKE MovPoliza.Id-SCta NO-UNDO.
DEF VAR l-SScta  LIKE MovPoliza.Id-SSCta NO-UNDO.
DEF VAR l-SSScta LIKE MovPoliza.Id-SSSCta NO-UNDO.
DEF VAR l-Sec    LIKE MovPoliza.Sec NO-UNDO.
DEF VAR v-totcargo          AS   DECIMAL                                NO-UNDO.
DEF VAR v-totcredito        AS   DECIMAL                                NO-UNDO.
DEF VAR v-Cortcargo         AS   DECIMAL                                NO-UNDO.
DEF VAR v-Cortcredito       AS   DECIMAL                                NO-UNDO.

DEF TEMP-TABLE w-MovPol
    FIELD Id-Cta    LIKE MovPoliza.Id-Cta
    FIELD Id-SCta   LIKE MovPoliza.Id-SCta
    FIELD Id-SSCta  LIKE MovPoliza.Id-SSCta
    FIELD Id-SSSCta LIKE MovPoliza.Id-SSSCta
    FIELD Tipo      LIKE MovPoliza.Tipo
    FIELD Importe   LIKE MovPoliza.Importe
    INDEX Idx-Def Id-Cta Id-SCta Id-SSCta Id-SSSCta Tipo.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetComisionesBancarias:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER l-Fecha AS DATE FORMAT "99/99/9999" NO-UNDO.
    DEFINE OUTPUT PARAMETER DATASET FOR dsMovimientos.

    EMPTY TEMP-TABLE ttMovBanco.
    
    ASSIGN
    v-totcargo    = 0
    v-totcredito  = 0
    v-CortCargo   = 0
    v-CortCredito = 0.

    FOR EACH MovBanco WHERE MovBanco.FecReg = l-Fecha NO-LOCK:
    
        CREATE ttMovBanco.
        ASSIGN 
            ttMovBanco.IdMovimiento = 1
            ttMovBanco.IdBanco  = MovBanco.Id-Banco 
            ttMovBanco.IdTipoMB = MovBanco.Id-TipoMB
            ttMovBanco.Importe  = MovBanco.Importe
            ttMovBanco.Coment   = MovBanco.Coment.
            
        FIND Banco WHERE Banco.Id-Banco = movbanco.Id-Banco
            NO-LOCK NO-ERROR.
        IF AVAILABLE banco THEN
            ttMovBanco.NomBanco = banco.nomcto.
        ELSE
            ttMovBanco.NomBanco = 'NO DISP.'.
        
        FIND tipomb WHERE tipomb.id-tipomb = movbanco.id-tipomb
            NO-LOCK NO-ERROR.
                                      
                                      
        IF AVAILABLE tipomb THEN 
        DO:
            IF Tipomb.tipocc THEN
                ASSIGN v-totcargo = v-totcargo + movbanco.importe
                       v-CortCargo = v-CortCargo + MovBanco.Importe.
            ELSE
                ASSIGN v-totcredito = v-totcredito + movbanco.importe
                       v-CortCredito = v-CortCredito + MovBanco.Importe.
            FIND FIRST Empresa NO-LOCK NO-ERROR.           
            FIND Cuenta WHERE Cuenta.Id-Cia     = Empresa.Id-Cia  AND
                Cuenta.Id-Cta     = tipomb.id-cta   AND
                Cuenta.Id-SCta    = tipomb.id-scta  AND
                Cuenta.Id-SSCta   = tipomb.id-sscta AND
                Cuenta.Id-SSSCta  = tipomb.id-ssscta
                NO-LOCK NO-ERROR.
            IF AVAILABLE cuenta THEN
                ttMovBanco.NomCuenta = cuenta.descr.
            ELSE
                ttMovBanco.NomCuenta = 'NO DISP.'.

            ASSIGN
                ttMovBanco.Descr    = tipomb.descr
                ttMovBanco.IdCta    = tipomb.id-cta
                ttMovBanco.IdSCta   = tipomb.id-scta
                ttMovBanco.IdSSCta  = tipomb.id-sscta
                ttMovBanco.IdSSSCta = tipomb.id-ssscta
                ttMovBanco.TipoCC   = TipoMB.TipoCC.
        END.

        RELEASE ttMovBanco.
    END.
    
    CREATE ttTotMovBanco.
    ASSIGN ttTotMovBanco.IdMovimiento = 1
           ttTotMovBanco.TotCargo = v-totcargo
           ttTotMovBanco.TotCredito = v-totcredito
           ttTotMovBanco.CortCargo = v-CortCargo
           ttTotMovBanco.CortCredito = v-CortCredito.
           
    RELEASE ttTotMovBanco.

END PROCEDURE.

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE PostMovBanco:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER l-Fecha AS DATE FORMAT "99/99/9999" NO-UNDO.
    DEFINE INPUT PARAMETER TABLE FOR ttMovBanco.
    DEFINE OUTPUT PARAMETER iStatus AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER cMensaje    AS CHARACTER NO-UNDO.

    DO ON ERROR UNDO, THROW:
    DO TRANSACTION:
        l-FecIni = DATE(MONTH(l-Fecha),1,YEAR(l-Fecha)).
        IF MONTH(l-Fecha) = 12 THEN l-FecFin = DATE(1,1,YEAR(l-Fecha) + 1).
        ELSE l-FecFin = DATE(MONTH(l-Fecha) + 1,1,YEAR(l-Fecha)).
        l-NPol = MONTH(l-Fecha) * 2 - 1.
        l-NSer = "R".
   //STATUS DEFAULT "Generando/Actualizando Poliza " + STRING(l-NPol,"zzzz9") + l-NSer + "...".
   FIND FIRST Empresa NO-LOCK NO-ERROR.
        l-sec = 10.
        FOR EACH MovPoliza WHERE movpoliza.id-cia = Empresa.Id-Cia
            AND movpoliza.id-poliza = l-NPol
            AND movpoliza.serie = l-NSer
            AND movpoliza.anio = YEAR(l-Fecha)
            EXCLUSIVE-LOCK:

            /*    desafectacion del saldo de cuentas    */
            ASSIGN 
                l-cta    = movpoliza.Id-Cta
                l-scta   = movpoliza.Id-SCta
                l-sscta  = movpoliza.Id-SSCta
                l-ssscta = movpoliza.Id-SSSCta.
            DO WHILE l-cta > 0 :
                FIND Saldos WHERE Saldos.Id-Cia    = MovPoliza.Id-Cia AND
                    Saldos.Id-Cta    = l-cta          AND
                    Saldos.Id-SCta   = l-scta         AND
                    Saldos.Id-SSCta  = l-sscta        AND
                    Saldos.Id-SSSCta = l-ssscta       AND
                    Saldos.Anio      = MovPoliza.Anio AND
                    Saldos.Mes       = MONTH(MovPoliza.FecReg) 
                    EXCLUSIVE-LOCK NO-ERROR.
                IF NOT AVAILABLE Saldos THEN 
                DO:
                    /*  crear los saldos */
                    CREATE Saldos.
                    ASSIGN 
                        Saldos.Id-Cia    = 1
                        Saldos.Id-Cta    = l-cta
                        Saldos.Id-SCta   = l-scta
                        Saldos.Id-SSCta  = l-sscta
                        Saldos.Id-SSSCta = l-ssscta
                        Saldos.Mes       = MONTH(MovPoliza.FecReg)
                        Saldos.Anio      = MovPoliza.Anio
                        Saldos.Cargos    = 0
                        Saldos.Creditos  = 0.
                END. /* si no existe saldos */
                IF MovPoliza.Tipo = TRUE THEN
                    ASSIGN Saldos.Cargos = Saldos.Cargos - MovPoliza.Importe.
                ELSE ASSIGN Saldos.Creditos = Saldos.Creditos - MovPoliza.Importe.
                RELEASE Saldos.

                IF l-ssscta  <> 0 THEN ASSIGN l-ssscta = 0.
                ELSE IF l-sscta <> 0 THEN ASSIGN l-sscta = 0.
                    ELSE IF l-scta  <> 0 THEN ASSIGN l-scta = 0.
                        ELSE IF l-cta   <> 0 THEN ASSIGN l-cta = 0.
            END. /* el do while l-cta > 0 */

            DELETE MovPoliza.
        END.


        FIND FIRST Poliza WHERE Poliza.Id-cia = Empresa.Id-Cia
            AND Poliza.Id-poliza = l-NPol
            AND Poliza.Serie = l-NSer
            AND Poliza.Anio = YEAR(l-Fecha)
            EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE Poliza THEN 
        DO:
            CREATE Poliza.
            ASSIGN 
                Poliza.Id-cia    = Empresa.Id-Cia
                Poliza.Id-Poliza = l-NPol
                Poliza.Serie     = l-NSer
                Poliza.Anio      = YEAR(l-Fecha).
        END.
        ASSIGN 
            Poliza.FecReg = l-FecFin - 1.

        FOR EACH w-MovPol EXCLUSIVE-LOCK:
            DELETE w-MovPol.
        END.
   
   
        FOR EACH MovBanco WHERE MovBanco.FecReg = l-Fecha EXCLUSIVE-LOCK:
            DELETE MovBanco.
        END.


        FOR EACH ttMovBanco:
            CREATE MovBanco.
            ASSIGN
                MovBanco.Id-Banco  = ttMovBanco.IdBanco
                MovBanco.Id-TipoMB = ttMovBanco.IdTipoMB
                MovBanco.Importe   = ttMovBanco.Importe
                MovBanco.Coment    = ttMovBanco.Coment
                MovBanco.FecReg    = l-Fecha.
        END.

        FOR EACH MovBanco WHERE MovBanco.FecReg >= l-FecIni AND MovBanco.FecReg < l-FecFin NO-LOCK:
            FIND TipoMB WHERE TipoMB.Id-TipoMB = MovBanco.Id-TipoMB NO-LOCK NO-ERROR.
            IF NOT AVAILABLE TipoMB THEN NEXT.
            FIND FIRST w-MovPol WHERE w-MovPol.Id-Cta = TipoMB.Id-Cta
                AND w-MovPol.Id-SCta = TipoMB.Id-SCta
                AND w-MovPol.Id-SSCta = TipoMB.Id-SSCta
                AND w-MovPol.Id-SSSCta = TipoMB.Id-SSSCta
                AND w-MovPol.Tipo = TipoMB.TipoCC
                EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAILABLE w-MovPol THEN 
            DO:
                CREATE w-MovPol.
                ASSIGN 
                    w-MovPol.Id-Cta    = TipoMB.Id-Cta
                    w-MovPol.Id-SCta   = TipoMB.Id-SCta
                    w-MovPol.Id-SSCta  = TipoMB.Id-SSCta
                    w-MovPol.Id-SSSCta = TipoMB.Id-SSSCta
                    w-MovPol.Tipo      = TipoMB.TipoCC.
            END.
            ASSIGN 
                w-MovPol.Importe = w-MovPol.Importe + MovBanco.Importe.
            RELEASE w-MovPol.
       
            l-NS = 0.
            IF MovBanco.Id-Banco = 1 THEN l-NS = 4.
            ELSE IF MovBanco.Id-Banco = 11 THEN l-NS = 6.
                ELSE IF MovBanco.Id-Banco = 13 THEN l-NS = 5.
                    ELSE IF MovBanco.Id-Banco = 18 THEN l-NS = 6.
                        ELSE IF MovBanco.Id-Banco = 20 THEN l-NS = 3.
                            ELSE IF MovBanco.Id-Banco = 25 THEN l-NS = 2.
    
            FIND FIRST w-MovPol WHERE w-MovPol.Id-Cta = 1103
                AND w-MovPol.Id-SCta = l-NS
                AND w-MovPol.Id-SSCta = 0
                AND w-MovPol.Id-SSSCta = 0
                AND w-MovPol.Tipo = (IF TipoMB.TipoCC THEN FALSE ELSE TRUE)
                EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAILABLE w-MovPol THEN 
            DO:
                CREATE w-MovPol.
                ASSIGN 
                    w-MovPol.Id-Cta    = 1103
                    w-MovPol.Id-SCta   = l-NS
                    w-MovPol.Id-SSCta  = 0
                    w-MovPol.Id-SSSCta = 0
                    w-MovPol.Tipo      = (IF TipoMB.TipoCC THEN FALSE ELSE TRUE).
            END.
            ASSIGN 
                w-MovPol.Importe = w-MovPol.Importe + MovBanco.Importe.
            RELEASE w-MovPol.
        END.
        l-sec = 10.
        FOR EACH w-MovPol NO-LOCK 
            BY w-MovPol.Id-Cta
            BY w-MovPol.Id-SCta
            BY w-MovPol.Id-SSCta
            BY w-MovPol.Id-SSSCta
            BY w-MovPol.Tipo DESCENDING:
            CREATE MovPoliza.
            ASSIGN 
                MovPoliza.Id-cia    = Poliza.Id-Cia
                MovPoliza.Id-poliza = Poliza.Id-Poliza
                MovPoliza.Serie     = Poliza.Serie
                MovPoliza.Anio      = Poliza.Anio
                MovPoliza.Sec       = l-sec
                l-sec               = l-sec + 10
                MovPoliza.Id-Cta    = w-MovPol.Id-Cta
                MovPoliza.Id-SCta   = w-MovPol.Id-SCta
                MovPoliza.Id-SsCta  = w-MovPol.Id-SSCTa
                MovPoliza.Id-SssCta = w-MovPol.Id-SSSCta
                MovPoliza.FecReg    = Poliza.FecReg
                MovPoliza.Tipo      = w-MovPol.Tipo
                MovPoliza.Importe   = w-MovPol.Importe
                MovPoliza.Concepto  = "CARGOS BANCARIOS MENSUALES".
        END.
        RELEASE Poliza.

        FOR EACH MovPoliza WHERE movpoliza.id-cia = Empresa.Id-Cia
            AND movpoliza.id-poliza = l-NPol
            AND movpoliza.serie = l-NSer
            AND movpoliza.anio = YEAR(l-Fecha)
            NO-LOCK:

            /*    afectacion del saldo de cuentas    */
            ASSIGN 
                l-cta    = movpoliza.Id-Cta
                l-scta   = movpoliza.Id-SCta
                l-sscta  = movpoliza.Id-SSCta
                l-ssscta = movpoliza.Id-SSSCta.
            DO WHILE l-cta > 0 :
                FIND Saldos WHERE Saldos.Id-Cia = MovPoliza.Id-Cia AND
                    Saldos.Id-Cta    = l-cta                 AND
                    Saldos.Id-SCta   = l-scta                AND
                    Saldos.Id-SSCta  = l-sscta               AND
                    Saldos.Id-SSSCta = l-ssscta              AND
                    Saldos.Anio      = MovPoliza.Anio        AND
                    Saldos.Mes       = MONTH(MovPoliza.FecReg) 
                    EXCLUSIVE-LOCK NO-ERROR.
                IF NOT AVAILABLE Saldos THEN 
                DO:
                    /*  crear los saldos */
                    CREATE Saldos.
                    ASSIGN 
                        Saldos.Id-Cia    = MovPoliza.Id-Cia
                        Saldos.Id-Cta    = l-cta
                        Saldos.Id-SCta   = l-scta
                        Saldos.Id-SSCta  = l-sscta
                        Saldos.Id-SSSCta = l-ssscta
                        Saldos.Mes       = MONTH(MovPoliza.FecReg)
                        Saldos.Anio      = MovPoliza.Anio
                        Saldos.Cargos    = 0
                        Saldos.Creditos  = 0.
                END. /* si no existe saldos */
                IF MovPoliza.Tipo = TRUE THEN
                    ASSIGN Saldos.Cargos = Saldos.Cargos + MovPoliza.Importe.
                ELSE ASSIGN Saldos.Creditos = Saldos.Creditos + MovPoliza.Importe.
                RELEASE Saldos.

                IF l-ssscta  <> 0 THEN ASSIGN l-ssscta = 0.
                ELSE IF l-sscta <> 0 THEN ASSIGN l-sscta = 0.
                    ELSE IF l-scta  <> 0 THEN ASSIGN l-scta = 0.
                        ELSE IF l-cta   <> 0 THEN ASSIGN l-cta = 0.
            END. /* el do while l-cta > 0 */
        END.
        
   //STATUS DEFAULT "".
    END. /* do transaction */
    ASSIGN
            iStatus   = 200
            cMensaje  = "Proceso finalizado correctamente".
            
    END.
    CATCH e AS Progress.Lang.Error:
        ASSIGN
            iStatus  = 500
            cMensaje = "Error: " + e:GetMessage(1).           
        
    END CATCH.
    
    
END PROCEDURE.

