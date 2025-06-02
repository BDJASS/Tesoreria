@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : teserp020.p
    Purpose     : 

    Syntax      :

    Description : Reporte Ficha Depositos

    Author(s)   : sis10
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

DEF VAR l-cf           AS INTEGER   NO-UNDO.
DEF VAR l-Refer1       AS CHAR      NO-UNDO FORMAT 'x(6)'.
DEF VAR l-Articulo     LIKE Articulo.Id-Articulo NO-UNDO.
DEF VAR l-Fecha        AS DATE      FORMAT "99/99/9999" NO-UNDO.
DEF VAR l-FrameValue   AS CHAR      NO-UNDO.
DEF VAR l-SubTot       LIKE FichaDep.Importe NO-UNDO.
DEF VAR l-Depositos    LIKE PagoAcuse.Importe NO-UNDO.
DEF VAR l-Deudores     LIKE PagoAcuse.Importe NO-UNDO.
DEF VAR l-totME        AS DECIMAL   FORMAT "zzzzz,zz9.99" NO-UNDO.      /*_ RNPC _*/
DEF VAR l-totMEMN      AS DECIMAL   FORMAT "zzzzz,zz9.99" NO-UNDO.      /*_ RNPC _*/
DEF VAR l-totMEFac     AS DECIMAL   FORMAT "zzzzz,zz9.99" NO-UNDO.      /*_ RNPC _*/
DEF VAR l-totMEMNFac   AS DECIMAL   FORMAT "zzzzz,zz9.99" NO-UNDO.      /*_ RNPC _*/
DEF VAR l-totFacMNenUS AS DECIMAL   FORMAT "zzzzz,zz9.99" NO-UNDO.      /*_ RNPC _*/
DEF VAR l-totMNpagoUS  AS DECIMAL   FORMAT "zzzzz,zz9.99" NO-UNDO.     /*_ RNPC _*/
DEF VAR l-totMEMNPago  AS DECIMAL   FORMAT "zzzzz,zz9.99" NO-UNDO.     /*_ RNPC _*/
DEF VAR l-moneda       AS CHARACTER FORMAT "x(3)" NO-UNDO INITIAL "".  /*_ RNPC _*/
DEF VAR l-acuselst     AS CHARACTER NO-UNDO INITIAL "".                /*_ RNPC _*/
DEF VAR l-acuseMNenUS  AS CHARACTER NO-UNDO INITIAL "".                /*_ RNPC _*/
DEF VAR l-SubTotME     LIKE FichaDep.Importe NO-UNDO.                   /*_ RNPC _*/
DEF VAR l-total        LIKE PagoAcuse.Importe NO-UNDO.
DEF VAR l-total1       LIKE PagoAcuse.Importe NO-UNDO.
DEF VAR l-total2       LIKE PagoAcuse.Importe NO-UNDO.
DEF VAR l-ImpDeud      LIKE FichaDep.Importe NO-UNDO.
DEF VAR l-Sub1         LIKE FichaDep.Importe NO-UNDO.
DEF VAR l-Sub2         LIKE FichaDep.Importe NO-UNDO.
DEF VAR l-Sub3         LIKE FichaDep.Importe NO-UNDO.
DEF VAR l-Tot1         LIKE FichaDep.Importe NO-UNDO.
DEF VAR l-Tot2         LIKE FichaDep.Importe NO-UNDO.
DEF VAR l-Tot3         LIKE FichaDep.Importe NO-UNDO.
DEF VAR l-Reporte      AS CHAR      NO-UNDO.
DEF VAR l-Tipo         AS CHAR      NO-UNDO FORMAT "x(3)".
DEF VAR l-Poliza       AS CHAR      NO-UNDO FORMAT "x(6)".
DEF VAR l-NPol         LIKE Poliza.Id-Poliza NO-UNDO.
DEF VAR l-NSer         LIKE Poliza.Serie NO-UNDO.
DEF VAR l-NS           LIKE Cuenta.Id-SCta NO-UNDO.
DEF VAR l-cta          LIKE MovPoliza.Id-Cta NO-UNDO.
DEF VAR l-Scta         LIKE MovPoliza.Id-SCta NO-UNDO.
DEF VAR l-SScta        LIKE MovPoliza.Id-SSCta NO-UNDO.
DEF VAR l-SSScta       LIKE MovPoliza.Id-SSSCta NO-UNDO.
DEF VAR l-Sec          LIKE MovPoliza.Sec NO-UNDO.
DEF VAR l-cargo        AS DECI      FORMAT "zz,zzz,zz9.99".
DEF VAR l-TMax         AS INTEGER   NO-UNDO.
// DEF STREAM s-Salida.

DEF BUFFER b-FichaDep FOR FichaDep.

DEFINE TEMP-TABLE ttFichaDep NO-UNDO
    FIELD Id       AS CHAR
    FIELD IdBanco  LIKE FichaDep.Id-Banco
    FIELD Banco    LIKE Banco.NomCto
    FIELD Tipo     LIKE FichaDep.Tipo
    FIELD Clave    LIKE FichaDep.Clave
    FIELD IdCta    LIKE FichaDep.Id-Cta
    FIELD IdSCta   LIKE FichaDep.Id-SSCta
    FIELD IdSSCta  LIKE FichaDep.Id-SSCta
    FIELD IdSSSCta LIKE FichaDep.id-ssscta
    FIELD Descr    LIKE Cuenta.Descr
    FIELD Importe  LIKE FichaDep.Importe
    FIELD IdPoliza LIKE FichaDep.Id-Poliza
    FIELD Serie    LIKE FichaDep.Serie.

DEFINE TEMP-TABLE ttFichaSaldos NO-UNDO
    FIELD Id          AS CHAR
    FIELD DiarioDep   AS DECIMAL
    FIELD TotalDepCte AS DECIMAL
    FIELD DepOtros    AS DECIMAL
    FIELD DepTotal    AS DECIMAL.

DEFINE DATASET dsFicha FOR 
    ttFichaSaldos,
    ttFichaDep /* Tabla principal */
    DATA-RELATION Relacion FOR ttFichaSaldos, ttFichaDep
    RELATION-FIELDS (Id, Id).
    
DEFINE TEMP-TABLE ttCuentas NO-UNDO
    FIELD IdCta     LIKE FichaDep.Id-Cta
    FIELD IdSCta    LIKE FichaDep.Id-SSCta
    FIELD IdSSCta   LIKE FichaDep.Id-SSCta
    FIELD IdSSSCta  LIKE FichaDep.id-ssscta
    FIELD Descr     LIKE Cuenta.Descr 
    FIELD DOA       AS CHARACTER
    FIELD Afectable LIKE Cuenta.afectable. 
/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetCuentas:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipCuenta AS INT NO-UNDO.
    DEFINE INPUT  PARAMETER ipCuenta2 AS INT NO-UNDO.
    DEFINE INPUT  PARAMETER ipCuenta3 AS INT NO-UNDO.
    DEFINE INPUT  PARAMETER ipCuenta4 AS INT NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttCuentas.

    EMPTY TEMP-TABLE ttCuentas.

    IF ipCuenta = ? THEN ipCuenta = 0.
    IF ipCuenta2 = ? THEN ipCuenta2 = 0.
    IF ipCuenta3 = ? THEN ipCuenta3 = 0.
    IF ipCuenta4 = ? THEN ipCuenta4 = 0.

    FOR EACH Cuenta NO-LOCK
        WHERE Cuenta.Id-Cia = 1 
        AND Cuenta.Id-Cta <> 0  /* Siempre diferente de cero */
        AND (ipCuenta = 0 OR Cuenta.Id-Cta = ipCuenta)
        AND (ipCuenta2 = 0 OR Cuenta.Id-scta = ipCuenta2)
        AND (ipCuenta3 = 0 OR Cuenta.id-sscta = ipCuenta3)
        AND (ipCuenta4 = 0 OR Cuenta.id-ssscta = ipCuenta4)
        USE-INDEX Idx-descr:

        CREATE ttCuentas.
        ASSIGN
          
            ttCuentas.IdCta     = Cuenta.Id-Cta
            ttCuentas.IdSCta    = Cuenta.Id-scta
            ttCuentas.IdSSCta   = Cuenta.id-sscta
            ttCuentas.IdSSSCta  = Cuenta.id-ssscta
            ttCuentas.Descr     = Cuenta.Descr
            ttCuentas.DOA       = IF Cuenta.doa THEN "DEUD" ELSE "ACRR"
            ttCuentas.Afectable = Cuenta.afectable.
    END.
END PROCEDURE.

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetReporteFicha:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER l-Fecha AS DATE.
    DEFINE OUTPUT PARAMETER DATASET FOR dsFicha.
    
    
    DEFINE VARIABLE l-found AS LOGICAL NO-UNDO INIT FALSE.
 
    FOR EACH FichaDep WHERE FichaDep.FecReg = l-Fecha NO-LOCK
        USE-INDEX Idx-Fecha:
        l-found = TRUE.  /* Marcar que se encontraron registros */

        FIND Banco WHERE Banco.Id-Banco = FichaDep.Id-Banco
            NO-LOCK NO-ERROR.
        FIND Cuenta WHERE Cuenta.Id-Cia = 1 
            AND Cuenta.Id-Cta = FichaDep.Id-Cta
            AND Cuenta.Id-SCta = FichaDep.Id-SCta
            AND Cuenta.Id-SSCta = FichaDep.Id-SSCta
            AND Cuenta.Id-SSSCta = FichaDep.Id-SSSCta
            NO-LOCK NO-ERROR.
                                  
        CREATE ttFichaDep.
                 
        ASSIGN
            ttFichaDep.Id       = "T"
            ttFichaDep.IdBanco  = FichaDep.Id-Banco
            ttFichaDep.Banco    = IF AVAILABLE Banco THEN Banco.NomCto  ELSE ""
            ttFichaDep.Tipo     = FichaDep.Tipo 
            WHEN FichaDep.Tipo <> 0 
            ttFichaDep.Clave    = FichaDep.Clave 
            ttFichaDep.IdCta    = FichaDep.Id-Cta 
            WHEN FichaDep.Id-Cta <> 0
            ttFichaDep.IdSCta   = FichaDep.Id-SCta 
            WHEN FichaDep.Id-Cta <> 0
            ttFichaDep.IdSSCta  = FichaDep.Id-SSCta 
            WHEN FichaDep.Id-Cta <> 0
            ttFichaDep.IdSSSCta = FichaDep.Id-SSSCta 
            WHEN FichaDep.Id-Cta <> 0
            ttFichaDep.Descr    = IF AVAILABLE Cuenta THEN Cuenta.Descr ELSE ""
            ttFichaDep.Importe  = FichaDep.Importe
            ttFichaDep.IdPoliza = FichaDep.Id-Poliza
            ttFichaDep.Serie    = FichaDep.Serie.                            
                                  
    END.
    
    /* Solo ejecutar GetTotalesFicha si se encontraron registros */
    IF l-found THEN
        RUN GetTotalesFicha(INPUT l-Fecha).

END PROCEDURE.   
   
PROCEDURE GetTotalesFicha:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER vFecha AS DATE.
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
        WHERE Acuse.FecDep = vFecha
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
    
    l-SubTot = 0.
    l-Deudores = 0.
    l-Total = 0.
    l-SubTotME = 0.
    /* Procesamos FichaDep de ese día */
    FOR EACH b-FichaDep 
        WHERE b-FichaDep.FecReg = vFecha NO-LOCK:

        IF b-FichaDep.Clave = 'C' THEN 
        DO:
            IF b-FichaDep.Tipo = 6 THEN
                ACCUMULATE b-FichaDep.Importe (TOTAL).
            ELSE 
                l-SubTot = l-SubTot + b-FichaDep.Importe.
        END.
        ELSE
            l-Deudores = l-Deudores + b-FichaDep.Importe.
        l-Total = l-Total + b-FichaDep.Importe.
    END.

    /* Guardamos un registro en la tabla temporal por fecha */
    CREATE ttFichaSaldos.
    ASSIGN 
        ttFichaSaldos.Id          = "T"
        ttFichaSaldos.DiarioDep   = l-Depositos
        ttFichaSaldos.TotalDepCte = l-SubTot
        ttFichaSaldos.DepOtros    = l-Deudores
        ttFichaSaldos.DepTotal    = l-Total.

END PROCEDURE.

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE PostFichaDep:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER DATASET FOR dsFicha.
    DEFINE INPUT PARAMETER l-Fecha AS DATE.
    DEFINE INPUT  PARAMETER pConfirmar AS LOGICAL NO-UNDO INITIAL FALSE. /* Confirmación del usuario */
    DEFINE OUTPUT PARAMETER Respuesta  AS CHAR. 
    DEFINE OUTPUT PARAMETER IdError    AS LOGICAL.
    
        
    /* 2. Validar bancos permitidos - Versión optimizada */
    DEFINE VARIABLE cBancosPermitidos AS CHARACTER NO-UNDO INIT "1,11,13,18,20,25".
    
    IF l-Fecha = ? THEN 
    DO:
        ASSIGN
            Respuesta = "Error: El parámetro Fecha es obligatorio y debe ser una fecha válida"
            IdError   = TRUE.
        RETURN.
    END.
    
    FIND FolPolDep WHERE FolPolDep.FecReg = l-Fecha NO-LOCK NO-ERROR.
    IF NOT AVAILABLE FolPolDep THEN 
    DO:
        ASSIGN
            Respuesta = "Folio de Poliza de Deposito No Definido Para Esta Fecha " +
            "Favor de Avisar al Departamento de Sistemas"
            IdError   = TRUE.
        RETURN.
    END.
    IF AVAILABLE FolPolDep THEN 
    DO:
        ASSIGN 
            l-NPol = FolPolDep.Id-Poliza
            l-NSer = FolPolDep.Serie.
    END.

    FIND FIRST sysgeneral NO-LOCK NO-ERROR.
    IF NOT AVAILABLE sysgeneral THEN 
    DO:
        ASSIGN
            Respuesta = "No existe el registro en Sysgeneral." +
            "Verifique con el depto de sistemas."
            IdError   = TRUE.
        RETURN.
    END.

    IF l-fecha <= sysgeneral.fecciedep AND pConfirmar = FALSE THEN 
    DO:
        ASSIGN
            Respuesta = "La fecha de cierre de polizas de depositos es " +
           STRING(sysgeneral.fecciedep)  +
            " no podra hacer modificaciones, verifique."
            IdError   = TRUE.
        RETURN.
    END.
    
  
    

            
    FOR EACH ttFichaDep  :        
        IF ttFichaDep.Clave = ? THEN 
        DO:
            ASSIGN
                Respuesta = "Indique si es de (C)lientes, (D)eudores, o (A)creedores"
                IdError   = TRUE.
            RETURN.
        END.
        /* Validación de valores permitidos para Clave */
        IF LOOKUP(ttFichaDep.Clave, "C,D,A") = 0 THEN 
        DO:
            ASSIGN
                Respuesta = "Solo pueden ser de Clientes, Deudores o Acreedores"
                IdError   = TRUE.
            RETURN.
        END.
   
        IF ttFichaDep.IdBanco = ? THEN 
        DO:
            ASSIGN
                Respuesta = "Banco Inexistente"
                IdError   = TRUE.
            RETURN.     
        END. 
    
        /* VALIDACIÓN DE BANCOS PERMITIDOS */
        IF ttFichaDep.IdBanco <> 0 THEN 
        DO:
            /* 1. Validar que el banco exista en la BD */
            FIND FIRST Banco WHERE Banco.Id-Banco = ttFichaDep.IdBanco NO-LOCK NO-ERROR.
    
            IF NOT AVAILABLE Banco THEN 
            DO:
                ASSIGN
                    Respuesta = "Banco Inexistente en base de datos"
                    IdError   = TRUE.
                RETURN ERROR.
            END.

            IF LOOKUP(STRING(ttFichaDep.IdBanco), cBancosPermitidos) = 0 THEN 
            DO:
                ASSIGN
                    Respuesta = "Banco no aceptado para depositos, sin cuenta contable"
                    IdError   = TRUE.
                RETURN ERROR.
            END.
        END.
    
        IF ttFichaDep.Tipo = ?  THEN 
        DO:
            ASSIGN
                Respuesta = "Ingresar Tipo"
                IdError   = TRUE.
            RETURN.       
        END.
    
        IF ttFichaDep.IdBanco = 1 THEN 
            ASSIGN l-TMax = 6.
        ELSE IF ttFichaDep.IdBanco = 25 THEN 
                ASSIGN l-TMax = 3.
            ELSE 
                ASSIGN l-TMax = 2.
        /* Validación del tipo se mantiene igual */
        IF ttFichaDep.Tipo <> 0 AND 
            (ttFichaDep.Tipo < 1 OR ttFichaDep.Tipo > l-TMax) THEN 
        DO:     
            ASSIGN
                Respuesta = "Tipo debe ser entre 1 y " + STRING(l-TMax).
            IdError = TRUE.
            RETURN.     
        END.
    
        CREATE FichaDep.
        ASSIGN 
            FichaDep.FecReg    = l-Fecha
            FichaDep.Id-Poliza = l-NPol
            FichaDep.Serie     = l-NSer
            FichaDep.Clave     = ttFichaDep.Clave
            FichaDep.Id-Banco  = ttFichaDep.IdBanco
            FichaDep.Id-Cta    = ttFichaDep.IdCta
            FichaDep.Id-scta   = ttFichaDep.IdSCta
            FichaDep.id-sscta  = ttFichaDep.IdSSCta
            FichaDep.id-ssscta = ttFichaDep.IdSSSCta
            FichaDep.Importe   = ttFichaDep.Importe
            FichaDep.Tipo      = ttFichaDep.Tipo.
    
    END. // ttFichaDEp   
    
    /* Si llegamos hasta aquí, todas las validaciones fueron exitosas */
    ASSIGN
    IdError = FALSE
    Respuesta = "Registro creado correctamente".
             
   // ASSIGN Respuesta = "Folio" + STRING(        
    
END PROCEDURE.
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE DeleteDep:
    DEFINE INPUT PARAMETER DATASET FOR dsFicha.
    DEFINE INPUT PARAMETER l-Fecha AS DATE.
    DEFINE OUTPUT PARAMETER Respuesta AS CHARACTER.
    DEFINE OUTPUT PARAMETER IdError   AS LOGICAL INITIAL FALSE.
    
    /* Variables para registro de eliminación */
    DEFINE VARIABLE l-deleted AS INTEGER NO-UNDO.  
    
    IF l-Fecha = ? THEN 
    DO:
        ASSIGN
            Respuesta = "Error: El parámetro Fecha es obligatorio y debe ser una fecha válida"
            IdError   = TRUE.
        RETURN.
    END.
    
    FOR EACH ttFichaDep :
    
        IF ttFichaDep.Tipo = ?  THEN 
        DO:
            ASSIGN
                Respuesta = "Ingresar Tipo"
                IdError   = TRUE.
            RETURN.       
        END.
        IF ttFichaDep.Clave = ? THEN 
        DO:
            ASSIGN
                Respuesta = "Indique si es de (C)lientes, (D)eudores, o (A)creedores"
                IdError   = TRUE.
            RETURN.
        END.
        /* Validación de valores permitidos para Clave */
        IF LOOKUP(ttFichaDep.Clave, "C,D,A") = 0 THEN 
        DO:
            ASSIGN
                Respuesta = "Solo pueden ser de Clientes, Deudores o Acreedores"
                IdError   = TRUE.
            RETURN.
        END.
   
        IF ttFichaDep.IdBanco = ? THEN 
        DO:
            ASSIGN
                Respuesta = "Banco Inexistente"
                IdError   = TRUE.
            RETURN.     
        END. 
    
 
        
        /* Buscar y eliminar registro foráneo */
        FIND FIRST FichaDep WHERE FichaDep.Id-Banco = ttFichaDep.IdBanco
            AND FichaDep.Clave    = ttFichaDep.Clave
            AND FichaDep.FecReg   = l-Fecha
            AND FichaDep.Id-Cta   = ttFichaDep.IdCta
            AND FichaDep.Id-scta  = ttFichaDep.IdSCta
            AND FichaDep.id-sscta = ttFichaDep.IdSSCta
            AND FichaDep.id-ssscta = ttFichaDep.IdSSSCta
            AND FichaDep.Importe   = ttFichaDep.Importe
            EXCLUSIVE-LOCK NO-ERROR.
        
        IF AVAILABLE FichaDep THEN 
        DO:
            DELETE FichaDep.
            l-deleted = l-deleted + 1.
        END.
   
    END. // ttFichadep
    /* Resultado final */  
    IF l-deleted > 0 THEN
        Respuesta = "Registros eliminados: " + STRING(l-deleted).
    ELSE
        ASSIGN
            Respuesta = "No se encontraron registros para eliminar"
            IdError   = FALSE.   
    
END PROCEDURE.

