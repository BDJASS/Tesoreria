@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : teserp010.p
    Purpose     : 

    Syntax      :

    Description : HU06 Ficha Depositos

    Author(s)   : sis10
    Created     : Thu May 01 20:28:23 CST 2025
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
    FIELD Importe  LIKE FichaDep.Importe.

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
PROCEDURE GetFichaDepositos:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER l-Fecha AS DATE.
    DEFINE INPUT  PARAMETER pConfirmar AS LOGICAL NO-UNDO INITIAL FALSE. /* Confirmación del usuario */
    DEFINE OUTPUT PARAMETER DATASET FOR dsFicha.
    DEFINE OUTPUT PARAMETER IdError    AS LOGICAL.
    DEFINE OUTPUT PARAMETER Respuesta  AS CHAR.
    
    FIND FolPolDep WHERE FolPolDep.FecReg = l-Fecha NO-LOCK NO-ERROR.
    IF NOT AVAILABLE FolPolDep THEN 
    DO:
        ASSIGN
        Respuesta = "Folio de Poliza de Deposito No Definido Para Esta Fecha " +
            "Favor de Avisar al Departamento de Sistemas"
            IdError = TRUE.
            RETURN.
    END.

    FIND FIRST sysgeneral NO-LOCK NO-ERROR.
    IF NOT AVAILABLE sysgeneral THEN 
    DO:
        ASSIGN
        Respuesta =  "No existe el registro en Sysgeneral." +
            "Verifique con el depto de sistemas."
        IdError = TRUE.
        RETURN.
    END.

    IF l-fecha <= sysgeneral.fecciedep AND pConfirmar = FALSE THEN 
    DO:
        ASSIGN
        Respuesta = "La fecha de cierre de polizas de depositos es " +
           STRING(sysgeneral.fecciedep)  +
            " no podra hacer modificaciones, verifique."
            IdError = TRUE.
            RETURN.
    END.
    
 
    FOR EACH FichaDep WHERE FichaDep.FecReg = l-Fecha NO-LOCK
        USE-INDEX Idx-Fecha:


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
            ttFichaDep.Importe  = FichaDep.Importe.                            
                                  
    END.


END PROCEDURE.

