
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
  FIELD Id AS CHAR
  FIELD DiarioDep AS DECIMAL
  FIELD TotalDepCte AS DECIMAL
  FIELD DepOtros    AS DECIMAL
  FIELD DepTotal    AS DECIMAL.

DEFINE DATASET dsFicha FOR 
    ttFichaSaldos,
    ttFichaDep /* Tabla principal */
    DATA-RELATION Relacion FOR ttFichaSaldos, ttFichaDep
    RELATION-FIELDS (Id, Id).  
/* **********************  Internal Procedures  *********************** */

PROCEDURE GetFichaDepositos:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER l-Fecha AS DATE.
    DEFINE OUTPUT PARAMETER DATASET FOR dsFicha.
    DEFINE OUTPUT PARAMETER IdError    AS LOGICAL.
    DEFINE OUTPUT PARAMETER Respuesta  AS CHAR.
 
    For each FichaDep WHERE FichaDep.FecReg = l-Fecha NO-LOCK
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
            ttFichaDep.IdBanco = FichaDep.Id-Banco
            ttFichaDep.Banco   = IF AVAILABLE Banco THEN Banco.NomCto  ELSE ""
                  ttFichaDep.Tipo    =  FichaDep.Tipo WHEN FichaDep.Tipo <> 0 
                  ttFichaDep.Clave   =  FichaDep.Clave 
                  ttFichaDep.IdCta   =  FichaDep.Id-Cta WHEN FichaDep.Id-Cta <> 0
                  ttFichaDep.IdSCta  =  FichaDep.Id-SCta WHEN FichaDep.Id-Cta <> 0
                  ttFichaDep.IdSSCta =  FichaDep.Id-SSCta WHEN FichaDep.Id-Cta <> 0
                  ttFichaDep.IdSSSCta =  FichaDep.Id-SSSCta WHEN FichaDep.Id-Cta <> 0
                  ttFichaDep.Descr    =  IF AVAILABLE Cuenta THEN Cuenta.Descr ELSE ""
                  ttFichaDep.Importe  = FichaDep.Importe.                            
                                  
    END.


END PROCEDURE.

