
/*------------------------------------------------------------------------
    File        : complepago.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : sis6
    Created     : Fri May 09 17:30:23 CST 2025
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

DEFINE TEMP-TABLE ttCompPago
    FIELD IdAcuse     AS CHARACTER
    FIELD FecReg      AS DATE
    FIELD FecDep      AS DATE
    FIELD FecPago     AS DATE
    FIELD IdCliente   AS INTEGER
    FIELD RazonSocial AS CHARACTER    
    FIELD Estatus     AS INTEGER
    FIELD Observ      AS CHARACTER
    FIELD FormaPago   AS CHARACTER.
    
DEFINE VARIABLE l-SuperUser  AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-hora       AS CHAR FORMAT "x(8)".

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

PROCEDURE GetComplePago:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER Acuse AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER IdUser AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oError AS CHARACTER NO-UNDO.

FIND adosa.URL WHERE adosa.URL.Parametro = "CompPagoCyC" NO-LOCK NO-ERROR.
IF AVAILABLE adosa.URL THEN ASSIGN l-SuperUser = adosa.URL.Valor.


IF LOOKUP(IdUser,l-SuperUser) = 0 THEN DO:
    ASSIGN oError = "Usuario No Permitido".    
    RETURN.
END.

FIND Acuse WHERE Acuse.Id-Acuse = Acuse NO-LOCK NO-ERROR.

IF NOT AVAILABLE Acuse THEN DO:                  
                  ASSIGN oError = "El Acuse no existe.".
                  RETURN.
               END. /* end del not available   */
               IF Acuse.Estatus <> 4 THEN DO:
                  
                  ASSIGN oError = "El Acuse no esta depositado...".
                  RETURN.
               END. /* end del not available   */
               IF Acuse.Tipo <> "N" THEN DO:                  
                  ASSIGN oError = "Solo se pueden corregir acuses NORMALES, no ANTICIPOS...".
                  RETURN.
               END. /* end del not available   */
               FIND FIRST CPago WHERE CPago.Id-Acuse = Acuse.Id-Acuse
                                  AND CPago.FecCanc = ?
                                  NO-LOCK NO-ERROR.
               IF NOT AVAILABLE CPago THEN DO:
                  
                  ASSIGN oError = "El Acuse no tiene complemento de pago ...".
                  RETURN.
               END. /* end del not available   */
               IF CPago.FecReg < TODAY - 90 THEN DO:
                  
                  MESSAGE "El complemento de pago es muy antiguo ...".
                  IF IdUser <> "NCR" AND IdUser <> "RGP" THEN DO:
                     RETURN.
                  END.
               END.
               
               
               FIND FIRST Acuse WHERE Acuse.Id-Acuse = Acuse NO-LOCK NO-ERROR.
               
               IF AVAILABLE Acuse THEN DO:
                   ASSIGN l-hora = (IF LENGTH(Acuse.Comen[3]) >= 40 THEN 
                          SUBSTRING(Acuse.Comen[3],42,50) 
                       ELSE "SIN IMPR").
                       
                       ASSIGN ttCompPago.IdAcuse = Acuse.Id-Acuse
                              ttCompPago.IdCliente = Acuse.Id-Cliente
                              ttCompPago.FecReg = Acuse.FecReg
                              ttCompPago.RazonSocial = CPago.RazonSocial
                              ttCompPago.Observ = Acuse.Comen[1] + " " + Acuse.Comen[2] + " " + Acuse.Comen[3]
                              ttCompPago.Estatus = Acuse.Estatus
                              ttCompPago.FecDep = Acuse.FecDep
                              ttCompPago.FecPago = CPago.FecPag
                              ttCompPago.FormaPago = CPago.FormaDePago.
                   RELEASE ttCompPago.
               END.
END PROCEDURE.

