@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : notacredito.p
    Purpose     : 

    Syntax      :

    Description :      

    Author(s)   : sis6
    Created     : Thu Apr 10 16:35:56 CST 2025
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

DEFINE TEMP-TABLE ttNCR
    FIELD IdNCR       AS CHARACTER   
    FIELD IdCliente   AS INTEGER
    FIELD RazonSocial AS CHARACTER
    FIELD FecReg      AS DATE
    FIELD Tipo        AS INTEGER
    FIELD CalleNo     AS CHARACTER
    FIELD Estatus     AS LOGICAL
    FIELD UsuarioCanc AS CHARACTER
    FIELD Colonia     AS CHARACTER
    FIELD CP          AS CHARACTER
    FIELD FecCanc     AS DATE
    FIELD UsuarioSol  AS CHARACTER
    FIELD Motivo      AS CHARACTER
    FIELD Ciudad      AS CHARACTER
    FIELD Concepto    AS CHARACTER
    FIELD Subtotal    AS DECIMAL
    FIELD IVA         AS DECIMAL
    FIELD Tot         AS DECIMAL
    FIELD oError      AS CHARACTER.
    
    
DEFINE TEMP-TABLE ttDetNCR
    FIELD IdNCR      AS CHARACTER
    FIELD Documento  AS CHARACTER
    FIELD Descr      AS CHARACTER
    FIELD Referencia AS CHARACTER
    FIELD TipoCR     AS INTEGER
    FIELD DescrMC    AS CHARACTER
    FIELD Importe    AS DECIMAL.
    
DEFINE DATASET dsNptasCR FOR ttNCR, ttDetNCR DATA-RELATION NotasCredito FOR ttNCR, ttDetNCR RELATION-FIELDS (IdNCR, IdNCR) NESTED.
    
DEFINE VAR l-user-cancela AS CHARACTER.
DEF    VAR numNCR         LIKE NCR.Id-NCR NO-UNDO.

/* Para manejo de conexion con web service */
DEF    VAR hWS            AS HANDLE.
DEF    VAR hEmiteCFDSoap  AS HANDLE.
DEF    VAR vlconnect      AS LOGICAL   NO-UNDO.
DEF    VAR servidor       AS CHAR      NO-UNDO.

/* Variables para funcion del web service */
DEF    VAR v-respuesta    AS CHAR      NO-UNDO.
DEF    VAR v-serie        AS CHAR      NO-UNDO.
DEF    VAR v-folio        AS CHAR      NO-UNDO.
DEF    VAR v-estatus      AS CHAR      NO-UNDO.

/* Variables para extraer los valores de la respuesta del WebService */
DEF    VAR v-valores      AS CHAR      EXTENT 4 NO-UNDO.
DEF    VAR v-tam          AS INT       NO-UNDO.
DEF    VAR v-ind          AS INT       NO-UNDO.
DEF    VAR v-pos          AS INT       NO-UNDO.
DEF    VAR v-siz          AS INT       NO-UNDO.
DEF    VAR v-num          AS INT       NO-UNDO.


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetNotaCredito:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER IdNCR AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcJson AS LONGCHAR NO-UNDO.

    CREATE ttNCR.
    FIND NCR WHERE NCR.Id-NCR = IdNCR NO-LOCK NO-ERROR.
    IF NOT AVAILABLE (NCR) THEN 
    DO:
        ASSIGN 
            ttNCR.oError = "No existe la Nota de Credito.".
            RELEASE ttNCR.
        DATASET dsNptasCR:WRITE-JSON("LONGCHAR", opcJson, TRUE).
        RETURN.
    END.
    FIND FIRST Empleado WHERE empleado.Iniciales = NCR.UsuarioCanc NO-LOCK NO-ERROR.
    IF NCR.FecCanc <> ? THEN 
    DO:
        
        ASSIGN 
            ttNCR.oError = "La nota de credito ya esta cancelada por " +
            empleado.Nombre + " el dia " + string(ncr.feccanc).
//        RELEASE ttNCR.
//        DATASET dsNptasCR:WRITE-JSON("LONGCHAR", opcJson, TRUE).
//        RETURN.
    END. 
    
           
    ASSIGN 
        ttNCR.Estatus = IF NCR.FecCanc = ? THEN FALSE ELSE TRUE.
    IF NCR.Tipo = 1 THEN 
    DO:
        FIND FIRST DetNCR OF NCR NO-LOCK NO-ERROR.
        FIND FIRST Devolucion WHERE Devolucion.Id-Dev = 
            INTE(DetNCR.Referencia) NO-LOCK NO-ERROR.
        ASSIGN 
            ttNCR.Concepto = "Devolucion de Ventas" + (IF AVAILABLE Devolucion THEN "   Dev: " + STRING(Devolucion.Id-Dev) ELSE '')
            ttNCR.oError   = "Al Cancelar, quedara viva la Devolucion # " + STRING(Devolucion.Id-Dev).
              
    END.
           
    IF NCR.Tipo = 2 THEN ASSIGN ttNCR.Concepto = "Descuentos y Bonificaciones".
    IF NCR.Tipo = 3 THEN ASSIGN ttNCR.Concepto = "Devoluciones de Efectivo".
    IF NCR.Tipo = 4 THEN 
    DO:
        FIND FIRST DetNcr OF Ncr NO-LOCK NO-ERROR.
        IF AVAILABLE DetNcr THEN 
            ASSIGN ttNCR.Concepto = IF Detncr.TipoCR = 66 THEN 
                                   'Diferencia en Precio' ELSE
                                   'Descuento Especial'.
    END.


    FIND Cliente WHERE Cliente.id-cliente = NCR.Id-Cliente NO-LOCK NO-ERROR.
    FIND Ciudad OF Cliente NO-LOCK NO-ERROR.
    IF AVAILABLE Ciudad THEN
        FIND Estado OF Ciudad NO-LOCK NO-ERROR.
       
    ASSIGN
        ttNCR.IdNCR       = NCR.Id-NCR
        ttNCR.IdCliente   = NCR.Id-Cliente
        ttNCR.FecReg      = NCR.FecReg
        ttNCR.RazonSocial = Cliente.RazonSocial
        ttNCR.Tipo        = NCR.Tipo
        ttNCR.CalleNo     = Cliente.CalleNo
        ttNCR.UsuarioCanc = NCR.UsuarioCanc
        ttNCR.Colonia     = Cliente.Colonia
        ttNCR.CP          = Cliente.CP
        ttNCR.FecCanc     = NCR.FecCanc
        ttNCR.UsuarioSol  = NCR.UsuarioSol
        ttNCR.Motivo      = NCR.Motivo
        ttNCR.Ciudad      = Ciudad.Nombre + ", " + Estado.Nombre
        ttNCR.Subtotal    = NCR.Subtotal
        ttNCR.IVA         = NCR.IVA
        ttNCR.Tot         = NCR.Tot.  
       
    
    FOR EACH DetNCR WHERE DetNCR.Id-NCR = NCR.Id-NCR NO-LOCK:
        CREATE ttDetNCR.
        ASSIGN
            ttDetNCR.Documento  = DetNCR.Documento
            ttDetNCR.IdNCR      = DetNCR.Id-NCR
            ttDetNCR.Referencia = DetNCR.Referencia
            ttDetNCR.TipoCR     = DetNCR.TipoCR
            ttDetNCR.Importe    = DetNCR.Importe.
            
        FIND TabMC OF DetNCR NO-LOCK NO-ERROR.
           
        IF AVAILABLE TabMC THEN ASSIGN ttDetNCR.Descr = TabMC.descr.
           
        FIND TabMC WHERE TabMC.id-MC = DetNCR.TipoCR NO-LOCK NO-ERROR.
           
        ASSIGN 
            ttDetNCR.DescrMC = TabMC.Descr.
        
    END.
    
    RELEASE ttDetNCR.
    RELEASE ttNCR.
    
    DATASET dsNptasCR:WRITE-JSON("LONGCHAR", opcJson, TRUE).
END PROCEDURE.

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE PostNotaCredito:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER IdNCR AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER IdUser AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER UsuarioSol AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER Motivo AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oError AS CHARACTER NO-UNDO.

    DO TRANSACTION:
        FIND adosa.URL WHERE adosa.URL.Parametro = "CancelaNCR" NO-LOCK NO-ERROR.
        IF AVAILABLE adosa.URL THEN ASSIGN l-user-cancela = adosa.URL.Valor.
        ELSE 
        DO:
            ASSIGN 
                oError = "No existe el parametro CancelaNCR, favor de hablar a sistemas".
            UNDO, RETURN.
        END.
   
        FIND NCR WHERE NCR.Id-NCR = IdNCR EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE NCR THEN 
        DO:
            IF NOT CAN-DO(l-user-cancela,IdUser) THEN 
            DO:
          //IF NOT CAN-DO("gee,elf,franc",USERID("dictdb")) THEN DO:
                ASSIGN 
                    oError = "Imposible cancelar. Usuario no autorizado. LLamar a Tesoreria".
                UNDO, RETURN.
            END.
            ASSIGN 
                NCR.feccanc     = TODAY
                NCR.usuariocanc = IdUser
                NCR.UsuarioSol  = UsuarioSol
                NCR.Motivo      = Motivo.
             
            numNCR = NCR.Id-NCR.
          
            FIND FIRST MovCaja WHERE MovCaja.Refer = NCR.Id-NCR
                AND MovCaja.TipoVenta = 9 EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE MovCaja THEN 
            DO:
                ASSIGN
                    MovCaja.Canc         = TRUE 
                    MovCaja.Usuario-Canc = IdUser.
                RELEASE MovCaja.
            END.
            ELSE 
            DO:
                FOR EACH DetNCR WHERE DetNCR.Id-NCR = NCR.Id-NCR NO-LOCK:
                    FOR EACH MovCliente WHERE MovCliente.Documento = NCR.Id-NCR EXCLUSIVE-LOCK:
                        DELETE MovCliente.
                    END. /* del movcliente */
                    FIND FIRST MovCliente WHERE MovCliente.RefSaldo = DetNCR.Documento 
                        AND MovCliente.Id-MC <= 3 NO-LOCK NO-ERROR.
                    /* {cxca0004.i &factura = NCR.Id-NCR} */
                    IF AVAILABLE MovCliente THEN 
                    DO:
                        FIND CURRENT MovCliente EXCLUSIVE-LOCK.
                        ASSIGN 
                            MovCliente.Saldo = MovCliente.Saldo + NCR.Tot.
                    END.
                END. /* del detNCR */
            END. /* del detNCR */

            IF NCR.Tipo = 1 THEN 
            DO:
                FIND FIRST DetNCR OF NCR NO-LOCK NO-ERROR.
                FIND FIRST Devolucion WHERE Devolucion.Id-Dev = 
                    INTE(DetNCR.referencia) EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Devolucion THEN 
                DO:
                    ASSIGN 
                        Devolucion.Id-NCR     = ""
                        Devolucion.Documento  = ""
                        Devolucion.UsuarioApl = ""
                        Devolucion.FecApl     = ?
                        Devolucion.Id-MC      = 0.
                END.
            END.
      
        END. /* si existe la NCR */
        FIND CURRENT NCR NO-LOCK NO-ERROR.
   //RELEASE NCR.
        RELEASE MovCliente.
        /* Cancela la nota de credito electronica */
   //FIND NCR WHERE NCR.Id-NCR = numNCR NO-LOCK NO-ERROR.
        IF NCR.Feccanc <> ? AND NCR.Folioe <> '' THEN 
        DO:
            //RUN ausc0072.p(INPUT NCR.Id-NCR).
            
            IF AVAILABLE NCR THEN 
            DO:

                v-estatus = ENTRY(1,NCR.Folioe).
                v-serie   = ENTRY(2,NCR.Folioe).
                v-folio   = ENTRY(3,NCR.Folioe).

            END.
            ELSE
                v-estatus = ''.

            IF v-estatus = 'A' THEN 
            DO:
                /* Asigna en que servidor se cancelara */
                IF v-serie = 'CHC' OR v-folio MATCHES "*J" THEN
                  //  servidor = "".
                    servidor = "-WSDL http://192.0.5.22/wseDocEmiteCFD/wsEmiteCFD.asmx?wsdl".
                ELSE 
                    IF (v-serie = 'SAC' OR v-folio BEGINS "Z" OR v-folio BEGINS "S") 
                        AND NCR.FecReg < 10/31/2016 THEN
                     //   servidor = "".
                        servidor = "-WSDL http://192.0.2.22/wseDocEmiteCFD/wsEmiteCFD.asmx?wsdl".
                    ELSE
                      //  servidor = "". 
                        servidor = "-WSDL http://192.0.1.7/wseDocEmiteCFD/wsEmiteCFD.asmx?wsdl". 
         
                               
                /* Realizar llamadas y operaciones con el webService de eDoc */
                CREATE SERVER hWS.
                vlconnect = hWS:CONNECT(servidor) NO-ERROR.
        
                IF vlconnect THEN 
                DO:

                    RUN wsEmiteCFDSoap SET hEmiteCFDSoap ON hws.
    
                    RUN CancelaFactura IN hEmiteCFDSoap(INPUT 'AOF870529IU7', INPUT v-serie, INPUT v-folio, OUTPUT v-respuesta).    
    
                    /* Separar los valores devueltos por el web service */
                    v-tam = LENGTH(v-respuesta, "CHARACTER").
                    v-ind = 1.
                    DO v-num = 1 TO 4:
        
                        v-pos = INDEX(v-respuesta,"_",v-ind).
                        v-siz = v-pos - v-ind.
            
                        IF v-pos = 0 THEN
                            v-siz = v-tam - v-ind + 1.
             
                        v-valores[v-num] = SUBSTRING(v-respuesta,v-ind,v-siz).
            
                        v-ind = v-pos + 1.
        
                    END.
    
                    IF v-valores[1] = "ERROR" THEN 
                    DO:    
                        //BELL. 
                        //BELL.
                        ASSIGN 
                            oError = "Ocurrio un error al intentar cancelar la nota de credito electronica " + v-valores[3].
                        //PAUSE 2 NO-MESSAGE.
                    END.
                    ELSE 
                    DO:
        
                        FIND NCR WHERE NCR.Id-NCR = numNCR EXCLUSIVE-LOCK.
                        ASSIGN 
                            NCR.Folioe = v-valores[1] + ',' + v-valores[3] + ',' + v-valores[4].
                        RELEASE NCR.
            
                        //BELL. 
                        //BELL.
                        ASSIGN 
                            oError = "La Nota de Credito Electronica se Cancelo con exito, Folio: " + v-valores[1] + "," + v-valores[2] + "," + v-valores[3] + "," + v-valores[4].
                        //PAUSE 2 NO-MESSAGE.
        
                    END.
            
                END.
                ELSE 
                DO:
                    //BELL. 
                    //BELL.
                    ASSIGN 
                         oError = "Ocurrio un error. No se puede conectar con el servicio eDoc...Favor de comunicarse a sistemas.".
                    //PAUSE 2 NO-MESSAGE.
                END.        
            END.
            ELSE IF v-estatus = 'C' THEN 
                DO:
                    //BELL. 
                    //BELL.
                    ASSIGN 
                        oError = "La nota de credito ya esta cancelada".
                    //PAUSE 2 NO-MESSAGE.
                END.        
                ELSE 
                DO:
                    //BELL. 
                    //BELL.
                    ASSIGN 
                        oError = "La nota de credito electronica no a sido generada".
                    //PAUSE 2 NO-MESSAGE.
                END.
        END.  
    END.

END PROCEDURE.

