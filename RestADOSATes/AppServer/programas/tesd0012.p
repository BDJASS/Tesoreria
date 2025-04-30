/*
    Empresa  :  ADOSA
    Programa :  tesd0012.p
    Funcion  :  Cancela una CFDI de complemento de pago
    Autor    :  FLC
    Fecha    :  13 ENE 2020
*/

DEF INPUT PARAMETER numCPago LIKE CPago.Id-CPago NO-UNDO.

/* Para manejo de conexion con web service */
DEF VAR hWS               AS HANDLE.
DEF VAR hEmiteCFDSoap     AS HANDLE.
DEF VAR vlconnect         AS LOGICAL NO-UNDO.
DEF VAR servidor          AS CHAR NO-UNDO.

/* Variables para funcion del web service */
DEF VAR v-respuesta   AS CHAR NO-UNDO.
DEF VAR v-serie       AS CHAR NO-UNDO.
DEF VAR v-folio       AS CHAR NO-UNDO.
DEF VAR v-estatus     AS CHAR NO-UNDO.

/* Variables para extraer los valores de la respuesta del WebService */
DEF VAR v-valores AS CHAR EXTENT 4 NO-UNDO.
DEF VAR v-tam     AS INT NO-UNDO.
DEF VAR v-ind     AS INT NO-UNDO.
DEF VAR v-pos     AS INT NO-UNDO.
DEF VAR v-siz     AS INT NO-UNDO.
DEF VAR v-num     AS INT NO-UNDO.

FIND CPago WHERE CPago.Id-CPago = numCPago NO-LOCK NO-ERROR.

IF AVAILABLE CPago THEN DO:

    v-estatus = ENTRY(1,CPago.Folioe).
    v-serie   = ENTRY(2,CPago.Folioe).
    v-folio   = ENTRY(3,CPago.Folioe).

END.
ELSE
    v-estatus = ''.

IF v-estatus = 'A' THEN DO:
    /* Asigna en que servidor se cancelara */
    servidor = "-WSDL http://192.0.1.7/wseDocEmiteCFD/wsEmiteCFD.asmx?wsdl". 
                               
    /* Realizar llamadas y operaciones con el webService de eDoc */
    CREATE SERVER hWS.
    vlconnect = hWS:CONNECT(servidor) NO-ERROR.
        
    IF vlconnect THEN DO:

        RUN wsEmiteCFDSoap SET hEmiteCFDSoap ON hws.
    
        RUN CancelaFactura IN hEmiteCFDSoap(INPUT 'AOF870529IU7', INPUT v-serie, INPUT v-folio, OUTPUT v-respuesta).    

        OUTPUT TO VALUE("/usr2/compartido/request/" + CPago.Id-CPago + ".canc").
        EXPORT v-respuesta.
        OUTPUT CLOSE.
    
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
    
        IF v-valores[1] = "ERROR" THEN DO:    
            BELL. BELL.
            MESSAGE "Ocurrio un error al intentar cancelar el CFDI CPago " + v-valores[3].
            PAUSE 2 NO-MESSAGE.
        END.            
        ELSE DO:
        
            DO TRANSACTION:
            
                FIND CPago WHERE CPago.Id-CPago = numCPago EXCLUSIVE-LOCK.
                ASSIGN CPago.Folioe = v-valores[1] + ',' + v-valores[3] + ',' + v-valores[4].
                ASSIGN CPago.FecCanc = TODAY
                       CPago.UsuarioCanc = USERID("dictdb").
                FIND Acuse WHERE Acuse.Id-Acuse = CPago.Id-Acuse
                     EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Acuse THEN 
                   ASSIGN Acuse.Id-CPago = "".
                RELEASE Acuse.
                RELEASE CPago.
            
            END.
            BELL. BELL.
            MESSAGE "El CFDI CPago se Cancelo con exito, Folio: " + v-valores[1] + "," + v-valores[2] + "," + v-valores[3] + "," + v-valores[4].            
            PAUSE 2 NO-MESSAGE.
        END.
    END.
    ELSE DO:
        BELL. BELL.
        MESSAGE "Ocurrio un error. No se puede conectar con el servicio...".
        PAUSE 2 NO-MESSAGE.
    END.        
END.
ELSE IF v-estatus = 'C' THEN DO:
        BELL. BELL.
        MESSAGE "El CFDI CPago ya esta cancelado".
        PAUSE 2 NO-MESSAGE.
     END.    
     ELSE DO:
        BELL. BELL.
        MESSAGE "El CFDI CPago no a sido generada".
        PAUSE 2 NO-MESSAGE.
     END.        
