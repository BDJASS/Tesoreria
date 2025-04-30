@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*------------------------------------------------------------------------
    File        : teserp003.p
    Purpose     : HU02   /CancelaFact  POST
                  Cancelación de Ventas [Factura]
                  COMO esponsable de tesoreria QUIERO poder capturar una factura 
                  y visualizar  la información de -[datos del cliente, 
                  articulos vendidos y totales] 
                  PARA poder cancelarla si asi es requerido o a solicitud.
    Author(s)   : sis10
    Created     : Fecha actual   
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW. /* Manejo de errores global */

DEFINE NEW SHARED VARIABLE g-Origen AS CHARACTER NO-UNDO INITIAL "02B".
/* **********************  Internal Procedures  *********************** */
DEFINE VARIABLE l-titulo AS CHARACTER.
    DEFINE BUFFER g-Folio      FOR Folio.
    DEFINE BUFFER b-movcaja    FOR MovCaja.
    DEFINE BUFFER bfDevolucion FOR Devolucion.
    DEFINE BUFFER bfNCR        FOR NCR.
    DEFINE BUFFER bfMovCaja    FOR MovCaja.
    DEFINE BUFFER bfEstPedido  FOR EstPedido.
    DEFINE VARIABLE l-genmov    AS LOGICAL.
    DEFINE VARIABLE l-consig    AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE l-recdev    AS RECID     NO-UNDO.
    DEFINE VARIABLE l-rec       AS RECID     NO-UNDO.
    DEFINE VARIABLE l-usuario   LIKE Password.Usuario.
    DEFINE VARIABLE l-saldo     LIKE MovCliente.Saldo.
    DEFINE VARIABLE l-acuse     LIKE Acuse.id-Acuse.
    DEFINE VARIABLE l-cantcom   LIKE DetRemis.Cant.
    DEFINE VARIABLE cp-question AS CHARACTER.
    DEFINE VARIABLE cp-answer   AS LOGICAL.
    DEFINE VARIABLE l-cant      LIKE DetRemis.Cant.
    DEF    VAR      l-fecvence  AS DATE.
    DEF    VAR      l-ubic      AS CHAR .
    DEF    VAR      l-recmov    AS RECID.
    DEF    VAR      l-resp      AS LOGICAL.

/* ***************************  Main Procedure *************************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE PostCancela:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    /*
        Empresa: Abastecedora de Oficinas
        Sistema: Adosa
        Modulo : TES
        
        Programa: tesa0581.p
        Funcion : Rutina de cancelacion de ventas de contado y de credito
        Usado por: tesa0581.i
        
        Autor:    MGGP
        Fecha:    23-Agosto-96   
    */     

    DEFINE INPUT PARAMETER l-tipo AS INTEGER.   
    DEFINE INPUT PARAMETER l-factura LIKE Factura.Id-Factura.
    DEFINE INPUT  PARAMETER pConfirmar AS LOGICAL NO-UNDO INITIAL FALSE.
    DEFINE INPUT  PARAMETER pIdUser    AS CHARACTER NO-UNDO.  
    DEFINE INPUT  PARAMETER pMotivo    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER pIdUserSol AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER IdError    AS LOGICAL.
    DEFINE OUTPUT PARAMETER Respuesta  AS CHAR.

    



    ASSIGN 
        l-acuse     = ''
        cp-question = "Acepta la cancelacion de la factura"
        cp-answer   = TRUE   
        l-resp      = FALSE . // lo puse default false ya que en el get siempre esta en false
             
    IF pConfirmar = ? THEN pConfirmar = FALSE.
    IF pMotivo = ? THEN pMotivo = "".
    IF pIdUserSol = ? THEN pIdUserSol = "".
    
    IF l-tipo <> 1 AND l-tipo <> 2 AND l-tipo <> 3 THEN DO:
        ASSIGN Respuesta ="Tipo de Documento No Valido"
               IdError   = TRUE.
        RETURN.
    END.    
    
    IF pMotivo = "" THEN DO:
        ASSIGN 
            Respuesta = "Se debe enviar Motivo de Cancelacion."
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
    
    FIND Usuario WHERE Usuario.Id-User = pIdUser NO-LOCK NO-ERROR.
    IF AVAILABLE Usuario AND Usuario.id-ubicacion <> "" THEN 
    DO:
        ASSIGN  
            g-Origen = Usuario.id-ubicacion.
    END.
    ELSE 
    DO:
        ASSIGN 
            Respuesta = "El usuario especificado " + pIdUser + " no cuenta con Id-Ubicacion"
            IdError   = TRUE.
        RETURN.
    END.
    
    IF cp-answer THEN 
    DO:
        FIND FIRST Devolucion WHERE Devolucion.Id-Factura = l-Factura
            AND Devolucion.TipoVenta = l-Tipo NO-LOCK NO-ERROR.

        FIND Remision WHERE Remision.Id-Remision = l-factura
            AND Remision.TipoVenta = l-Tipo NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Remision THEN 
            FIND Factura WHERE Factura.Id-Factura = l-Factura NO-LOCK NO-ERROR.
    
        IF ((AVAILABLE Remision AND Remision.Pedidos <> "") OR (AVAILABLE Factura AND Factura.Pedidos <> "")) AND
            (IF AVAILABLE Devolucion THEN Devolucion.VtaCanc = FALSE 
        ELSE TRUE) THEN 
        DO:
            IF NOT pConfirmar THEN 
            DO:
                ASSIGN 
                    Respuesta = "Los pedidos relacionados con esta factura quedaran activos, avisar a Materiales."
                    IdError   = FALSE.  /* MENSAJE PERO DEBE CONTINUAR EL PROCESO */
                RETURN.
            END.     
        END.
        

        STATUS DEFAULT "Cancelando ...".
        IF l-tipo <> 3 THEN 
        DO:
            proceso:
            DO TRANSACTION ON ENDKEY UNDO, LEAVE ON ERROR UNDO, LEAVE:
                /* Validar si fue cobrada la factura */
                ASSIGN 
                    l-genmov = TRUE.
                FIND FIRST Remision WHERE Remision.Id-Remision = l-Factura NO-LOCK NO-ERROR.

                /*ENE-2011 PARA QUE HABILITE LOS VALES CUANDO SE CANCELA LA VENTA*/
                FOR EACH Vale WHERE Vale.Id-FacApl = Remision.Id-Remision EXCLUSIVE-LOCK:
                    ASSIGN
                        Vale.FecApl    = ?
                        Vale.Id-FacApl = ''
                        Vale.Id-User   = ''.
                    
                    FOR EACH DetVale WHERE DetVale.Id-Vale = Vale.Id-Vale
                        AND DetVale.Id-Remision = Remision.Id-Remision EXCLUSIVE-LOCK:
                        ASSIGN 
                            Vale.MontoUsado = Vale.MontoUsado - DetVale.Importe.
                        DELETE DetVale.
                    END.
                END.

                FIND FIRST Pedido WHERE Pedido.Id-Factura = Remision.Id-Remision
                    AND CAN-DO(Remision.Pedidos, Pedido.Id-Pedido)
                    AND Pedido.Adelantado = TRUE NO-LOCK NO-ERROR.
                IF AVAILABLE Pedido THEN 
                    ASSIGN l-GenMov = FALSE.
                IF l-Resp THEN
                    ASSIGN l-GenMov = FALSE.
            
                IF AVAILABLE Devolucion AND  
                    Devolucion.VtaCanc AND
                    Devolucion.Feccanc = ? AND 
                    Devolucion.FecApl = ? THEN 
                DO:
                    ASSIGN 
                        l-recdev = RECID(Devolucion).
                    l-genmov = FALSE.
                    
                    FIND Devolucion WHERE RECID(Devolucion) = l-recdev EXCLUSIVE-LOCK NO-ERROR.
                    ASSIGN 
                        Devolucion.FecApl     = TODAY
                        Devolucion.UsuarioApl = CAPS(pIdUser)
                        Devolucion.Documento  = Devolucion.Id-Factura
                        Devolucion.Id-Mc      = 1.
                END.
                FIND FIRST MovCaja WHERE MovCaja.Referencia = l-factura
                    AND MovCaja.TipoVenta = l-tipo NO-LOCK NO-ERROR.
                IF AVAILABLE MovCaja THEN 
                DO:
                    ASSIGN 
                        l-rec = RECID(MovCaja).
                    
                    FIND MovCaja WHERE RECID(MovCaja) = l-rec EXCLUSIVE-LOCK NO-ERROR.
                
                    FIND CtlCaja WHERE CtlCaja.Id-Caja = MovCaja.Id-Caja
                        AND CtlCaja.Turno = MovCaja.Turno
                        AND CtlCaja.FecOper = MovCaja.FecOper NO-LOCK NO-ERROR.
                               
                    IF AVAILABLE CtlCaja AND CtlCaja.FecCierre <> ? THEN 
                    DO:
                    
                        FOR EACH DetMovC WHERE DetMovC.Id-Caja = MovCaja.Id-Caja
                            AND DetMovC.Folio = MovCaja.Folio
                            NO-LOCK BREAK BY DetMovC.Id-TP ON ERROR UNDO Proceso, LEAVE Proceso ON ENDKEY UNDO Proceso, LEAVE Proceso:
                                        
                            IF DetMovC.Mov <> 'P' THEN 
                                NEXT.
                            IF MovCaja.FecDep > MovCaja.FecReg THEN
                                ACCUMULATE DetMovC.MontoPago (TOTAL BY DetMovC.Id-TP).
                            ELSE
                                ACCUMULATE DetMovC.MontoPago * 1 (TOTAL BY DetMovC.Id-TP).
                            IF LAST-OF(DetMovC.Id-TP) THEN 
                            DO:
                                FIND FIRST CorteCaja WHERE CorteCaja.Id-Caja = CtlCaja.Id-Caja
                                    AND CorteCaja.Turno = CtlCaja.Turno
                                    AND CorteCaja.FecOper = CtlCaja.FecOper
                                    AND CorteCaja.Id-Tp = DetMovC.Id-Tp NO-LOCK NO-ERROR.
                                IF AVAILABLE (CorteCaja) THEN 
                                DO:
                                    ASSIGN 
                                        l-rec = RECID(CorteCaja).
                                    FIND CorteCaja WHERE RECID(CorteCaja) = l-rec EXCLUSIVE-LOCK NO-ERROR.
                                END.
                                ELSE 
                                DO:
                                    CREATE CorteCaja.
                                    ASSIGN 
                                        CorteCaja.Id-Caja = CtlCaja.Id-Caja
                                        CorteCaja.Turno   = CtlCaja.Turno
                                        CorteCaja.FecOper = CtlCaja.FecOper
                                        CorteCaja.Id-Tp   = DetMovC.Id-Tp.
                                END.
                                ASSIGN 
                                    CorteCaja.TotPagoP = CorteCaja.TotPagoP - (ACCUM TOTAL BY DetMovC.Id-TP DetMovC.MontoPago )
                                    CorteCaja.TotPagoN = CorteCaja.TotPagoN - (ACCUM TOTAL BY DetMovC.Id-TP DetMovC.MontoPago * 1).
                            END.  /* last-of de detmovc  */
                        END.   /* for detmovc  */
                    END. /* if available ctlcaja */

                    /* Borra los cheques postfechados */
                    FOR EACH ChequePF WHERE ChequePF.Id-Caja = MovCaja.Id-Caja
                        AND ChequePF.Folio = MovCaja.Folio
                        EXCLUSIVE-LOCK ON ERROR UNDO Proceso, LEAVE Proceso ON ENDKEY UNDO Proceso, LEAVE Proceso :
                        DELETE ChequePF.
                    END.
                END.
                FIND Remision WHERE Remision.Id-Remision = l-factura EXCLUSIVE-LOCK NO-ERROR.
                IF Remision.FecCancel <> ? THEN 
                DO: 
                    ASSIGN 
                        Respuesta = 'La Remision ya fue cancelada por otro usuario. '
                        IdError   = TRUE.
                    RETURN.
                END.
                IF Remision.Amonedero > 0 THEN 
                DO:
                    FIND b-MovCaja WHERE b-MovCaja.Referencia = Remision.Id-Remision
                        AND b-MovCaja.FecReg = Remision.FecReg NO-LOCK NO-ERROR.
                    IF AVAILABLE b-MovCaja THEN 
                        FIND DetMovMon WHERE DetMovMon.Folio = b-MovCaja.folio
                            AND DetMovMon.Id-caja = b-MovCaja.Id-caja
                            AND DetMovMon.BRespCode = 0 NO-LOCK NO-ERROR.
                    IF AVAILABLE DetMovMon THEN 
                    DO:
                        ASSIGN 
                            Respuesta = 'La Remision no puede cancelarse, bonifico a monedero CFE.'
                            IdError   = TRUE.
                        RETURN.
                    END.                        
                END.
                IF Remision.Facglobal = '' AND AVAILABLE MovCaja THEN
                    ASSIGN 
                        MovCaja.Canc         = TRUE
                        MovCaja.Usuario-Canc = pIdUser.
                FIND FIRST DetRemis OF Remision WHERE DetRemis.CantDev > 0 EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE DetRemis AND l-GenMov THEN 
                DO:
                    ASSIGN 
                        Respuesta = "La Remision no puede cancelarse. " + "Hay Devoluciones en Factura."
                        IdError   = TRUE.
                    RETURN.
                END.
            /* Se cancela la venta de contado */
                {cxca0006.i
                &Cliente = Remision.Id-Cliente
                &Importe = "(Remision.Tot * -1)"
                &Subtotal = "(Remision.Subtotal * -1)"
                &renglon = 1
                &fecha = TODAY }
            IF Remision.Pagada THEN DO:  /* Se elimina el Pago de Contado */
                {cxca0006.i
                    &Cliente = Remision.Id-Cliente
                    &Importe = "(Remision.Tot * -1)"
                    &renglon = 3
                    &fecha = TODAY }
        END.
        IF Remision.Id-Vendedor <> '' THEN 
        DO:
        {vtaa0005.i
                    &Vendedor = Remision.Id-Vendedor
                    &Iniciales = Remision.Iniciales
                    &Fecha = TODAY
                    &Importe = "(Remision.Subtotal * -1)"
                    &Tipo = 1 }
        END.
        ASSIGN 
            Remision.FecCancel   = TODAY
            Remision.UsuarioCanc = CAPS(pIdUser). /*l-usuario*/
                
        /* Pago con Devolucon, reactiva devolucion */
        FIND FIRST MovCaja WHERE MovCaja.Referencia = l-factura
            AND MovCaja.TipoVenta = l-tipo NO-LOCK NO-ERROR.
        IF AVAILABLE MovCaja THEN 
        DO:
            FOR EACH DetMovC WHERE DetMovC.Id-Caja = MovCaja.Id-Caja
                AND DetMovC.Folio = MovCaja.Folio
                AND DetMovC.Mov = 'D'
                NO-LOCK BREAK BY DetMovC.Id-TP ON ERROR UNDO Proceso, LEAVE Proceso ON ENDKEY UNDO Proceso, LEAVE Proceso:
                                     
                FIND FIRST bfDevolucion WHERE bfDevolucion.Id-Dev = DetMovC.Id-Dev
                    AND bfDevolucion.Documento = l-Factura EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE bfDevolucion THEN 
                DO:
                    ASSIGN 
                        bfDevolucion.Documento  = ""
                        bfDevolucion.FecApl     = ?
                        bfDevolucion.UsuarioApl = "".
                            
                            
                    /* Para reastrear el motivo del por que no se cancela la NCR al cancelar una venta con devolucion aplicada */
                    IF TODAY <= 04/30/2022 THEN 
                    DO:
                        OUTPUT TO /usr2/sis7/tmp/DevSinNCR.lst APPEND.
                        EXPORT bfDevolucion.
                        OUTPUT CLOSE.
                    END.
                    /**/
                            
                    IF bfDevolucion.Id-NCR <> "" THEN 
                    DO:
                        FOR EACH bfNCR WHERE bfNCR.Id-NCR = bfDevolucion.Id-NCR
                            AND bfNCR.FecReg = bfDevolucion.FecReg EXCLUSIVE-LOCK:
                            ASSIGN
                                bfNCR.FecCanc     = TODAY
                                bfNCR.UsuarioCanc = pIdUser.
                                  
                            FIND FIRST bfMovCaja WHERE bfMovCaja.Refer = bfNCR.Id-NCR
                                AND bfMovCaja.TipoVenta = 9 EXCLUSIVE-LOCK NO-ERROR.
                            IF AVAILABLE bfMovCaja THEN 
                            DO:
                                ASSIGN
                                    bfMovCaja.Canc         = TRUE 
                                    bfMovCaja.Usuario-Canc = pIdUser.
                                RELEASE bfMovCaja.
                            END.
                            
                            /* Cancela electronicamente la NCR */
                            IF bfNCR.Feccanc <> ? AND bfNCR.Folioe <> '' THEN
                                RUN ausc0072.p(INPUT bfNCR.Id-NCR).
                            
                        END.
                    END.
                END.
            END.
            RELEASE bfDevolucion.
            RELEASE bfNCR.
            RELEASE bfMovCaja.
        END.
        /**/    
                
        FOR EACH Detremis OF Remision EXCLUSIVE-LOCK ON ERROR UNDO Proceso, LEAVE Proceso ON ENDKEY UNDO Proceso , LEAVE Proceso:
            IF DetRemis.Tipo = 6 THEN 
            DO:
                FIND Devolucion WHERE Devolucion.Id-Dev = INTE(SUBSTRING(DetRemis.Descr,14,6)) EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Devolucion AND Devolucion.FecCanc = ? THEN 
                DO:
                    ASSIGN 
                        Devolucion.FecApl     = ?
                        Devolucion.UsuarioApl = ''
                        Devolucion.Documento  = ''.
                END.
            END.
            ASSIGN 
                Detremis.CantDev = DetRemis.Cant.

            IF DetRemis.Tipo <> 1 THEN 
                NEXT.
            FIND ArtPres OF DetRemis NO-LOCK NO-ERROR.
            IF Remision.Pedidos BEGINS "Prestamo-" AND DetRemis.RengPed <> 0 THEN 
            DO:
                ASSIGN 
                    l-cant = DetRemis.Cant.
                FIND FIRST DetSPrest WHERE DetSPrest.Id-SPrest = SUBSTRING(Remision.Pedidos,10,7)
                    AND DetSPrest.Id-Articulo = DetRemis.Id-Articulo
                    AND DetSPrest.Id-Color = DetRemis.Id-Color
                    AND DetSPrest.Id-Pres = DetRemis.Id-Pres
                    AND DetSPrest.Sec = DetRemis.RengPed EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE DetSPrest THEN 
                    ASSIGN 
                        DetSPrest.Id-Factura = ''
                        DetSPrest.Concil     = ?
                        DetSPrest.Id-User    = ''
                        DetSPrest.ConcilUMI  = DetSPrest.ConcilUMI - (DetRemis.Cant * (IF AVAILABLE ArtPres THEN ArtPres.Equiv ELSE 1)).
                RELEASE DetSPrest.
            END.
            ELSE 
            DO:
                IF (DetRemis.Id-pedido = '' OR DetRemis.Origen = 2) AND l-genmov THEN 
                DO:
                    /* Genera Movimiento de Entrada */
                    /*
                    {inva0001.i 
                        &Tipomov = "'EDev'"
                        &Buffer = TRUE
                        &Refer = "DetRemis.Id-Remision"
                        &Cantidad = "(DetRemis.Cant * ArtPres.Equiv)"
                        &Almacen = "DetRemis.Id-Alm"
                        &llaveArt = "DetRemis.Id-Art"
                        &IdPres = DetRemis.id-Pres
                        &CantPres = DetRemis.Cant
                        &MaxMin = TRUE
                        &llaveColor = "DetRemis.Id-Color" } */
                    IF DetRemis.Id-Pedido <> "" THEN 
                    DO:
                        ASSIGN 
                            l-cant = DetRemis.Cant.
                        FIND FIRST DetPedido WHERE DetPedido.Id-pedido = DetRemis.Id-Pedido
                            AND DetPedido.Resto = DetRemis.Resto
                            AND Detpedido.Id-articulo = DetRemis.id-articulo
                            AND DetPedido.Id-color = DetRemis.Id-color
                            AND DetPedido.Id-pres = DetRemis.Id-pres
                            AND DetPedido.Reng = DetRemis.RengPed
                            EXCLUSIVE-LOCK NO-ERROR.
                        IF AVAILABLE DetPedido THEN 
                        DO:
                            IF DetPedido.CantEnt >= l-cant THEN
                                ASSIGN 
                                    DetPedido.CantEnt = DetPedido.CantEnt - l-cant
                                    l-cant            = 0.
                            ELSE
                                ASSIGN 
                                    l-cant            = l-cant - DetPedido.CantEnt
                                    DetPedido.CantEnt = 0.
                        END.
                    END.
                END.

                /* Afectar la cant facturada del pedido */
                IF DetRemis.Id-Pedido <> "" THEN 
                DO:
                    ASSIGN 
                        l-cant = DetRemis.Cant.
                    FIND FIRST DetPedido WHERE DetPedido.Id-pedido = DetRemis.Id-Pedido
                        AND DetPedido.Resto = DetRemis.Resto
                        AND Detpedido.Id-articulo = DetRemis.id-articulo
                        AND DetPedido.Id-color = DetRemis.Id-color
                        AND DetPedido.Id-pres = DetRemis.Id-pres
                        AND DetPedido.Reng = DetRemis.RengPed EXCLUSIVE-LOCK NO-ERROR.
                    IF AVAILABLE DetPedido THEN 
                    DO:
                        IF DetPedido.Cantfac >= l-cant THEN
                            ASSIGN 
                                DetPedido.CantFac = DetPedido.CantFac - l-cant
                                l-cant            = 0.
                        ELSE
                            ASSIGN 
                                l-cant            = l-cant - DetPedido.CantFac
                                DetPedido.CantFac = 0.
                    END.
                END.
            END.
        END.
    
        /* Afecta pedidos */
        IF NOT AVAILABLE Devolucion OR (AVAILABLE Devolucion AND Devolucion.VtaCanc = FALSE) THEN 
        DO:
            RUN /usr2/adosa/procs/embd0020.p(INPUT 2, INPUT 'Remision', INPUT Remision.Id-Remision).
        END.
                        
    END. /* Fin de transaccion */
        
    RELEASE Remision.         
    RELEASE DetRemis.
    RELEASE Movim.
    RELEASE ArtUbic.        
    RELEASE Folio.        
    RELEASE EstCte.
    RELEASE EstVendedor.
    RELEASE MovCaja.
    RELEASE Devolucion.
    RELEASE CorteCaja.
    RELEASE DetSPrest.
    RELEASE Pedido.
    RELEASE DetPedido.
        
        
    /* Cancela el folioe electronica de la Remision */
    FIND Remision WHERE Remision.Id-Remision = l-factura NO-LOCK NO-ERROR.
    IF AVAILABLE Remision AND Remision.FecCancel <> ? AND Remision.Folioe <> ''  THEN 
    DO:
        RUN /usr2/adosa/procs/vtac2072.p(INPUT Remision.Id-Remision).  /* PROGRAMA FUNCIONA 
                                                                          SOLO EN PROD POR WEBSERVICE */
    END.
        
    BELL.
    MESSAGE 'FACTURA CANCELADA.'.
    PAUSE 2 NO-MESSAGE.
END. /* if l-tipo <> 3 */
    ELSE DO:
Proc:
DO TRANSACTION ON ENDKEY UNDO,LEAVE ON ERROR UNDO,LEAVE:
    ASSIGN 
        l-genmov = TRUE.
    FIND Factura WHERE Factura.Id-Factura = l-Factura NO-LOCK NO-ERROR.
    FIND FIRST Pedido WHERE Pedido.Id-Factura = l-Factura
        AND CAN-DO(Factura.Pedidos, Pedido.Id-Pedido)
        AND Pedido.Adelantado = TRUE NO-LOCK NO-ERROR. 
    IF AVAILABLE Pedido THEN 
        ASSIGN l-GenMov = FALSE.
    IF l-Resp THEN l-GenMov = FALSE.

    FIND FIRST Devolucion WHERE Devolucion.Id-Factura = l-factura
        AND Devolucion.TipoVenta = l-tipo NO-LOCK NO-ERROR.

    IF AVAILABLE Devolucion AND Devolucion.VtaCanc
        AND Devolucion.Feccanc = ?
        AND Devolucion.FecApl = ? THEN 
    DO:
        ASSIGN 
            l-recdev = RECID(Devolucion).
        l-genmov = FALSE.
        FIND Devolucion WHERE RECID(Devolucion) = l-recdev EXCLUSIVE-LOCK NO-ERROR.
        ASSIGN 
            Devolucion.FecApl     = TODAY
            Devolucion.UsuarioApl = CAPS(pIdUser)
            Devolucion.Documento  = Devolucion.Id-Factura
            Devolucion.Id-Mc      = 1.
    END.
    FIND FIRST MovCaja WHERE MovCaja.Referencia = l-factura
        AND MovCaja.TipoVenta = 3 NO-LOCK NO-ERROR.
    IF AVAILABLE MovCaja THEN 
    DO:
        ASSIGN 
            l-rec = RECID(MovCaja).
        FIND MovCaja WHERE RECID(MovCaja) = l-rec EXCLUSIVE-LOCK NO-ERROR.
        ASSIGN 
            MovCaja.Canc         = TRUE
            MovCaja.Usuario-Canc = pIdUser.
    END.
    FIND Factura WHERE Factura.Id-Factura = l-factura EXCLUSIVE-LOCK NO-ERROR.
    FIND FIRST DetFactura WHERE DetFactura.Descr BEGINS "SALIDAS POR CONS"
        AND DetFactura.Id-Factura = l-factura NO-LOCK NO-ERROR.
    IF NOT AVAILABLE DetFactura THEN 
        FIND FIRST DetFactura WHERE DetFactura.Descr BEGINS "SALIDA POR CONS"
            AND DetFactura.Id-Factura = l-factura NO-LOCK NO-ERROR.
    IF AVAILABLE DetFactura THEN 
        ASSIGN 
            l-GenMov = FALSE
            l-Consig = TRUE.
    IF Factura.FecCancel <> ? THEN 
    DO:
        ASSIGN 
        Respuesta = 'La Factura ya fue cancelada por otro usuario. '
        IdError =   TRUE.
        RETURN.
    END.
    FIND FIRST DetFactura OF Factura WHERE DetFactura.CantDev > 0 EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE DetFactura AND l-GenMov THEN 
    DO:
        ASSIGN
        Respuesta ="La Factura no puede cancelarse. " + "Hay Devoluciones de Articulos."
        IdError =   TRUE.
        RETURN.
    END.

    {cxca0004.i &Factura = Factura.Id-Factura }

    IF NOT AVAILABLE (MovCliente) THEN 
    DO:
        ASSIGN
        Respuesta = "No existe este Documento en cartera."
        IdError =   TRUE.
        RETURN.
    END.

    ASSIGN 
        l-saldo = MovCliente.Saldo.

    FOR EACH MovCliente WHERE MovCliente.Refsaldo = Factura.Id-factura
        AND MovCliente.Afectado = FALSE NO-LOCK:
        ASSIGN 
            l-saldo = l-saldo + MovCliente.Importe.
    END.

            {cxca0004.i &Factura = Factura.Id-Factura }

    IF l-saldo - MovCliente.Importe <> 0 THEN 
    DO:
        ASSIGN
        RESPUESTA = "No se permite cancelar la factura si se han recibido abonos."
        IdError =   TRUE.
        RETURN.
    END.

    /* Solo si se esta cancelando una Factura Global */
    IF Factura.Id-Cliente <= 10 AND Factura.Id-Cliente <> 3 THEN 
    DO:
        FOR EACH Remision WHERE Remision.FacGlobal = Factura.Id-Factura EXCLUSIVE-LOCK 
            ON ERROR UNDO Proc, LEAVE Proc
            ON ENDKEY UNDO Proc, LEAVE Proc:
                    
            ASSIGN 
                Remision.FacGlobal = "".
        END.
                
        FOR EACH NCR WHERE NCR.FacGlobal = Factura.Id-Factura EXCLUSIVE-LOCK 
            ON ERROR UNDO Proc, LEAVE Proc
            ON ENDKEY UNDO Proc, LEAVE Proc:
            ASSIGN 
                NCR.FacGlobal = "".
        END.
    END.

    /* Si estoy cancelando una factura de consignacion */
    IF l-Consig THEN 
    DO:
        FOR EACH sUsoInt WHERE sUsoInt.Id-Factura = l-Factura EXCLUSIVE-LOCK:
            ASSIGN 
                sUsoInt.Id-Factura = "".
        END.
    END.
            
    /* Fin de cancelacion de factura de consignacion */
            
    IF (MONTH(Factura.FecReg) = MONTH(TODAY) AND YEAR(Factura.FecReg) = YEAR(TODAY)) OR NOT l-resp THEN           
        DELETE MovCliente.
            
            /*
            IF (MONTH(Factura.FecReg) <> MONTH(TODAY) OR YEAR(Factura.FecReg) <> YEAR(TODAY)) AND l-resp THEN DO:
                /* Generara Acuse */
                FIND Cobrador WHERE Cobrador.Id-Cobrador = 25 
                    NO-LOCK NO-ERROR.
                FIND Folio WHERE Folio.Id-Doc = ('ACUSEN' + IF AVAILABLE Cobrador AND Cobrador.Iniciales <> ""
                                                            THEN Caja.ConCob ELSE Caja.SinCob) EXCLUSIVE-LOCK NO-ERROR.
                CREATE Acuse.
                ASSIGN 
                    Acuse.Id-Acuse = (STRING(Folio.Folio,'999999') + IF Cobrador.Iniciales <> "" THEN Caja.ConCob ELSE Caja.SinCob)
                    Acuse.Comen[1] = "CANCELACION DE FACTURA " + Factura.Id-Factura
                    Acuse.Estatus = 2
                    Acuse.FecDep = TODAY
                    Acuse.FecOper = CtlCaja.FecOper
                    Acuse.FecReg = TODAY
                    Acuse.Id-Caja = CtlCaja.Id-Caja
                    Acuse.Id-Cliente = Factura.Id-Cliente
                    Acuse.Id-Cobrador = 25
                    Acuse.Iniciales = Empleado.Iniciales
                    Acuse.Id-Cajero = Usuario.Id-Cajero
                    Acuse.Tipo = 'N'
                    Acuse.Turno = CtlCaja.Turno
                    Acuse.UsuarioReg = pIdUser
                    Folio.Folio = Folio.Folio + 1
                    l-acuse = Acuse.Id-Acuse.

                CREATE DocAcuse.
                ASSIGN 
                    DocAcuse.Id-Acuse = Acuse.Id-Acuse
                    DocAcuse.Documento = Factura.Id-Factura
                    DocAcuse.Id-mc = 1
                    DocAcuse.FecDoc = Factura.Fecreg
                    DocAcuse.Sec = 1
                    DocAcuse.ImpDescAdc = l-saldo
                    DocAcuse.Tipo-adc = 67.
                {cxca0001.i
                    &TipoMov = 67
                    &Buffer = TRUE
                    &TipoPadre = 1
                    &FecReg = TODAY
                    &Documento = Acuse.Id-Acuse
                    &refsaldo = Factura.id-Factura
                    &Importe = "- l-saldo "
                    &Afectar = TRUE
                    &Cliente = Factura.id-cliente
                    &Ubic = Factura.Id-Ubic }
                    
            END. /* Si la Factura es de otro Mes */
            */

            /* Se cancela la venta de credito */
        {cxca0006.i
                &Cliente = Factura.Id-Cliente
                &Importe = "(l-saldo * -1) "
                &renglon = 2
                &Subtotal = "(Factura.Subtotal * -1)"
                &fecha = TODAY }

            IF Factura.Id-Cliente > 10 OR Factura.Id-Cliente = 3 THEN DO:
        {vtaa0005.i
                    &Vendedor = Factura.Id-Vendedor
                    &Iniciales = Factura.Iniciales
                    &Fecha = TODAY
                    &Importe = "(Factura.Subtotal * -1)"
                    &Tipo = 2 } 
END.

FOR EACH Vale WHERE Vale.id-factura = Factura.Id-Factura EXCLUSIVE-LOCK:
    ASSIGN
        Vale.FecApl    = TODAY
        Vale.Id-FacApl = Vale.Id-Factura
        Vale.Id-User   = pIdUser.
END.
            
ASSIGN 
    Factura.FecCancel   = TODAY
    Factura.UsuarioCanc = CAPS(pIdUser)
    Factura.UsuarioSol  = CAPS(pIdUserSol)
    Factura.Motivo      = pMotivo.   

IF NOT Factura.Especial AND NOT l-resp THEN 
DO:
    FOR EACH DetFactura OF Factura EXCLUSIVE-LOCK ON ERROR UNDO Proc, LEAVE Proc ON ENDKEY UNDO Proc, LEAVE Proc:
        ASSIGN 
            DetFactura.CantDev = DetFactura.Cant.
                    
        IF DetFactura.Tipo <> 1 THEN 
            NEXT.
                    
        FIND ArtPres OF DetFactura NO-LOCK NO-ERROR.
                    
        IF Factura.Pedidos BEGINS "Prestamo-" AND DetFactura.RengPed <> 0 THEN 
        DO:
            ASSIGN 
                l-cant = DetFactura.Cant.
                        
            FIND FIRST DetSPrest WHERE DetSPrest.Id-SPrest = SUBSTRING(Factura.Pedidos,10,7) AND  
                DetSPrest.Id-Articulo = DetFactura.Id-Art   AND
                DetSPrest.Id-Color = DetFactura.Id-Color AND
                DetSPrest.Id-Pres = DetFactura.Id-Pres  AND
                DetSPrest.Sec = DetFactura.RengPed
                EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE DetSPrest THEN 
                ASSIGN 
                    DetSPrest.Id-Factura = ''
                    DetSPrest.Concil     = ?
                    DetSPrest.Id-User    = ''
                    DetSPrest.ConcilUMI  = DetSPrest.ConcilUMI - (DetFactura.Cant * (IF AVAILABLE ArtPres THEN ArtPres.Equiv ELSE 1)).
            RELEASE DetSPrest.
        END.
        ELSE 
        DO:
            IF (DetFactura.Id-Pedido = ""  OR
                DetFactura.Origen = 2   OR
                {salt0005.i} <> 'MATRIZ' ) AND l-genmov THEN 
            DO:
                /*
                /* Entrada al almacen */
                {inva0001.i
                    &Tipomov = "'EDev'"
                    &Refer = "DetFactura.Id-Factura"
                    &Cantidad = "(DetFactura.Cant * ArtPres.Equiv)"
                    &Almacen = "DetFactura.Id-Alm"
                    &llaveArt = "DetFactura.Id-Art"
                    &llaveColor = "DetFactura.Id-Color"
                    &IdPres = DetFactura.id-Pres
                    &CantPres = DetFactura.Cant
                    &MaxMin = TRUE
                    &Buffer = TRUE } */
                IF DetFactura.Id-Pedido <> '' THEN 
                DO:
                    ASSIGN 
                        l-cant = DetFactura.Cant.
                    FIND FIRST DetPedido WHERE DetPedido.Id-pedido = DetFactura.Id-Pedido AND
                        DetPedido.Resto = DetFactura.Resto AND
                        Detpedido.Id-articulo = DetFactura.id-art AND
                        DetPedido.Id-color = DetFactura.Id-color AND
                        DetPedido.Id-pres = DetFactura.Id-pres AND
                        DetPedido.Reng = DetFactura.RengPed
                        EXCLUSIVE-LOCK NO-ERROR.
                    IF AVAILABLE DetPedido THEN 
                    DO:
                        IF DetPedido.CantEnt >= l-cant THEN
                            ASSIGN 
                                DetPedido.CantEnt = DetPedido.CantEnt - l-cant
                                l-cant            = 0.
                        ELSE
                            ASSIGN 
                                l-cant            = l-cant - DetPedido.CantEnt
                                DetPedido.CantEnt = 0.
                    END. 
                END.                    
            END.
            IF DetFactura.Id-Pedido <> ""  THEN 
            DO:
                ASSIGN 
                    l-cant = DetFactura.Cant.
                            
                FIND FIRST DetPedido WHERE DetPedido.Id-pedido = DetFactura.Id-Pedido AND
                    DetPedido.Resto = DetFactura.Resto AND
                    Detpedido.Id-articulo = DetFactura.id-art AND
                    DetPedido.Id-color = DetFactura.Id-color AND
                    DetPedido.Id-pres = DetFactura.Id-pres AND
                    DetPedido.Reng = DetFactura.RengPed
                    EXCLUSIVE-LOCK NO-ERROR.
                            
                IF AVAILABLE DetPedido THEN 
                DO:
                    IF DetPedido.Cantfac >= l-cant THEN 
                        ASSIGN 
                            DetPedido.CantFac = DetPedido.CantFac - l-cant
                            l-cant            = 0.
                    ELSE
                        ASSIGN 
                            l-cant            = l-cant - DetPedido.CantFac
                            DetPedido.CantFac = 0.
                END.
            END.
        END.
    END. /* for each detfactura */

    /* Afecta pedidos */
    IF NOT AVAILABLE Devolucion OR (AVAILABLE Devolucion AND Devolucion.VtaCanc = FALSE) THEN 
    DO:
        RUN /usr2/adosa/procs/embd0020.p(INPUT 2, INPUT 'Factura', INPUT Factura.Id-Factura).
    END.
                    
END. /* if not factura.especial */
            
END.  /* Fin de Transaccion */
        
RELEASE Factura.
RELEASE DetFactura.        
RELEASE Folio.
RELEASE Acuse.
RELEASE MovCliente.
RELEASE DocAcuse.
RELEASE EstCte.
RELEASE EstVendedor.
RELEASE MovCaja.
RELEASE Devolucion.
RELEASE NCR.
RELEASE SUsoInt.
RELEASE DetSPrest.
RELEASE ArtUbic.
RELEASE Movim.
RELEASE Pedido.
RELEASE DetPedido.

/* Cancelar la factura electronica */
FIND Factura WHERE Factura.Id-Factura = l-factura NO-LOCK NO-ERROR.
IF AVAILABLE Factura AND Factura.FecCancel <> ? AND Factura.Folioe <> '' AND l-acuse = '' THEN 
DO:
    RUN /usr2/adosa/procs/vtac2062.p(INPUT l-factura).  /* SOLO FUNCIONA EN PRODUCCION */          
END.

STATUS DEFAULT ''.  
BELL.
ASSIGN
Respuesta = 'FACTURA CANCELADA.'.
PAUSE 2 NO-MESSAGE.
    
IF l-acuse <> '' THEN 
DO:
    MESSAGE "Acuse de Recibo No.: " l-acuse + '. Presione cualquier tecla para continuar...'.
    READKEY.
    RUN /usr2/adosa/procs/cxcc0610.p (INPUT l-acuse, INPUT '').
END.
END. // /* if l-tipo <> 3 */ 
STATUS DEFAULT .
END. // IF cp-answer   
  

END PROCEDURE.



