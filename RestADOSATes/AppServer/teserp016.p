@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*------------------------------------------------------------------------
    File        : teserp016.p
    Purpose     : Extraer txt
                  
    Author(s)   : sis10
    Created     : Fecha actual   
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW. /* Manejo de errores global */

/* **********************  Internal Procedures  *********************** */
DEF VAR l-opciones AS CHAR EXTENT 3 INITIAL ['Tickets','Contado','Credito'] NO-UNDO.
DEF VAR l-lopciones AS LOGICAL NO-UNDO.
DEF VAR l-iopciones AS INTEGER NO-UNDO.
DEF VAR l-tipo AS CHAR EXTENT 8 INITIAL ['Matriz','Chih','Salt','PLivas','RuizC','Cumbres','DDiaz','Cerradas'] NO-UNDO.
DEF VAR l-ltipo AS LOGICAL NO-UNDO.
DEF VAR l-itipo AS INTEGER NO-UNDO.
DEF STREAM s-salida.
DEF VAR l-fecha    AS DATE FORMAT "99/99/9999" NO-UNDO.


/* Variables adicionales para el proceso de importación */

DEFINE TEMP-TABLE ttBanco NO-UNDO
    FIELD Id      AS INTEGER           
    FIELD lineaTexto  AS CHARACTER 
    FIELD numLinea  AS INT 
    INDEX idx-mc  Id ASCENDING  .


/* ***************************  Main Procedure *************************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetExtraer:    
    DEFINE INPUT PARAMETER l-iopciones AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER l-itipo     AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER l-fecha    AS DATE    NO-UNDO.
    DEFINE OUTPUT PARAMETER Respuesta  AS CHAR. 
    DEFINE OUTPUT PARAMETER IdError    AS LOGICAL.
    DEFINE OUTPUT PARAMETER TABLE FOR ttBanco.
    
        /* Variables adicionales */
    DEFINE VARIABLE l-linea AS CHARACTER NO-UNDO.
    DEFINE VARIABLE l-archivo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE l-contador AS INTEGER NO-UNDO INITIAL 0.
    DEFINE VARIABLE l-lineas-a-leer AS INTEGER NO-UNDO INITIAL 10. /* Número de líneas a leer para la prueba */
    DEFINE VARIABLE l-contenido AS LONGCHAR NO-UNDO.
    
    /* Validación básica de parámetros obligatorios */
    IF l-fecha = ? THEN DO:
        ASSIGN
            Respuesta = "Error: Fecha es obligatoria"
            IdError   = TRUE.
        RETURN.
    END.

    IF l-fecha > TODAY THEN DO:
        ASSIGN
            Respuesta = "Error: La fecha no puede ser futura"
            IdError   = TRUE.
        RETURN.
    END.

    /* Validación de l-iopciones (1-3 siempre) */
    IF l-iopciones < 1 OR l-iopciones > 3 THEN DO:
        ASSIGN
            Respuesta = "Error: Tipo de Reporte No Correcto "
            IdError   = TRUE.
        RETURN.
    END.

    /* Validación CONDICIONAL de l-itipo */
    IF (l-iopciones = 1 OR l-iopciones = 2) THEN DO:
        IF l-itipo < 1 OR l-itipo > 8 THEN DO:
            ASSIGN
                Respuesta = "Error: Tipo de Tienda No correcta"
                IdError   = TRUE.
            RETURN.
        END.
    END.


    /* Si pasa todas las validaciones */
    ASSIGN
        Respuesta = "Parámetros válidos"
        IdError   = FALSE.
    
   
   STATUS DEFAULT "".
   ASSIGN l-archivo = "/usr3/adosa/diariovta/dv".
   
   IF l-iopciones = 1 THEN l-archivo = l-archivo + "t".
   ELSE IF l-iopciones = 2 THEN l-archivo = l-archivo + "c".
   ELSE IF l-iopciones = 3 THEN l-archivo = l-archivo + "f".
   
   IF l-iopciones < 3 THEN DO:
        CASE l-itipo:
            WHEN 1 THEN l-archivo = l-archivo + "0".
            WHEN 2 THEN l-archivo = l-archivo + "5".
            WHEN 3 THEN l-archivo = l-archivo + "4".
            WHEN 4 THEN l-archivo = l-archivo + "6".
            WHEN 5 THEN l-archivo = l-archivo + "7".
            WHEN 6 THEN l-archivo = l-archivo + "8".
            WHEN 7 THEN l-archivo = l-archivo + "9".
            WHEN 8 THEN l-archivo = l-archivo + "10".
        END CASE.
    END.
  
  MESSAGE " PROC " + STRING(l-archivo) VIEW-AS ALERT-BOX.    
   
   ASSIGN l-archivo = l-archivo +
                   STRING(YEAR(l-fecha),"9999") +
                   STRING(MONTH(l-fecha),"99") +
                   STRING(DAY(l-fecha),"99") + ".lst".
                   
     MESSAGE " PROC3 " + STRING(l-archivo) VIEW-AS ALERT-BOX.    
   
   /* PASO 1: Verificación de existencia del archivo */
    IF SEARCH(l-archivo) = ? THEN DO:
        ASSIGN
            Respuesta = "Archivo no encontradoee: " + l-archivo
            IdError   = TRUE.
        RETURN.
    END.
    
    /* Opción 1: Enviar el contenido completo como un solo texto */
   /* COPY-LOB FROM FILE l-archivo TO l-contenido.
    
    CREATE ttBanco.
    ASSIGN 
        ttBanco.Id = 1
        ttBanco.lineaTexto = l-contenido
        ttBanco.numLinea = 0. /* 0 indica contenido completo */  */
        
             
     /* PASO 2: Lectura de las primeras líneas para prueba */
    INPUT FROM VALUE(l-archivo).
    
    /* Mensaje inicial para ver la ruta completa */
    MESSAGE "Leyendo archivo:" l-archivo VIEW-AS ALERT-BOX.
    
    REPEAT:
        IMPORT UNFORMATTED l-linea.
        l-contador = l-contador + 1.
        
        /* Muestra cada línea en el log */
     //   MESSAGE "Línea" l-contador ":" l-linea VIEW-AS ALERT-BOX.
        
        /* Opcional: Guardar en ttBanco si necesitas */
        CREATE ttBanco.
        ASSIGN ttBanco.lineaTexto = l-linea
               ttBanco.numLinea = l-contador.
        
        /* Salir del bucle después de leer las líneas de prueba */
       /* IF l-contador >= l-lineas-a-leer THEN LEAVE. */
    END.
       
    INPUT CLOSE.       
    
    /* Resultado final */
    ASSIGN
        Respuesta = "Lectura de prueba completada. Líneas leídas: " + STRING(l-contador)
        IdError   = FALSE.
   

     
END PROCEDURE.
