@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*------------------------------------------------------------------------
    File        : teserp016.p
    Purpose     : Extraer archivos .lst y enviarlos como BLOB
                  
    Author(s)   : basado en tesc0160.p
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
DEF VAR l-fecha AS DATE FORMAT "99/99/9999" NO-UNDO.

/* Estructura de la tabla temporal con campo BLOB */
DEFINE TEMP-TABLE ttArchivo NO-UNDO
    FIELD lineaTexto AS BLOB . /* Cambiado a BLOB para manejar contenido binario */

/* ***************************  Main Procedure *************************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetExt:    
    DEFINE INPUT PARAMETER l-iopciones AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER l-itipo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER l-fecha AS DATE NO-UNDO.
    DEFINE OUTPUT PARAMETER Respuesta AS CHAR. 
    DEFINE OUTPUT PARAMETER IdError AS LOGICAL.
    DEFINE OUTPUT PARAMETER TABLE FOR ttArchivo.
    
    /* Variables para manejo de archivos */
    DEFINE VARIABLE l-archivo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE mData AS MEMPTR NO-UNDO.
    DEFINE VARIABLE l-tamanio AS INTEGER NO-UNDO.
    DEFINE VARIABLE l-contador AS INTEGER NO-UNDO INITIAL 0.
    
    /* 1. Validación de parámetros */
    IF l-fecha = ? THEN DO:
        ASSIGN Respuesta = "Error: Fecha es obligatoria"
               IdError = TRUE.
        RETURN.
    END.

    IF l-fecha > TODAY THEN DO:
        ASSIGN Respuesta = "Error: La fecha no puede ser futura"
               IdError = TRUE.
        RETURN.
    END.

    IF l-iopciones < 1 OR l-iopciones > 3 THEN DO:
        ASSIGN Respuesta = "Error: Tipo de Reporte No Correcto"
               IdError = TRUE.
        RETURN.
    END.

    IF (l-iopciones = 1 OR l-iopciones = 2) AND 
       (l-itipo < 1 OR l-itipo > 8) THEN DO:
        ASSIGN Respuesta = "Error: Tipo de Tienda No correcta"
                 IdError = TRUE.
        RETURN.
    END.
    
    /* 2. Generación de la ruta del archivo */
    ASSIGN l-archivo = "/usr3/adosa/diariovta/dv".
    
    /* Construcción del nombre según parámetros */
    CASE l-iopciones:
        WHEN 1 THEN l-archivo = l-archivo + "t".
        WHEN 2 THEN l-archivo = l-archivo + "c".
        WHEN 3 THEN l-archivo = l-archivo + "f".
    END CASE.
    
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
    
    /* Agregar fecha y extensión */
    ASSIGN l-archivo = l-archivo +
           STRING(YEAR(l-fecha),"9999") +
           STRING(MONTH(l-fecha),"99") +
           STRING(DAY(l-fecha),"99") + ".lst".
    
    /* 3. Verificación de existencia del archivo */
    IF SEARCH(l-archivo) = ? THEN DO:
        ASSIGN Respuesta = "Reporte sin existencia "  
               IdError = TRUE.
        RETURN.
    END.
    
    /* 4. Obtener información del archivo */
    FILE-INFO:FILE-NAME = l-archivo.
    l-tamanio = FILE-INFO:FILE-SIZE.
    
    /* 5. Leer archivo binario */
    SET-SIZE(mData) = 0. /* Limpiar memoria primero */
    SET-SIZE(mData) = l-tamanio.
    
    COPY-LOB FROM FILE l-archivo TO mData NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        ASSIGN Respuesta = "Error al leer archivo: " + ERROR-STATUS:GET-MESSAGE(1)
         IdError = TRUE
        SET-SIZE(mData) = 0.
        RETURN.
    END.
    
    /* 6. Crear registro con el contenido completo */
    CREATE ttArchivo.
  //  ASSIGN 

    
    /* Asignar el contenido binario al BLOB */
    COPY-LOB FROM mData TO ttArchivo.lineaTexto.
      
    /* 7. Resultado exitoso */
    ASSIGN
        Respuesta = "Archivo cargado correctamente. Tamaño: " + STRING(l-tamanio) + " bytes " + STRING(l-archivo)
        IdError = FALSE.
    
    /* 8. Limpieza de recursos */
    FINALLY:
        SET-SIZE(mData) = 0.
    END FINALLY.
END PROCEDURE.