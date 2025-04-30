/*
  Empresa  : Consultoria en Informatica Ejecutiva
  Programa : inva0001.i
  Funcion  : Crea Movimiento para articulo en una entrada y si no existe lo crea
  Autor    : MALP, EAEO
  Fecha    : 07-AGO-96
*/
/*  &Tipomov    = Tipo de movimiento que se trate. Preguntar a EAEO los tipos
    &Refer      = Numero de documento que genera la entrada
    &Cantidad   = Cantidad en UMI que entra al inventario
    &llaveArt   = Codigo del articulo
    &llaveColor = Codigo del color
    &Almacen    = Almacen para el cual se va a hacer el movimiento de entrada
                  Si este parametro no se define, se asume g-origen
    &MaxMin     = Indica si se toma en cuenta para estadisticas o no
    &IdPres     = Numero de Presentacion
    &CantPres   = Cantidad en la presentacion
    &PAlias     = Alias de La Base de Datos
*/

DO TRANSACTION:
    {ifndef {&buffer}}
    DEF BUFFER g-Folio FOR {&PAlias}Folio.  {endif} */
    FIND g-Folio WHERE g-Folio.id-doc = "Mov" AND
         g-Folio.Id-Alm = {ifndef {&Almacen}} g-origen {else} */ {&Almacen} {endif} */ EXCLUSIVE NO-ERROR.

    FIND {&PAlias}ArtUbic WHERE {&PAlias}ArtUbic.Id-Art   = {&llaveArt}   AND
                                {&PAlias}ArtUbic.Id-color = {&llaveColor} AND
                                {&PAlias}Artubic.Id-Alm   = {ifndef {&Almacen}} g-origen {else} */ {&Almacen} {endif} */
                          EXCLUSIVE NO-ERROR.
    IF NOT  AVAILABLE {&PAlias}ArtUbic THEN DO:
      RUN invd0200.p({ifndef {&Almacen}} g-origen {else} */ {&Almacen} {endif} */, {&llaveArt}, {&llaveColor}).
      CREATE {&PAlias}ArtUbic.
      ASSIGN {&PAlias}ArtUbic.id-art     = {&llaveArt}
             {&PAlias}ArtUbic.id-color   = {&llaveColor}
             {&PAlias}ArtUbic.id-alm     = {ifndef {&Almacen}} g-origen {else} */ {&Almacen} {endif} */
             {&PAlias}ArtUbic.pedira     = IF g-Origen = '02B' THEN "P" ELSE "A"
             {&PAlias}ArtUbic.Maximo     = 0
             {&PAlias}ArtUbic.Minimo     = 0
             {&PAlias}ArtUbic.Exist      = 0.
    END.
    CREATE {&PAlias}Movim.
    ASSIGN
      {&PAlias}Movim.folio    = g-Folio.Folio
      {&PAlias}Movim.Id-Art   = {&llaveArt}
      {&PAlias}Movim.Id-Alm   = {ifndef {&Almacen}} g-origen {else} */ {&Almacen} {endif} */
      {&PAlias}Movim.id-color = {&llaveColor}
      {&PAlias}Movim.Cant     = {&cantidad}
      {&PAlias}Movim.Tipo     = {&Tipomov}
      {&PAlias}Movim.Refer    = {&Refer}
      {&PAlias}Movim.Exist    = {&PAlias}ArtUbic.Exist + {&PAlias}ArtUbic.Compro + {&PAlias}Movim.Cant
      {&PAlias}Movim.FecReg   = g-TODAY
      {&PAlias}Movim.MaxMin   = {&MaxMin}
      {&PAlias}Movim.Id-Pres  = {&IdPres}
      {&PAlias}Movim.CantPres = {&CantPres}
      {&PAlias}ArtUbic.Exist  = {&PAlias}ArtUbic.Exist + {&PAlias}Movim.Cant
      g-Folio.Folio           = g-Folio.folio + 1.
END.
RELEASE {&PAlias}Movim.
RELEASE {&PAlias}ArtUbic.
RELEASE g-Folio.    
