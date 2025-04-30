/*
  Empresa  : ADOSA
  Modulo   : Ventas
  Programa : vtaa0005.i
  Funcion  : Rutina para afectar la estadistica de vendedores y empleados
  Autor    : FLC
  Fecha    : 12-SEP-1997
*/
/*
Parametros:
 &Vendedor  = Vendedor que hace el movimiento
 &Iniciales = Empleado que hace el movimiento
 &Fecha     = Fecha del movimiento a afectar
 &Importe   = Valor del movimiento (negativo para cancelaciones)
 &Tipo      = (1) Remision (2) Factura (3) Devolucion
*/

FIND EstVendedor WHERE
     EstVendedor.Id-Vendedor = {&Vendedor} AND EstVendedor.Tipo = 1 AND
     EstVendedor.Anio       = YEAR({&Fecha}) EXCLUSIVE-LOCK NO-ERROR.

IF NOT AVAILABLE (EstVendedor) THEN DO:
   CREATE EstVendedor.
   ASSIGN EstVendedor.Id-Vendedor = {&Vendedor}
	  EstVendedor.Anio        = YEAR({&Fecha})
	  EstVendedor.Tipo        = 1.
END.
IF {&Tipo} = 1 THEN DO:
   ASSIGN EstVendedor.VentasCo[MONTH({&fecha})] =
	     EstVendedor.VentasCo[MONTH({&fecha})] + {&Importe}
          EstVendedor.NumVenCo[MONTH({&fecha})] =
	     EstVendedor.NumVenCo[MONTH({&fecha})] + 1.
END.

IF {&Tipo} = 2 THEN DO:
   ASSIGN EstVendedor.VentasCr[MONTH({&fecha})] =
	     EstVendedor.VentasCr[MONTH({&fecha})] + {&Importe}
          EstVendedor.NumVenCr[MONTH({&fecha})] =
	     EstVendedor.NumVenCr[MONTH({&fecha})] + 1.
END.

IF {&Tipo} = 3 THEN DO:
   ASSIGN EstVendedor.ImpDevol[MONTH({&fecha})] =
	     EstVendedor.ImpDevol[MONTH({&fecha})] + {&Importe}
          EstVendedor.NumDev[MONTH({&fecha})] =
	     EstVendedor.NumDev[MONTH({&fecha})] + 1.
END.

FIND EstVendedor WHERE
     EstVendedor.Iniciales = {&Iniciales} AND EstVendedor.Tipo = 2 AND
     EstVendedor.Anio      = YEAR({&Fecha}) EXCLUSIVE-LOCK NO-ERROR.

IF NOT AVAILABLE (EstVendedor) THEN DO:
   CREATE EstVendedor.
   ASSIGN EstVendedor.Iniciales = {&Iniciales}
	  EstVendedor.Anio      = YEAR({&Fecha})
	  EstVendedor.Tipo      = 2.
END.
IF {&Tipo} = 1 THEN DO:
   ASSIGN EstVendedor.VentasCo[MONTH({&fecha})] =
	     EstVendedor.VentasCo[MONTH({&fecha})] + {&Importe}
          EstVendedor.NumVenCo[MONTH({&fecha})] =
	     EstVendedor.NumVenCo[MONTH({&fecha})] + 1.
END.

IF {&Tipo} = 2 THEN DO:
   ASSIGN EstVendedor.VentasCr[MONTH({&fecha})] =
	     EstVendedor.VentasCr[MONTH({&fecha})] + {&Importe}
          EstVendedor.NumVenCr[MONTH({&fecha})] =
	     EstVendedor.NumVenCr[MONTH({&fecha})] + 1.
END.

IF {&Tipo} = 3 THEN DO:
   ASSIGN EstVendedor.ImpDevol[MONTH({&fecha})] =
	     EstVendedor.ImpDevol[MONTH({&fecha})] + {&Importe}
          EstVendedor.NumDev[MONTH({&fecha})] =
	     EstVendedor.NumDev[MONTH({&fecha})] + 1.
END.
