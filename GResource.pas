unit GResource;

interface

resourcestring
{$DEFINE TG_ENGLISH}
{-$DEFINE TG_TAIWAN}
{-$DEFINE TG_SPANISH}

{$IFDEF TG_ENGLISH}
  rsStrToLLMsg = 'StrToLL: format error';
  rsEStrToUnitsMsg = 'StrToUnits: Unknown Unit %s';
  rsEFileNotFoundMsg = 'File %s not found';
  rsEDataFileVersionMsg = 'Invalid File Version %X in %s';
  rsEGlobeTextureMsg = 'Globe texture must be a 256 color RGB encoded bitmap';
  rsEConversionError = 'StrToExtended: Conversion failed for %s';
  rsEClassRegisteredMsg = 'TGlobe5.RegisterGlobeClass: Class %s already registered';
  rsEBadLayerIndexMsg = 'TGlobe5.GetLayer: Bad layer index %d';
  rsEBadProjectionClassMsg = 'Class %s does not inherit from TProjectionModel';
  rsEGlobeBadObjectMsg = 'TGlobeReader returned nil object';
  rsEGlobeBadMethodMsg = 'Method %s not defined for %s';

  rsMapping = 'Mapping';
  rsFinished = 'Finished';
  rsMIFImportError = 'MIF Import only supports Lat/Long tables.';
  rsNewObject = 'New Object';
  rsNewPolygon = 'New Polygon';
  rsNewPolyline = 'New Polyline';

  rsPointPresenterName = 'Point Presenter';
  rsPolyPresenterName = 'Poly Presenter';
  rsPolygonPresenterName = 'Polygon Presenter';
  rsPolylinePresenterName = 'Polyline Presenter';

  rsOpenError = 'Unable to open file %s';
  rsFileMappingCreateError = 'Unable to create file mapping';
  rsFileMappingViewError = 'Unable to map view of file';
{$ENDIF}

{$IFDEF TG_TAIWAN}
  rsStrToLLMsg = 'StrToLL: format error';
  rsEStrToUnitsMsg = 'StrToUnits: Unknown Unit %s';
  rsEGlobeTextureMsg = 'Globe texture must be a 256 color RGB encoded bitmap';
  rsEFileNotFoundMsg = '�ɮ� %s ���s�b';
  rsEDataFileVersionMsg = '��~���ɮת��� %X �� %s';
  rsEConversionError = 'StrToExtended: Conversion failed for %s';
  rsEClassRegisteredMsg = 'TGlobe5.RegisterGlobeClass: Class %s already registered';
  rsEBadLayerIndexMsg = 'TGlobe5.GetLayer: Bad layer index %d';
  rsEBadProjectionClassMsg = 'Class %s does not inherit from TProjectionModel';
  rsEGlobeBadObjectMsg = 'TGlobeReader returned nil object';
  rsEGlobeBadMethodMsg = 'Method %s not defined for %s';

  rsMapping = 'Mapping';
  rsFinished = 'Finished';
  rsMIFImportError = 'MIF Import only supports Lat/Long tables.';
  rsNewObject = 'New Object';
  rsNewPolygon = 'New Polygon';
  rsNewPolyline = 'New Polyline';

  rsPointPresenterName = 'Point Presenter';
  rsPolyPresenterName = 'Poly Presenter';
  rsPolygonPresenterName = 'Polygon Presenter';
  rsPolylinePresenterName = 'Polyline Presenter';

  rsOpenError = 'Unable to open file %s';
  rsFileMappingCreateError = 'Unable to create file mapping';
  rsFileMappingViewError = 'Unable to map view of file';
{$ENDIF}

{$IFDEF TG_SPANISH}
  rsStrToLLMsg = 'StrToLL: Error de formato';
  rsEStrToUnitsMsg = 'StrToUnits: Unidad ''%s'' desconocida';
  rsEFileNotFoundMsg = 'Archivo ''%s'' no encontrado';
  rsEDataFileVersionMsg = 'Versi�n de Archivo Inv�lida %X en ''%s''';
  rsEGlobeTextureMsg = 'La textura debe ser de un bitmap RGB de 256 colores';
  rsEConversionError = 'StrToExtended: Fallo de conversi�n para ''%s''';
  rsEClassRegisteredMsg = 'TGlobe5.RegisterGlobeClass: Clase ''%s'' ya registrada';
  rsEBadLayerIndexMsg = 'TGlobe5.GetLayer: �ndice de capa %d incorrecto';
  rsEBadProjectionClassMsg = 'La clase ''%s'' no hereda de TProjectionModel';
  rsEGlobeBadObjectMsg = 'TGlobeReader retorna objecto nulo';
  rsEGlobeBadMethodMsg = 'M�todo ''%s'' no definido para ''%s''';

  rsMapping = 'Mapeando';
  rsFinished = 'Finalizado';
  rsMIFImportError = 'Importaci�n de MIF s�lo soporta tablas de Lat/Long.';
  rsNewObject = 'Nuevo Objeto';
  rsNewPolygon = 'Nuevo Pol�gono';
  rsNewPolyline = 'Nueva Polil�nea';

  rsPointPresenterName = 'Presentador de Puntos';
  rsPolyPresenterName = 'Presentador de Poliobjetos';
  rsPolygonPresenterName = 'Presentador de Pol�gonos';
  rsPolylinePresenterName = 'Presentador de Polil�neas';

  rsOpenError = 'Imposible abrir %s';
  rsFileMappingCreateError = 'No se puede crear archivo de mapeado';
  rsFileMappingViewError = 'No se puede mapear la vista del archivo';
{$ENDIF}

implementation

end.


